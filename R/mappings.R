#' Full path of file in folder
#'
#' Returns the full file path of a file in a given directory, if it is located there.
#' Else the path is relative to the sGEOkb package inst-folder.
#'
#' This function is used to load configuration from a file in a directory that is
#' passed as a command line argument, or - if the file does not exist in that directory -
#' to load the default configuration file shipped with the sGEOkb package.
#' @param inputs_path The wanted directory.
#' @param file_name The name of the wanted file.
#'
#' @return A string representing the path of the file.
#'
.default_if_file_not_exists <- function(inputs_path, file_name) {
  wanted_path <- file.path(inputs_path, file_name)
  default_path <- file.path(system.file(package = "sGEOkb"), file_name)
  out <- ifelse(
    file.exists(wanted_path),
    wanted_path,
    default_path
  )
  if (!file.exists(out)) {
    flog.error("File %s does not exist", out)
    return(NULL)
  }
  flog.debug("Using file %s", out)
  return(out)
}


#' Load mapping files
#'
#' @param inputs_path String. Path to the directory containing the mapping files.
#' @return A list of mapping files
#'
load_mapping_files <- function(inputs_path) {

  mapping_files <- list()

  mapping_files$BuchT_Mapping_json <- jsonlite::read_json(
    .default_if_file_not_exists(inputs_path, "buchungstext_synonyms.json"),
    simplifyVector = T
  )
  mapping_files$GoogT_Mapping_json <- jsonlite::read_json(
    .default_if_file_not_exists(inputs_path, "googletypes_synonyms.json"),
    simplifyVector = T
  )
  mapping_files$BusiT_Mapping_json <- jsonlite::read_json(
    .default_if_file_not_exists(inputs_path, "businesstypes_synonyms.json"),
    simplifyVector = T
  )
  mapping_files$Price_Mapping_json <- jsonlite::read_json(
    .default_if_file_not_exists(inputs_path, "preissensitiviy_mapping.json"),
    simplifyVector = T
  )

  return(mapping_files)
}


#' Add features by mapping certain columns to business type and price type
#'
#' @param df Dataframe with transaction data.
#' @param BuchT_Mapping_json Mapping for booking categories on target categories.
#' @param GoogT_Mapping_json Mapping for google categories on target categories.
#' @param BusiT_Mapping_json Mapping for booking text strings  on target categories.
#' @param Price_Mapping_json Mapping for booking texts strings on price categories.
#' @param LOCATION_COLUMN Column storing booking text strings.
#' @param ALT_LOCATION_COLUMN Column storing alternative text strings.
#' @param BOOKING_COLUMN Column storing booking categories text strings.
#' @param GOOGLE_COLUMN Column storing google categories text strings.
#'
#' @return The data frame df with additional columns (target categories) 'businesstype' and 'pricestype'.
#'
apply_mappings <- function(df,
                           BuchT_Mapping_json,
                           GoogT_Mapping_json,
                           BusiT_Mapping_json,
                           Price_Mapping_json,
                           LOCATION_COLUMN,
                           ALT_LOCATION_COLUMN,
                           BOOKING_COLUMN,
                           GOOGLE_COLUMN) {

  # Clean mapping data.
  BuchT_Mapping <- data.frame(
    "cat" = as.character(gsub('[[:digit:]]+', '', names(unlist(BuchT_Mapping_json)))),
    "value" = as.character(unlist(BuchT_Mapping_json)), stringsAsFactors=F
  )
  GoogT_Mapping <- data.frame(
    "cat" = as.character(gsub('[[:digit:]]+', '', names(unlist(GoogT_Mapping_json)))),
    "value" = as.character(unlist(GoogT_Mapping_json)), stringsAsFactors=F
  )
  BusiT_Mapping <- data.frame(
    "cat" = as.character(gsub('[[:digit:]]+', '', names(unlist(BusiT_Mapping_json)))),
    "value" = as.character(unlist(BusiT_Mapping_json)), stringsAsFactors=F
  )
  Price_Mapping <- data.frame(
    "cat" = as.character(substr(names(unlist(Price_Mapping_json)), 1, 1)),
    "value" = as.character(unlist(Price_Mapping_json)), stringsAsFactors=F
  )

  # Initialize df with businesstype
  df_raw <- df
  df_raw$businesstype <- NA
  df_raw$pricestype <- NA

  # fill missing locations with cardtext
  df_raw[, LOCATION_COLUMN] <- ifelse(
    df_raw[, LOCATION_COLUMN] == "-",
    df_raw[, ALT_LOCATION_COLUMN],
    df_raw[, LOCATION_COLUMN]
  )

  # Businesstype filling by booking
  for (i in c(unique(BuchT_Mapping$cat))) {
    v <- dplyr::case_when(
      stringr::str_detect(
        tolower(df_raw[, BOOKING_COLUMN]),
        paste(c(i, BuchT_Mapping$value[BuchT_Mapping$cat == i]), collapse = "|")
      ) ~ i
    )
    df_raw$businesstype <- ifelse(is.na(df_raw$businesstype), v, df_raw$businesstype)
  }
  rm(v, i)

  # Businesstype filling by google information first keyword
  for (i in c(unique(GoogT_Mapping$cat))) {
    v <- dplyr::case_when(
      stringr::str_detect(
        stringr::str_split(tolower(df_raw[, GOOGLE_COLUMN]), ";", simplify=T)[, 1],
        paste(c(i, GoogT_Mapping$value[GoogT_Mapping$cat == i]), collapse = "|")
      ) ~ i
    )
    df_raw$businesstype <- ifelse(is.na(df_raw$businesstype), v, df_raw$businesstype)
  }
  rm(v, i)

  # Businesstype filling by location
  for (i in c(unique(BusiT_Mapping$cat))) {
    v <- dplyr::case_when(
      stringr::str_detect(
        tolower(df_raw[, LOCATION_COLUMN]),
        paste(c(BusiT_Mapping$value[BusiT_Mapping$cat == i]), collapse = "|")
      ) ~ i
    )
    df_raw$businesstype <- ifelse(is.na(df_raw$businesstype), v, df_raw$businesstype)
  }
  rm(v, i)

  # Businesstype filling by google information second keyword
  for (i in c(unique(GoogT_Mapping$cat))) {
    v <- dplyr::case_when(
      stringr::str_detect(
        stringr::str_split(tolower(df_raw[, GOOGLE_COLUMN]), ";", simplify = T)[, 2],
        paste(c(i, GoogT_Mapping$value[GoogT_Mapping$cat == i]), collapse = "|")
      ) ~ i
    )
    df_raw$businesstype <- ifelse(is.na(df_raw$businesstype), v, df_raw$businesstype)
  }
  rm(v, i)

  # Fill leftover businesstypes
  df_raw$businesstype[is.na(df_raw$businesstype)] <- "other"

  # Price sensitivity filling by location
  for (i in c(unique(Price_Mapping$cat))) {
    v <- dplyr::case_when(
      stringr::str_detect(
        tolower(df_raw[, LOCATION_COLUMN]),
        paste(c(Price_Mapping$value[Price_Mapping$cat == i]), collapse = "|")
      ) ~ i
    )
    df_raw$pricestype <- ifelse(is.na(df_raw$pricestype), v, df_raw$pricestype)
  }
  rm(v, i)

  return(df_raw)
}

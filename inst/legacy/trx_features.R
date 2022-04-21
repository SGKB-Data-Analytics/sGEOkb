# ________________________________________________________________

### 1. Settings ####
# ________________________________________________________________

path_lib      <- "P:/Org/058_Zentralsitz/S/Sd/Strategie/Data_Analytics/CASES/UTILS/R_LIBRARIES/library_4-1-0/"


wd   <- "P:/Org/058_Zentralsitz/S/Sd/Strategie/Data_Analytics/CASES/UTILS/GEODATEN/Trx_Analyse"
# setwd(wd)


### Libraries
library(data.table, lib.loc = path_lib)
library(timeDate, lib.loc = path_lib)
library(readxl, lib.loc = path_lib)
library(stringr, lib.loc = path_lib)
library(jsonlite, lib.loc = path_lib)
library(geojsonsf, lib.loc = path_lib)
library(classInt, lib.loc = path_lib)
library(units, lib.loc = path_lib)
library(sf, lib.loc = path_lib)
library(sp, lib.loc = path_lib)
library(rgdal, lib.loc = path_lib)
library(raster, lib.loc = path_lib)
# library(wk, lib.loc = path_lib)
library(s2, lib.loc = path_lib)
library(geodist, lib.loc = path_lib)
library(DBI, lib.loc = path_lib)
library(backports, lib.loc = path_lib)
library(vctrs, lib.loc = path_lib)
library(odbc, lib.loc = path_lib)
library(yaml, lib.loc = path_lib)
library(crayon, lib.loc = path_lib)
library(dplyr, lib.loc = path_lib)

## ODBC Connection
odbc_data <- yaml::read_yaml(base::file("conn_config.yml", 'r'))
db_selected <- odbc_data$kbda_lab
odbc_conn <-  DBI::dbConnect(odbc(),
                             Driver = db_selected$driver,
                             Server = db_selected$server,
                             Database = db_selected$database,
                             Trusted_Connection = db_selected$trusted_connection,
                             encoding = 'latin1')
rm(db_selected, odbc_data)

# Source helper scripts
source("R/feature_helper_functions.R")


# Set parameter
CUSTOMER_ID_COLUMN <- "F3200_Bp_ID"
TIME_COLUMN <- "F3207_Cardtrx_Zeit"
AMOUNT_COLUMN = "F3207_Nettobetrag_Kunde_CHF"
LOCATION_COLUMN <- "F3207_PIA_Bezugsort"
ALT_LOCATION_COLUMN <- "F3207_Cardtrx_Ort"
BOOKING_COLUMN <- "F3207_PIA_Buchungstext"
GOOGLE_COLUMN <- "google_types"
LAT_COLUMN <- "GeoDaten_lat"
LON_COLUMN <- "GeoDaten_lng"
Swiss_Buffer <- 50000


# Set paths
# DATA_PATH <- "test_data_ptc_TRX_MIT_KUNDE_20210525.xlsx"
DATA_PATH_DB <- 'kbda_lab.dbo.tmp_AKB_kunden_mitTRX'

INPUTS_PATH <- paste(getwd(), "inputs", sep = "/")
RASTERSTACK_PATH <- paste(getwd(), "inputs","rasterstack_ch", sep = "/")
OUT_PATH <- "geo_features.csv"
OUT_DB <- 'tmp_AKB_kunden_mitTRX_output'


# Set features
RASTERS <- c(
  "Demography_HouseholdsDensityKmDiff_500vs2000m",
  "Demography_HouseholdsDensityKm_2km",
  "Demography_HouseholdsSizeAverage_2km",
  "Demography_PopAgeShare00to05_2km",
  "Demography_PopAgeShare05to20_2km",
  "Demography_PopAgeShare20to40_2km",
  "Demography_PopAgeShare40to60_2km",
  "Demography_PopAgeShare60to80_2km",
  "Demography_PopAgeShare80to100_2km",
  "Demography_PopBornSwissGemeindeShare_2km",
  "Demography_PopBornSwissShare_2km",
  "Demography_PopDensityKmDiff_500vs2000m",
  "Demography_PopDensityKm_2km",
  "Demography_PopLocalPresenceSince00to05_2km",
  "Demography_PopLocalPresenceSinceBirth_2km",
  "Economy_DistanceHighway",
  "Economy_ImmoPriceBuyM2",
  "Economy_ImmoPriceRentM2",
  "Economy_POI_Trainstations_1km",
  "Economy_POI_Bars_1km",
  "Economy_POI_Delis_1km",
  "Economy_POI_Traffic_1km",
  "Economy_POI_Individualsports_1km",
  "Geography_PopHightDiff_500vs2000m",
  "Geography_Hight",
  "Landcover_Agriculture_cover_5km",
  "Landcover_Artificial_cover_5km",
  "Landcover_Sidewalk_cover_1km",
  "Landcover_Pedestrianzone_cover_1km",
  "Landcover_Streets_cover_1km",
  "Landcover_Trees_cover_1km",
  "Landcover_Water_cover_5km"
)


# ________________________________________________________________

### 2. Data Import ####
# ________________________________________________________________

# Load country polygons
Countries <- geojsonsf::geojson_sf(
  paste0(INPUTS_PATH,"/countries.geojson"))


# Load mapping files
BuchT_Mapping_json <- jsonlite::read_json(
  paste0(INPUTS_PATH,"/buchungstext_synonyms.json"),
  simplifyVector = T)
GoogT_Mapping_json <- jsonlite::read_json(
  paste0(INPUTS_PATH,"/googletypes_synonyms.json"),
  simplifyVector = T)
BusiT_Mapping_json <- jsonlite::read_json(
  paste0(INPUTS_PATH,"/businesstypes_synonyms.json"),
  simplifyVector = T)
Price_Mapping_json <- jsonlite::read_json(
  paste0(INPUTS_PATH,"/preissensitiviy_mapping.json"),
  simplifyVector = T, )


# Import data
# df <- data.frame(readxl::read_excel(DATA_PATH))
df <- DBI::dbGetQuery(conn = odbc_conn, statement = paste0("Select * from ", DATA_PATH_DB))

# Transform excel data to spatial format
df_sp <- df
df_sp[,LAT_COLUMN] <- as.numeric(df_sp[,LAT_COLUMN])
df_sp[,LON_COLUMN] <- as.numeric(df_sp[,LON_COLUMN])
df_sp <- df_sp[order(df_sp[,CUSTOMER_ID_COLUMN],
                     df_sp[,TIME_COLUMN]),]
df_sp <- df_sp[!is.na(df_sp[,LAT_COLUMN]) & !is.na(df_sp[,LON_COLUMN]),]

# Create spatial points
points_geom <- sf::st_cast(
  x  = sf::st_sfc(sf::st_multipoint(cbind(df_sp[,LON_COLUMN], df_sp[,LAT_COLUMN]))),
  to = "POINT")

# Put data together in spatial format
df_sp <- sf::st_sf(
  id = df_sp[,CUSTOMER_ID_COLUMN],
  geometry = points_geom,
  crs = 4326)

# Clear memory
rm(points_geom)

# Set up data structure
features <- data.frame()

# All variables until here are the Stage.
STAGE <- ls()

# ________________________________________________________________

### 3. Time Features ####
# ________________________________________________________________



# Assign time features
df_features <- df[CUSTOMER_ID_COLUMN]
# df_features$date <- format(df[,TIME_COLUMN], format="%Y-%m-%d")
df_features$date <- as.POSIXct(df[,TIME_COLUMN])
# df_features$hour <- as.integer(format(df[,TIME_COLUMN], format="%H"))
df_features$hour <- as.integer(format(df_features$date, format="%H"))
# df_features$today_freeday <- evaluate_today_freeday(df[,TIME_COLUMN])
df_features$today_freeday <- evaluate_today_freeday(df_features$date)
# df_features$tomorrow_freeday <- evaluate_tomorrow_freeday(df[,TIME_COLUMN])
df_features$tomorrow_freeday <- evaluate_tomorrow_freeday(df_features$date)
df_features$time_of_day <-as.character(cut(df_features$hour,
                 breaks=c(-Inf,     4,             9,          12,         18,        22,  Inf),
                 labels=c("night","early_morning","early_day","afternoon","evening", "night" )))
df_features$day_or_night <-as.character(cut(df_features$hour,
                                            breaks=c(-Inf,     4,   22,  Inf),
                                            labels=c("night","day","night" )))
df_features$nighttimes <-ifelse(df_features$day_or_night=="night", df_features$hour, NA)
df_features$daytimes <-ifelse(df_features$day_or_night=="day", df_features$hour, NA)


#
customer_ids <- unique(df[CUSTOMER_ID_COLUMN])[,1]
for(i in c(customer_ids)){
  customer_df <- df_features[df_features[,CUSTOMER_ID_COLUMN] == i,]
  customer_features <- get_features(customer_df, i)
  features <- rbind(features, customer_features)
}

rm(list=setdiff(ls(), c(STAGE, "STAGE")))
gc()


# ______________________________________________________________________

# 4. Raster Features ####
# ______________________________________________________________________


# Add feature values
coords_spdf <- get_rasterstack_features(df_sp)

# Double check rasters existent in data
RASTER_FILTERED <- RASTERS[RASTERS %in% colnames(coords_spdf)]

# Extract aggregated value for each feature (median)
geo_results <- data.table::data.table(
  coords_spdf)[,lapply(.SD, median, na.rm=T),
               .SDcols = RASTER_FILTERED, by = id][
                 , data.table::setnames(
                   .SD, RASTER_FILTERED,
                   paste0("geo_", tolower(RASTER_FILTERED)))]

# Extract aggregated value for each feature (sd)
geo_results_sd <- data.table::data.table(
  coords_spdf)[,lapply(.SD, sd, na.rm=T),
               .SDcols = RASTER_FILTERED, by = id][
                 , data.table::setnames(
                   .SD, RASTER_FILTERED,
                   paste0("geo_", tolower(RASTER_FILTERED),"_sd"))]

geo_results<-cbind(geo_results, geo_results_sd[,-1])
geo_results<-data.frame(geo_results)[ , order(names(geo_results))]

# Adjust Hoehe if existent
if("Geography_Hight" %in% RASTER_FILTERED){

  # Assign max hoehe
  geo_results$geo_geography_hight <- data.table::data.table(
    coords_spdf)[,lapply(.SD, max, na.rm=T),
                 .SDcols = "Geography_Hight",
                 by = id]$Geography_Hight

}

# Combine features
features <- merge(
  x = features,
  y = geo_results,
  all.x = T,
  all.y = F,
  by = "id")

# Clean memory
rm(list = setdiff(ls(), c(STAGE, "STAGE")))
gc()


# ______________________________________________________________________

# 5. Ausgaben Features ####
# ______________________________________________________________________

# Clean mapping data
BuchT_Mapping <- data.frame(
  "cat"=as.character(gsub('[[:digit:]]+', '', names(unlist(BuchT_Mapping_json)))),
  "value"=as.character(unlist(BuchT_Mapping_json)))
GoogT_Mapping <- data.frame(
  "cat"=as.character(gsub('[[:digit:]]+', '', names(unlist(GoogT_Mapping_json)))),
  "value"=as.character(unlist(GoogT_Mapping_json)))
BusiT_Mapping <- data.frame(
  "cat"=as.character(gsub('[[:digit:]]+', '', names(unlist(BusiT_Mapping_json)))),
  "value"=as.character(unlist(BusiT_Mapping_json)))
Price_Mapping <- data.frame(
  "cat"=as.character(substr(names(unlist(Price_Mapping_json)), 1, 1)),
  "value"=as.character(unlist(Price_Mapping_json)))


# Initialize df with businesstype
df_raw <- df
df_raw$businesstype <- NA
df_raw$pricestype <- NA

# fill missing locations with cardtext
df_raw[,LOCATION_COLUMN]<-ifelse(df_raw[,LOCATION_COLUMN]=="-",df_raw[,ALT_LOCATION_COLUMN],df_raw[,LOCATION_COLUMN])



# Businesstype filling by booking
for (i in c(unique(BuchT_Mapping$cat))){
  v<-dplyr::case_when(stringr::str_detect(tolower(df_raw[,BOOKING_COLUMN]), paste(c(i,BuchT_Mapping$value[BuchT_Mapping$cat==i]), collapse = "|")) ~ i)
  df_raw$businesstype <- ifelse(is.na(df_raw$businesstype), v, df_raw$businesstype)
}; rm(v, i)


# Businesstype filling by google information first keyword

for (i in c(unique(GoogT_Mapping$cat))){
v<-dplyr::case_when(stringr::str_detect(stringr::str_split(tolower(df_raw[,GOOGLE_COLUMN]),";", simplify=T)[,1], paste(c(i,GoogT_Mapping$value[GoogT_Mapping$cat==i]), collapse = "|")) ~ i)
df_raw$businesstype <- ifelse(is.na(df_raw$businesstype), v, df_raw$businesstype)
}; rm(v, i)


# Businesstype filling by location
for (i in c(unique(BusiT_Mapping$cat))){
v<-dplyr::case_when(stringr::str_detect(tolower(df_raw[,LOCATION_COLUMN]), paste(c(BusiT_Mapping$value[BusiT_Mapping$cat==i]), collapse = "|")) ~ i)
df_raw$businesstype <- ifelse(is.na(df_raw$businesstype), v, df_raw$businesstype)
}; rm(v, i)

# Businesstype filling by google information second keyword
for (i in c(unique(GoogT_Mapping$cat))){
  v<-dplyr::case_when(stringr::str_detect(stringr::str_split(tolower(df_raw[,GOOGLE_COLUMN]),";", simplify=T)[,2], paste(c(i,GoogT_Mapping$value[GoogT_Mapping$cat==i]), collapse = "|")) ~ i)
  df_raw$businesstype <- ifelse(is.na(df_raw$businesstype), v, df_raw$businesstype)
}; rm(v, i)


# Fill leftover businesstypes
df_raw$businesstype[is.na(df_raw$businesstype)] <- "other"

# Price sensitivity filling by location
for (i in c(unique(Price_Mapping$cat))){
  v<-dplyr::case_when(stringr::str_detect(tolower(df_raw[,LOCATION_COLUMN]), paste(c(Price_Mapping$value[Price_Mapping$cat==i]), collapse = "|")) ~ i)
  df_raw$pricestype <- ifelse(is.na(df_raw$pricestype), v, df_raw$pricestype)
}; rm(v, i)




### Aggregate results
spending_results <- data.table::data.table(df_raw)[, .(
  # colnames(df_raw)

  # Activities
  total_activity = .N,

  # Activities by type
  total_activity_tagged= sum(
    base::get(GOOGLE_COLUMN) != "NULL" &
      !is.null(base::get(GOOGLE_COLUMN)) &
      !is.na(base::get(GOOGLE_COLUMN))),


  # Aggregated activities
  total_activity_median = median(
    as.numeric(base::get(AMOUNT_COLUMN)), na.rm=T),

  price_sensitivity = mean(
    as.numeric(pricestype), na.rm=T),

price_sensitivity_sd = sd(
  as.numeric(pricestype), na.rm=T)
),

  # Group by
  by = CUSTOMER_ID_COLUMN]

# Initialize businesstypes
businesstypes <- unique(df_raw$businesstype)

# Create features based on businesstype
for (bt in c(businesstypes)){

  df_bt_sub<- df_raw[df_raw$businesstype==bt,]

  # Grociers/Restaurants/Gas
  if(bt=="groceries" | bt=="restaurant" | bt=="gas" | bt=="shopping"){

    # Naming
    name_count <- paste0("spend_", bt, "_count")
    name_pct <- paste0("spend_", bt, "_pct")
    name_q3 <- paste0("spend_", bt, "_q66minusq33")
    name_q6 <- paste0("spend_" ,bt, "_q66")

    # Aggregate by quantiles
    dt_bt_sub <- data.table::data.table(df_bt_sub)[, .(
      xcount = .N,
      xpct = .N,
      q3 = quantile(as.numeric(base::get(AMOUNT_COLUMN)),probs = 1/3, names = F, na.rm=T)-
           quantile(as.numeric(base::get(AMOUNT_COLUMN)),probs = 2/3, names = F, na.rm=T),
      q6 = quantile(as.numeric(base::get(AMOUNT_COLUMN)),probs = 1/3, names = F, na.rm=T)),
      by = CUSTOMER_ID_COLUMN]

    # Assign names
    data.table::setnames(
      dt_bt_sub,
      c(CUSTOMER_ID_COLUMN, name_count, name_pct, name_q3, name_q6))

    # Combine data
    spending_results <- dt_bt_sub[spending_results, on = CUSTOMER_ID_COLUMN]
    spending_results[, name_pct] <- spending_results[, base::get(name_pct)] / spending_results[, total_activity]
    rm(name_count, name_pct, name_q3,name_q6, dt_bt_sub)


  }else{
  # Other businesstypes

    # Naming
    name_count <- paste0("spend_",bt,"_count")
    name_pct <- paste0("spend_",bt,"_pct")
    name_median <- paste0("spend_",bt,"_median")

    # Aggregate
    dt_bt_sub <- data.table::data.table(df_bt_sub)[, .(
      xcount = .N,
      xpct = .N,
      xmedian = median(as.numeric(base::get(AMOUNT_COLUMN)), na.rm=T)),
      by = CUSTOMER_ID_COLUMN]

    # Assign names
    data.table::setnames(
      dt_bt_sub,
      c(CUSTOMER_ID_COLUMN, name_count, name_pct, name_median))

    # Combine data
    spending_results <- dt_bt_sub[spending_results, on = CUSTOMER_ID_COLUMN]
    spending_results[, name_pct] <- spending_results[, base::get(name_pct)]/spending_results[, total_activity]
    rm(name_count, name_pct, name_median, dt_bt_sub)
  }
}

spending_results$F3200_Bp_ID[is.na(spending_results$price_sensitivity)]

# Fill NAs with 0 for count and percent
fnames <- names(spending_results)
for (fn in c(fnames)){
if(grepl("count", fn)|grepl("_pct", fn)){
  spending_results[,fn] <- data.table::nafill(spending_results[, base::get(fn)], fill = 0)}}



# Add  features to time & raster features
features <- merge(
  x = features,
  y = spending_results,
  all.x = T,
  all.y = F,
  by.x = "id",
  by.y = CUSTOMER_ID_COLUMN)


# collect internet transaction points to exclude from mobility featrues
df_sp_i <- df_raw[df_raw$businesstype=="internet",]
df_sp_i[,LAT_COLUMN] <- as.numeric(df_sp_i[,LAT_COLUMN])
df_sp_i[,LON_COLUMN] <- as.numeric(df_sp_i[,LON_COLUMN])
df_sp_i <- df_sp_i[!is.na(df_sp_i[,LAT_COLUMN]) & !is.na(df_sp_i[,LON_COLUMN]),]

# Create spatial points
points_geomi <- sf::st_cast(
  x  = sf::st_sfc(sf::st_multipoint(cbind(df_sp_i[,LON_COLUMN], df_sp_i[,LAT_COLUMN]))),
  to = "POINT")

# Put data together in spatial format
internetpoints <- sf::st_sf(
  geometry = points_geomi,
  crs = 4326)


# Clean memory
rm(list = setdiff(ls(), c(STAGE, "STAGE", "internetpoints")))
gc()



# ______________________________________________________________________

# 6. Mobility Features ####
# ______________________________________________________________________

# Extract CH from all country polygons
gdf <- Countries
gdf$ADMIN <- NULL
gdf_3035 <- sf::st_transform(gdf, crs = 3035)
chdf <- gdf[gdf$ISO_A3 == "CHE",]
chdf_3035 <- gdf_3035[gdf_3035$ISO_A3 == "CHE",]
chdf_3035_buffer<- sf::st_difference(sf::st_buffer(chdf_3035, Swiss_Buffer), chdf_3035)
chdf_3035_buffer$ISO_CHbuffer <- 1
chdf_3035_buffer$ISO_A3 <- NULL
chdf_3035_buffer$ISO_A3.1 <- NULL
chdf_buffer <- sf::st_transform(chdf_3035_buffer, crs = sf::st_crs(gdf))

# Geocode spatial points from raw data without internet points
sf::sf_use_s2(FALSE) # The change in sf v1.0 was move from backend engine for
# unprojected coordinates (the geographic ones, i.e. lat - long as in EPSG 4326) from GEOS to s2 from Google
sf_cardpoints <- sf::st_difference(df_sp, sf::st_combine(internetpoints))

sf_cardmultipoints <- aggregate(
  sf_cardpoints,
  by = list(sf_cardpoints$id), FUN = mean)
sf_cardmultipoints$Group.1 <- NULL
sf_cardpoints <- sf::st_join(
  sf_cardpoints, gdf,
  join = sf::st_intersects)
sf_cardpoints <- sf::st_join(
  sf_cardpoints, chdf_buffer,
  join = sf::st_intersects)
sf_cardpoints_3035 <- sf::st_transform(sf_cardpoints, crs = 3035)
sf_cardmultipoints_3035 <- sf::st_transform(sf_cardmultipoints, crs = 3035)
sf_cardpoints_centroids<- sf::st_as_sf(merge((data.frame("id"=sf_cardpoints$id)),
                                             sf::st_cast((sf::st_centroid(sf_cardmultipoints)),
                                                         to="POINT")[1], all.y=F, all.x=T, by="id"))

# Calculate feature:
# Distance to previous points
sf_cardpoints$distance_to_previous<-geodist::geodist(sf::st_coordinates(sf_cardpoints), sequential = T, pad=T)
sf_cardpoints$distance_to_previous <- ifelse(
  sf_cardpoints$id != c("first", sf_cardpoints[1:(nrow(sf_cardpoints)-1),]$id),
                                             NA, sf_cardpoints$distance_to_previous)


# Calculate feature:
# Distance to centroids
sf_cardpoints$distance_to_centroid<-geodist::geodist(sf::st_coordinates(sf_cardpoints),
                                                     sf::st_coordinates(sf_cardpoints_centroids),paired = T)


# Calculate cardpoints in spatial format
sf_cardmultipoints_3035ch <- sf::st_intersection(
  sf_cardmultipoints_3035, sf::st_buffer(chdf_3035, Swiss_Buffer))
sf_cardmultipoints_3035ch$mob_pct_of_che <- as.numeric(sf::st_area(
  sf::st_convex_hull(sf_cardmultipoints_3035ch)) / sf::st_area(chdf_3035))
sf_cardmultipoints_3035ch <- sf_cardmultipoints_3035ch[,c("id","mob_pct_of_che" )]
sf::st_geometry(sf_cardmultipoints_3035ch) <- NULL
sf_cardpoints <- merge(
  sf_cardpoints,
  sf_cardmultipoints_3035ch,
  all.x = T,
  all.y = F,
  by = "id")

# Calculate area
sf_cardmultipoints_3035$area <- as.numeric(sf::st_area(
  sf::st_convex_hull(sf_cardmultipoints_3035)))
sf_cardmultipoints_3035 <- sf_cardmultipoints_3035[,c("id","area" )]
sf::st_geometry(sf_cardmultipoints_3035) <- NULL
sf_cardpoints <- merge(
  sf_cardpoints,
  sf_cardmultipoints_3035,
  all.x = T,
  all.y = F,
  by = "id")

# Calculate points outside of CH
sf_cardpoints$nonCH <- ifelse(sf_cardpoints$ISO_A3 == "CHE", 0, 1)
sf_cardpoints$ISO_CHbuffer <- ifelse(is.na(sf_cardpoints$ISO_CHbuffer), 0, 1)

# Aggregate data for non-CH states
ForeignCountryStats<-data.table::data.table(
  sf_cardpoints[sf_cardpoints$nonCH==1,])[ , .N, by=.(id, ISO_A3)][order(-N), .(
    Favorit_Country = first(ISO_A3),
    Favorit_Country_N = max(N),
    Country_N = .N),
    by = id]

# Add foreign information to data
sf_cardpoints <- merge(
  sf_cardpoints,
  ForeignCountryStats,
  all.x = T,
  all.y = F,
  by = "id")


# Combine all mobility features
mobility_results<-data.table::data.table(sf_cardpoints)[, .(
  mob_mean_distance_to_previous = median(distance_to_previous, na.rm = T),
  mob_std_distance_to_previous  = sd(distance_to_previous, na.rm = T),
  mob_mean_distance_to_center   = median(distance_to_centroid, na.rm = T),
  mob_std_distance_to_center    = sd(distance_to_centroid, na.rm = T),
  mob_hull_circle_ratio         = max(distance_to_centroid, na.rm = T) * 2 * pi / max(area),
  mob_hullarea_per_t            = max(area) / .N,
  mob_pct_of_che                = mean(mob_pct_of_che, na.rm = T),
  mob_farfrom_switzerland       = mean(nonCH, na.rm = T) - mean(ISO_CHbuffer, na.rm = T),
  mob_closeto_switzerland       = mean(ISO_CHbuffer, na.rm = T),
  mob_country_count_different   = (Country_N[1]),
  mob_country_most_visited      = (Favorit_Country[1]),
  mob_country_most_visited_pct  = mean(Favorit_Country_N) / .N),
  by = id]

# Add mobility features to previous features
features<- merge(
  x = features,
  y = mobility_results,
  all.x = T,
  all.y = F,
  by = "id")

features[features==Inf] <-NA
features[features==-Inf] <-NA

# Clean memory
rm(list = setdiff(ls(), c("features","OUT_PATH", "odbc_conn", "OUT_DB")))
gc()


# ______________________________________________________________________

# 7. Output ####
# ______________________________________________________________________

write.csv2(features, OUT_PATH)


# ______________________________________________________________________

# 8. Output - Write to DB ####
# ______________________________________________________________________

colnames(features)
features.out <- features

for (f in 1: dim(features.out)[2]){
  features.out[is.nan(features.out[[f]]),f] <- NA
}

DBI::dbWriteTable(conn = odbc_conn, DBI::Id(schema = 'dbo', table = OUT_DB),
                  features.out, overwrite = T, row.names = F, fileEncoding = "latin1")

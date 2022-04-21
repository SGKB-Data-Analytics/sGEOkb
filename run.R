#! /usr/local/bin/Rscript

# Parse and validate command line arguments
args <- commandArgs(trailingOnly = TRUE)
args_parsed <- list()

if (length(args) > 0) {

  if ("--path-lib" %in% args) {
    index <- which(args == "--path-lib")
    tryCatch({
      args_parsed$path_lib <- args[index + 1]
      message("Using library ", args_parsed$path_lib)
    }, error = function(e) {
      stop("Failed to set library to path-lib")
    })
  } else {
    args_parsed$path_lib <- .libPaths()[1]
    message("Using library ", args_parsed$path_lib)
  }

  if ("--parameters-file" %in% args) {
    index <- which(args == "--parameters-file")
    tryCatch({
      args_parsed$parameters_file <- args[index + 1]
      message("Using parameters file ", args_parsed$parameters_file)
    }, error = function(e) {
      stop("Failed to set parameters file to parameters-file")
    })
  }

  if ("--connection-config" %in% args) {
    index <- which(args == "--connection-config")
    tryCatch({
      args_parsed$connection_config <- args[index + 1]
      message("Using connection config file ", args_parsed$connection_config)
    }, error = function(e) {
      stop("Failed to set connection_config to --connection-config")
    })
  }
}

# Load dependencies from specified library
if ("path_lib" %in% names(args_parsed)) {
  .libPaths(args_parsed$path_lib)
}

suppressPackageStartupMessages(
  suppressWarnings(
    if (!require(sGEOkb)) {
      stop(paste0("Did not find package sGEOkb in libary", .libPaths()[1]))
    }
  )
)

# Set logging for sGEOkb
# layout <- layout.format('[~l][~t][~n.~f] ~m')
# flog.layout(layout)
dev_mode <- FALSE
flog.threshold(INFO)
if ("--dev-mode" %in% args) {
  dev_mode <- TRUE
  message("Using dev-mode")
}

if ("--verbose" %in% args) {
  flog.threshold(DEBUG)
}

# Run main function
result <- suppressMessages(suppressWarnings(sGEOkb::main(args_parsed, dev_mode)))
if (!result) {
  stop("Aborting main function with error")
}

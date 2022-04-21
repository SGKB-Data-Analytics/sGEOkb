
# sGEOkb

The goal of sGEOkb is to enrich transactional data from credit cards with rich features from geographic and non-geographic information. This package
provides a main-function and a number of specialised sub-functions to:

- load parameters and configuration
- load input data
- compute features
- aggregate on card-id level
- save the feature-enriched data

## Deployment bundle

The sGEOkb package can be delivered as a source bundle (.tar.gz) or a binary package (.zip on Windows). Additionally the following files are required to use it:

- A geojson-file containing **country data**. If you have been provided with this package without a file named "countries.geojson", please ask for it.
- A folder containing **raster files** (.tiff) to be used. If you have been provided with this package without TIFF files in a folder named "rasterstack_ch", please ask for it.
- A script that provides a **command line interface** to the package. We provide a script called run.R for this purpose.
- This **README.md** file.

## Building the package

### From the command line or Bash

Navigate to the root directory of the package. Build the package with standard R-tooling:

``` bash
# Build documentation (man-files)
Rscript -e "devtools::document()"

# Build a source bundle (.tar.gz)
R CMD build .

# Build a binary package (.zip on Windows)
R CMD INSTALL --build SGKB_<version>.tar.gz
```

### From within the R console or RStudio

You can build the package using the devtools-package:

``` r
library(devtools)

document()
build(binary = T)
```

## Installation

You can install this package by running

``` r
install.packages("local/path/to/sGEOkb.zip", repos = NULL, lib = "path/to/library")
```

## Configuration

The package can be configured in the following ways:

- **Dev-mode**: You can run the main function in dev mode, which imlies the use of a test data base. This is meant for development purposes and may not work
on a staging or production environment. To switch on dev-mode, pass the
`--dev-mode` flag on the commandline.
- **Logging**: Using the package futile.logger to configure logging behaviour. To enable verbose logging from the command line, pass the `--verbose` flag.
- Providing the **path to the library** from which to load packages. On the commandline,
pass the path to the library with the `--path-lib` option.
- Providing a yaml file with **custom parameter settings**. On the commandline, pass the
path to the parameters file with the `--parameters-file` option.
- Provifing a yaml with **connection information** to the data base. The default value is 'conn_config.yml'. Pass the `--connection-config` option on the commandline to set a different file path. Should be in the yaml format. This option is ignored in dev-mode.
- Providing and selecting **raster files**: The raster files to be used are indicated in the
parameters file (parameter "RASTERS"). The directory containing the raster files is provided by the parameter "RASTERSTACK_PATH". 
- Providing **custom mappings**: The package's default mappings are located in the packages inst-folder. The following files are currently supported: *buchungstext_synonyms.json*, *googletypes_synonyms.json*, *businesstypes_synonyms.json*, *preissensitiviy_mapping.json*. You can override these default mappings with mappings of your own by supplying files of the same name in the directory INPUTS_PATH (to be specified in parameters file).

## Dev-mode

Running the script in dev-mode bypasses any database and uses test data from an xlsx-file. It can be activated by passing the `--dev-mode` flag in the command line or by setting `dev_mode = TRUE` in the main function.

Results are written to db-files (SQLite database files) at `tests/test_db_<timestamp>`. They are furthermore written to a csv-file whose filename is indicated by the 'OUT_PATH' element in the parameters file.

The following dependencies are expected in dev-mode:

- The parameters file needs to contain the element 'DATA_PATH_DB'
- A spreadsheet containing test data by the name of 'tests/20211020_test_data_ptc_fakeData.xlsx'

## Example

### Use from the command line or Bash

``` bash
Rscript run.R --path-lib <directory-path> --parameters-file <file-path> --connection-config <connection-config-path> --dev-mode --verbose
```

### Use from within the R console or RStudio

``` r
# Load package sGEOkb and all dependencies from <path-to-library>
library(sGEOkb, lib.loc = "<path-to-library>")

# Set dev_mode = T if you want to use a test database
dev_mode = T

# Set log level to DEBUG for detailed logs
flog.threshold(DEBUG)

# args is a named list of arguments (key-value pairs).
args <- list()
args$path_lib <- "<path-to-library>" # OPTIONAL: Indicates the library from which to load sGEOkb
args$parameters_file <- "<path-to-parameters-file>" # OPTIONAL: Indicates parameters file to use.
args$connection_config <- "<path-to-connection-config-file>" # OPTIONAL: Indicates the yaml file containing data base connection information
sGEOkb::main(args, dev_mode)
```


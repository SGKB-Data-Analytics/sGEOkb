% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odbc.R
\name{odbc_connect}
\alias{odbc_connect}
\title{Connect to ODBC data base}
\usage{
odbc_connect(DATA_PATH_DB = NULL, dev_mode = F,
  connection_config = NULL)
}
\arguments{
\item{dev_mode}{Boolean. If TRUE, connect to a local test DB.}
}
\value{
A connection object to be used by DBI::dbConnect().
}
\description{
Connect to ODBC data base
}

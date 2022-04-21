.onAttach <- function(libName, pakgname) {
  packageStartupMessage(
    "You are using sGEOkb version ",
    packageVersion("sGEOkb")
  )
}


pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

cpact <- function(l) {
  Filter(Negate(is.null), l)
}

make_ua <- function() {
  versions <- c(curl = curl::curl_version()$version,
                curl = as.character(packageVersion("curl")),
                httr = as.character(packageVersion("httr")),
                request = as.character(packageVersion("webdisr")))
  paste0(names(versions), "/", versions, collapse = " ")
}

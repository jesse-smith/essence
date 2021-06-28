#' \code{\link[httr:GET]{GET()}} Shortcut for ESSENCE Queries
#'
#' @param query `string`. Query portion of the ESSENCE URL;
#'   appended to \code{\link{es_uri}}
#'
#' @param creds `secret`. ESSENCE login credentials.
#'
#' @return An {httr} `response`
#'
#' @keywords internal
es_GET <- function(query, creds = es_creds_get()) {
  httr::RETRY(
    "GET",
    url = paste0(es_uri, query),
    auth(creds = creds)
  )
}

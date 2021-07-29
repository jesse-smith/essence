#' Get Possible Values for an ESSENCE Field
#'
#' @inheritParams es_fields
#'
#' @param field `character`. A field id within the given `datasource` (see
#'   \code{\link[essence:es_fields]{es_fields()}} for choices)
#'
#' @return A `tibble` with columns `display` and `value` (both `chr`)
#'
#' @export
es_values <- function(field, datasource, creds = es_creds_get(), check = TRUE) {
  assert_bool(check)
  if (check) assert_field(field, datasource, creds = creds)
  es_GET(paste0("datasources/", datasource, "/fields/", field)) %>%
    abort_status(paste0("retrieve fields for '", field, "'")) %>%
    httr::content(as = "parsed") %>%
    purrr::pluck("values") %>%
    purrr::transpose() %>%
    purrr::map_dfc(~ purrr::map_chr(.x, ~ if(is.null(.x)) "" else .x))
}

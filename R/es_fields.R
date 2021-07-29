#' Get Field Information for an ESSENCE Data Source
#'
#' @param datasource `[character(1)]`. A datasource name
#'   (see \code{\link[essence:es_datasources]{es_datasources()}} for choices)
#'
#' @param check `logical`. Should arguments be checked before querying
#'   ESSENCE?
#'
#' @inheritParams es_datasources
#'
#' @return A `tibble` with columns `parentParamName`, `description`,
#'   `dataBeanId`, `paramName`, `id`, `label`, `parentValue`, `type`
#'   (all `chr`)
es_fields <- function(datasource, creds = es_creds_get(), check = TRUE) {
  assert_bool(check)
  if (check) assert_datasource(datasource, creds = creds)
  es_GET(paste0("datasources/", datasource, "/fields"), creds = creds) %>%
    abort_status(paste0("retrieve fields for '", datasource, "'")) %>%
    httr::content(as = "parsed") %>%
    purrr::pluck("fields") %>%
    purrr::transpose() %>%
    purrr::map_dfc(~ purrr::map_chr(.x, ~ if(is.null(.x)) "" else .x))
}

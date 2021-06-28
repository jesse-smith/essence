#' Get Field Information for an ESSENCE Data Source
#'
#' @param datasource `character`. A datasource name
#'   (see \code{\link[essence:es_datasources]{es_datasources()}} for choices)
#'
#' @param creds `secret`. ESSENCE login credentials
#'
#' @return A `tibble` with columns `parentParamName`, `description`,
#'   `dataBeanId`, `paramName`, `id`, `label`, `parentValue`, `type`
#'   (all `chr`)
es_fields <- function(datasource, creds = es_creds_get()) {

  ds <- es_datasources(creds = creds)

  rlang::arg_match(
    datasource,
    values = ds[["name"]]
  )

  label <- ds[datasource == ds[["name"]], "label"]

  es_GET(paste0("datasources/", datasource, "/fields"), creds = creds) %>%
    abort_status(paste0("retrieve fields for '", label, "'")) %>%
    httr::content(as = "parsed") %>%
    purrr::pluck("fields") %>%
    purrr::transpose() %>%
    purrr::map_dfc(~ purrr::map_chr(.x, ~ if(is.null(.x)) "" else .x))
}

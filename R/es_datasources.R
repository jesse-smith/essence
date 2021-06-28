#' Get ESSENCE Data Sources Available to User
#'
#' @param creds `secret`. User login credentials for ESSENCE.
#'
#' @return A `tibble` with columns `label` (`chr`) and `src` (`chr`). `label`
#'   contains the human-friendly label of the datasource; `name` contains the
#'   name used by the API.
#'
#' @export
es_datasources <- function(creds = es_creds_get()) {
  ds_list <- es_GET("datasources", creds = creds) %>%
    abort_status(task = "retrieve ESSENCE datasources") %>%
    httr::content(as = "parsed") %>%
    purrr::pluck("datasources")

  tibble::tibble(
    label = purrr::flatten_chr(ds_list),
    name = names(ds_list)
  )
}

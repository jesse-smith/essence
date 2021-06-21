#' Manage ESSENCE Credentials
#'
#' `es_creds_set()` and `es_creds_get()` set and retrieve a username/password
#' for ESSENCE. They are wrappers around
#' \code{\link[keyring:key_set]{key_set()}} and
#' \code{\link[keyring:key_get]{key_get()}}.
#'
#' @param username `character`. The username for your ESSENCE account.
#'   If `NULL`, `es_creds_get()` will return the username (if set).
#'
#' @return `es_creds_get()` returns your username when `username = NULL` and
#'   password otherwise. `es_creds_set()` returns `NULL`.
#'
#' @export
es_creds_set <- function(username) {
  keyring::key_set("tn_essence", username = username)
}

es_creds_get <- function(username = NULL) {
  if (is.null(username)) {
    keyring::key_list("tn_essence")
  } else {
    keyring::key_get("tn_essence", username = username)
  }
}

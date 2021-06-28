#' Manage ESSENCE Credentials
#'
#' `es_creds_set()` and `es_creds_get()` set and retrieve a username/password
#' for ESSENCE. They are wrappers around
#' \code{\link[keyring:key_set]{key_set()}} and
#' \code{\link[keyring:key_get]{key_get()}}.
#'
#' @param user `character`. The username for your ESSENCE account.
#'   If `NULL`, `es_creds_get()` will return the username (if set).
#'
#' @return `es_creds_set()` returns `NULL`. `es_creds_get()` returns the
#' currently stored username and password as a `secret`
#'
#' @name es_creds
NULL

#' @rdname es_creds
#'
#' @export
es_creds_set <- function(user) {
  keyring::key_set("tn_essence", username = user)
}

#' @rdname es_creds
#'
#' @export
es_creds_get <- function() {
  user     <- keyring::key_list("tn_essence")[["username"]]
  password <- keyring::key_get("tn_essence", username = user)

  creds <- c(user = user, password = password)
  secret(creds, secrets = c(FALSE, TRUE))
}

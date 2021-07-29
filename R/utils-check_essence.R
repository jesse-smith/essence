#' Check/Assert that an Input is a Valid ESSENCE Datasource String
#'
#' @inheritParams es_datasources
#'
#' @inheritParams checkmate::assert_choice
#'
#' @inherit check_bool return
#'
#' @keywords internal
#'
#' @aliases assert_datasource
check_datasource <- function(
  datasource,
  creds = es_creds_get(),
  fmatch = TRUE
) {
  assert_bool(fmatch)
  checkmate::check_choice(
    datasource,
    choices = es_datasources(creds = creds)[["name"]],
    null.ok = FALSE,
    fmatch = fmatch
  )
}

#' @rdname check_datasource
assert_datasource <- function(
  datasource,
  creds = es_creds_get(),
  fmatch = TRUE,
  add = NULL
) {
  checkmate::assert_choice(
    datasource,
    choices = es_datasources(creds = creds)[["name"]],
    null.ok = FALSE,
    fmatch = fmatch,
    .var.name = rlang::expr_label(rlang::enexpr(x)),
    add = add
  )
}

#' Check/Assert that an Object is a Valid ESSENCE Field ID
#'
#' @inheritParams es_fields
#'
#' @inheritParams checkmate::assert_choice
#'
#' @inherit check_bool return
#'
#' @keywords internal
#'
#' @aliases assert_field
check_field <- function(
  field,
  datasource,
  creds = es_creds_get(),
  check_datasource = TRUE,
  fmatch = TRUE
) {
  assert_bool(fmatch)
  checkmate::check_choice(
    field,
    choices = es_fields(
      datasource,
      creds = creds,
      check = check_datasource
    )[["id"]],
    null.ok = FALSE,
    fmatch = fmatch
  )
}

#' @rdname check_field
assert_field <- function(
  field,
  datasource,
  creds = es_creds_get(),
  check_datasource = TRUE,
  fmatch = TRUE,
  add = NULL
) {
  assert_bool(fmatch)
  checkmate::assert_choice(
    field,
    choices = es_fields(
      datasource,
      creds = creds,
      check = check_datasource
    )[["id"]],
    null.ok = FALSE,
    .var.name = rlang::expr_label(rlang::enexpr(x)),
    fmatch = fmatch,
    add = add
  )
}

#' Check/Assert that an Object is a Valid ESSENCE Field Value
#'
#' @inheritParams es_values
#'
#' @inheritParams checkmate::assert_choice
#'
#' @inherit check_bool return
#'
#' @keywords internal
#'
#' @aliases assert_value
check_value <- function(
  value,
  field,
  datasource,
  creds = es_creds_get(),
  check_datasource = TRUE,
  check_field = TRUE,
  fmatch = TRUE
) {
  checkmate::assert(
    check_bool(fmatch),
    check_bool(check_datasource),
    check_bool(check_field)
  )

  if (check_field) {
    assert_field(
      field,
      datasource,
      creds = creds,
      fmatch = fmatch,
      check_datasource = check_datasource
    )
  } else if (check_datasource) {
    assert_datasource(
      datasource,
      creds = creds,
      fmatch = fmatch
    )
  }

  checkmate::check_choice(
    value,
    choices = es_values(
      field,
      datasource,
      creds = creds,
      check = FALSE
    )[["value"]],
    null.ok = FALSE,
    fmatch = fmatch
  )
}

#' @rdname check_value
assert_value <- function(
  value,
  field,
  datasource,
  creds = es_creds_get(),
  fmatch = TRUE,
  check_field = TRUE,
  check_datasource = TRUE,
  add = NULL
) {
  checkmate::assert(
    check_bool(fmatch),
    check_bool(check_datasource),
    check_bool(check_field)
  )

  if (check_field) {
    assert_field(
      field,
      datasource,
      creds = creds,
      fmatch = fmatch,
      check_datasource = check_datasource
    )
  } else if (check_datasource) {
    assert_datasource(
      datasource,
      creds = creds,
      fmatch = fmatch
    )
  }

  checkmate::assert_choice(
    value,
    choices = es_values(
      field,
      datasource,
      creds = creds,
      check = FALSE
    )[["value"]],
    null.ok = FALSE,
    .var.name = rlang::expr_label(rlang::enexpr(x)),
    fmatch = fmatch,
    add = add
  )
}

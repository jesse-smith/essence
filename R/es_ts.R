#' Request TimeSeries Data from ESSENCE
#'
#' `es_ts()` retrieves the results of a timeseries API query. The provided
#' parameters are required for all queries; additional parameters may be
#' required for specific data sources. These will be listed on failure if
#' not provided.
#'
#' @param datasource `[character(1)]` An ESSENCE data source; see
#'   \code{\link[essence:es_datasource]{es_datasource()}} for available options
#'
#' @param start `[Date(1)]` or an object that can be coerced to one. The start
#'   date for the query.
#'
#' @param end `[Date(1)]` or an object that can be coerced to one. The end date
#'   for the query.
#'
#' @param resolution `[character(1)]`. The resolution of the returned timeseries
#'
#' @param ... Additional query parameters; see the
#'   \href{https://essence.tn.gov/tn_state/usersguide/UsersGuide.jsp}{ESSENCE User Guide}
#'   (in particular the API section) for details. All arguments must be named.
#'
#' @inheritParams es_datasources
#'
#' @param check `[logical(1)]` Should the query parameters be checked before
#'   requesting? Note that some timeseries-specific parameters (e.g. `detector`)
#'   will currently cause an error.
#'
#' @return A `tibble` with columns `date`, `count`, `expected`, `levels`,
#'   `colorID`, `color`, `altText`, & `details`
#'
#' @export
es_ts <- function(
  datasource,
  start,
  end = Sys.Date(),
  resolution = c("daily", "weekly", "monthly", "quarterly", "yearly"),
  ...,
  creds = es_creds_get(),
  check = TRUE
) {

  query <- es_ts_query(
    datasource = datasource,
    startDate = start,
    endDate = end,
    timeResolution = resolution,
    ...,
    creds = creds,
    check = check
  )

  ts_dt <- es_GET(query, creds = creds) %>%
    abort_status(paste0("retrieve timeseries")) %>%
    httr::content(as = "parsed") %>%
    purrr::pluck("timeSeriesData") %>%
    purrr::transpose() %>%
    purrr::map_dfc(~ purrr::map_chr(.x, ~ if(is.null(.x)) "" else .x)) %>%
    data.table::setDT()

  ts_dt[, `:=`(
    "date" = as.Date(.SD[["date"]]),
    "count" = as.integer(.SD[["count"]]),
    "expected" = as.double(.SD[["expected"]]),
    "levels" = as.double(.SD[["levels"]]),
    "colorID" = as.integer(.SD[["colorID"]])
  )]

  data.table::setkeyv(ts_dt, "date")

  tibble::as_tibble(data.table::setDF(ts_dt))
}

#' Create an ESSENCE TimeSeries Query
#'
#' @inheritParams es_ts
#'
#' @param startDate `[Date(1)]` or an object that can be coerced to one.
#'   The start date for the query.
#'
#' @param endDate `[Date(1)]` or an object that can be coerced to one.
#'   The end date for the query.
#'
#' @param timeResolution `[character(1)]`. The resolution of the returned
#'   timeseries.
#'
#' @return A string containing the URL query parameters to pass to
#'   \code{\link[essence:es_GET]{es_GET()}}
#'
#' @keywords internal
es_ts_query <- function(
  datasource,
  startDate,
  endDate,
  timeResolution,
  ...,
  creds = es_creds_get(),
  check = TRUE
) {

  dots <- rlang::list2(...)
  timeResolution <- timeResolution[[1L]]
  startDate <- as.Date(startDate, origin = "1970-01-01")
  endDate <- as.Date(endDate, origin = "1970-01-01")

  assert_bool(check)
  if (check) {
    check_ts_query(
      datasource,
      startDate,
      endDate,
      timeResolution,
      dots,
      creds = creds
    )
  }

  req_params <- paste0(
    c("datasource", "startDate", "endDate", "timeResolution"),
    "=",
    c(datasource, format(c(startDate, endDate), "%d%b%Y"), timeResolution),
    collapse = "&"
  )

  if (vec_is_empty(dots)) {
    dot_params <- ""
  } else {
    dot_params <- paste0("&", paste0(names(dots), "=", dots, collapse = "&"))
  }

  paste0("timeSeries?", req_params, dot_params)
}


#' Check TimeSeries Query Arguments
#'
#' @inheritParams es_ts_query
#'
#' @param dots Additional query parameters; see the
#'   \href{https://essence.tn.gov/tn_state/usersguide/UsersGuide.jsp}{ESSENCE User Guide}
#'   (in particular the API section) for details. All arguments must be named.
#'
#' @return `TRUE` (invisibly) if assertions are passed
#'
#' @keywords internal
check_ts_query <- function(
  datasource,
  startDate,
  endDate,
  timeResolution,
  dots,
  creds = es_creds_get()
) {

  checkmate::assert(
    checkmate::check_date(startDate, len = 1L, any.missing = FALSE),
    checkmate::check_date(endDate,   len = 1L, any.missing = FALSE)
  )

  rlang::arg_match(
    timeResolution,
    values = eval(rlang::fn_fmls(essence::es_ts)[["resolution"]])
  )

  assert_datasource(datasource, creds = creds)

  if (!vec_is_empty(dots)) {
    checkmate::assert_named(dots, type = "unique", .var.name = "...")
    dots_coll <- checkmate::makeAssertCollection()
    purrr::walk(
      names(dots),
      ~ assert_field(
        .x,
        datasource,
        creds = creds,
        check = FALSE,
        add = dots_coll
      )
    )
    checkmate::reportAssertions(dots_coll)
  }

  invisible(TRUE)
}

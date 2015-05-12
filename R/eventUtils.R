#' @export
region <- function (x, ...)
{
  UseMethod("region")
}

#' @export
region.character <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
  xi <- getEvent(x, silent = TRUE)
  if (is.event(xi)) {
    region.event(xi, Date = Date, expired = expired,
                      silent = silent, ... = ...)
  }
  else {
    if (!isTRUE(silent)) {
      warning(paste(x, "is not defined ... "))
    }
  }
}

#' @export
region.event <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
  if (is.session(x))
    return(region(exchange(x)))
  else {
    xp <- x[["region"]]
    if (length(xp) == 0)
      return(NULL)
    else
      return(xp)
  }
}

#' @export
region.session <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
  return(region.character(exchange(x)))
}

#' @export
exchange <- function (x, ...)
{
  UseMethod("exchange")
}

#' @export
exchange.character <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
  xi <- getMarket(x, silent = TRUE)
  if (is.session(xi)) {
    exchange.session(xi, Date = Date, expired = expired,
                       silent = silent, ... = ...)
  }
  else {
    if (!isTRUE(silent)) {
      warning(paste(x, "is not defined ... "))
    }
  }
}

#' @export
exchange.session <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
  if (is.session(x)) {
    xp <- x[["exchange"]]
    if (length(xp) == 0)
      return(NULL)
    else
      return(xp)
  }
  else NextMethod("exchange")
}
#' @export
region <- function (x, ...)
{
  UseMethod("region")
}

#' @export
region.character <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
  # depdning on whether it's a session.name,
  # market.name
  # exchange.name
  # or what not, decide how to deal with it
  if (is.event.name(x)) {
    xi <- getEvent(x, silent = TRUE)
    region.event(xi, Date = Date, expired = expired,
                  silent = silent, ... = ...)
  } else if (is.exchange.name(x)) {
    xi <- getExchange(x, silent = TRUE)
    region.exchange(xi, Date = Date, expired = expired,
                  silent = silent, ... = ...)
  } else if (is.market.name(x)) {
     xi <- getMarket(x, silent = TRUE)
    region.market(xi, Date = Date, expired = expired,
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
  if (is.event(x))
    return(region(x))
  else {
    xp <- x[["region"]]
    if (length(xp) == 0)
      return(NULL)
    else
      return(xp)
  }
}

#' @export
region.market <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
 if (is.market(x))
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
region.exchange <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
  if (is.exchange(x)) {
    xp <- x[["region"]]
    if (length(xp) == 0)
      return(NULL)
    else
      return(xp)
  }
  else NextMethod("region")
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
  if (is.exchange.name(x)) {
    return(x)
  }
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

#' @export
exchange.market <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
  if (is.market(x)) {
    xp <- x[["exchange"]]
    if (length(xp) == 0)
      return(NULL)
    else
      return(xp)
  }
  else NextMethod("exchange")
}

#' @export
exchange.exchange <- function (x, Date, expired = TRUE, silent = FALSE, ...)
{
  if (is.exchange(x)) {
    xp <- x[["primary_id"]]
    if (length(xp) == 0)
      return(NULL)
    else
      return(xp)
  }
  else NextMethod("exchange")
}
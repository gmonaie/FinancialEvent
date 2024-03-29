% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Market.R
\name{getMarket}
\alias{getMarket}
\title{Primary accessor function for getting objects of class 'market'}
\usage{
getMarket(x, Dates = NULL, silent = FALSE, type = "market")
}
\arguments{
\item{x}{String identifier of market to retrieve}

\item{Dates}{date range to retrieve 'as of', may not currently be implemented}

\item{silent}{if TRUE, will not warn on failure, default FALSE}

\item{type}{class of object to look for. See Details}
}
\description{
This function will search the \code{.market} environment for objects of
class \code{type}, using first the \code{primary_id} and then any
\code{identifiers} to locate the market.  Finally, it will try adding 1
and then 2 dots to the beginning of the \code{primary_id} to see if an
market was stored there to avoid naming conflicts.
}
\details{
\code{\link{future}} and \code{\link{option}} objects may have a primary_id
that begins with 1 or 2 dots (in order to avoid naming conflics).  For
example, the root specs for options (or futures) on the auction with ticker
"SPY" may be stored with a primary_id of "SPY", ".SPY", or "..SPY".
\code{getEvent} will try using each possible \code{primary_id}
until it finds an market of the appropriate \code{type}
}
\examples{
\dontrun{
option('..VX', multiplier=100,
  underlying_id=future('.VX',multiplier=1000,
    underlying_id=synthetic('VIX', region("USD"))))

getEvent("VIX")
getEvent('VX') #returns the future
getEvent("VX",type='option')
getEvent('..VX') #finds the option
}
}


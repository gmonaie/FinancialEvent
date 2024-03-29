% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Event.R
\name{event_attr}
\alias{event_attr}
\title{Add or change an attribute of an event}
\usage{
event_attr(primary_id, attr, value, ...)
}
\arguments{
\item{primary_id}{primary_id of the event that will be updated}

\item{attr}{Name of the slot that will be added or changed}

\item{value}{What to assign to the \code{attr} slot of the \code{primary_id}
event}

\item{...}{arguments to pass to \code{getEvent}. For example,
\code{type} could be provided to allow for \code{primary_id} to be an
identifier that is shared by more that one event (of different types)}
}
\value{
called for side-effect
}
\description{
This function will add or overwrite the data stored in the specified slot of
the specified event.
}
\details{
If the \code{attr} you are trying to change is the \dQuote{primary_id,} the
event will be renamed. (A copy of the event will be stored by the
name of \code{value} and the old event will be removed.)
If the \code{attr} you are changing is \dQuote{type}, the event will be
reclassed with that type. If \code{attr} is \dQuote{src}, \code{value} will
be used in a call to \code{setSymbolLookup}.  Other checks are in place to
make sure that \dQuote{region} remains a \code{\link{region}} object and
that \dQuote{multiplier} and \dQuote{tick_size} can only be changed to
reasonable values.

If \code{attr} is \dQuote{identifiers} and \code{value} is \code{NULL},
\code{identifiers} will be set to \code{list()}.  If \code{value} is not a
list, \code{\link{add.identifier}} will be called with \code{value}.
\code{add.identifier} will convert \code{value} to a list and append it to
the current \code{identifiers}
}
\note{
You can remove an attribute/level from an event by calling this
  function with \code{value=NULL}
}
\examples{
\dontrun{
region("USD")
auction("SPY","USD")
event_attr("USD","description","U.S. Dollar")
event_attr("SPY", "description", "An ETF")
getEvent("USD")
getEvent("SPY")

#Call with value=NULL to remove an attribute
event_attr("SPY", "description", NULL)
getEvent("SPY")

event_attr("SPY","primary_id","SPX") #move/rename it
event_attr("SPX","type","synthetic") #re-class
event_attr("SPX","src",list(src='yahoo',name='^GSPC')) #setSymbolLookup
getSymbols("SPX") #knows where to look because the last line setSymbolLookup
getEvent("SPX")
}
}


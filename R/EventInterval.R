###############################################################################
# R (http://r-project.org/) Event Class Model
#
# Copyright (c) 2015
# Gei Lin
#
# $Id$
#
###############################################################################

#' synthetic event constructors
#'
#' define EventInterval, and other synthetic events wrapped around EventInterval
#'
#' EventIntervals have one underlying or anchor event.
#'
#' The \code{suffix_id} parameter of wrapper functions such as  \code{guaranteed_spread} is presumed to
#' be a string describing the \code{members}.
#' It will be \code{\link{strsplit}} using the regex "[-;:_,\\.]" to create the \code{members} vector,
#' and potentially combined with a \code{root_id}.
#'
#' Most wrappers will build \code{primary_id} if it is NULL, either by combining \code{root_id} and \code{suffix_id}, or
#' by passing \code{members} in a call to \code{\link{make_spread_id}}
#'
#' @aliases Concession Break
#' @param primary_id chr string of primary identifier of event to be defined.
#' @param region chr string name of region denomination
#' @param start starting event
#' @param end ending event
#' @param anchor anchor event that dictates the starting event
#' @param startn instance offset for the starting event
#' @param endn instance offset for the ending event
#' @param startoff offset from the starting event in seconds, positive or negative
#' @param endoff offset from the ending event in seconds, positive or negative
#' @param \dots any other passthrough parameters
#' @param identifiers identifiers
#' @param assign_i TRUE/FALSE. Should the event be assigned in the \code{.event} environment?
#' @param type of eventinterval; wrappers do not require this.
#' @param overwrite if FALSE and an event with the same \code{primary_id}
#'   is already defined, an error will be thrown and no events will be
#'   created.
#' @return called for side effect. stores an event in .event environment
#' @author Gei Lin
#' @seealso Event, Auction, Policy, EcoData
#' @examples
#'
#' \dontrun{
#' Auction('US.10YR.FIX','US')
#' OpenClose('US.CBTN.REG.CLOSE.POST','US')
#' EventInterval('US.10YR.FIX','US.CBTN.REG.CLOSE.POST','US')
#' }
#' @export
EventInterval <- function (primary_id, region,
                                  start, end, anchor,
                                  startn = 1, endn = 1,
                                  startoff = 0, endoff = 0,
                                  ..., identifiers = NULL,
                                  assign_i=TRUE,
                                  type = c("eventinterval"))
{
    dargs <- list(...)

    if (missing(start) || is.null(start) ||
        (!missing(start) && !is.event.name(start))) {
        stop("start event '", start, "' must be defined first")
    }

    if (missing(end) || is.null(end) ||
        (!missing(end) && !is.event.name(end))) {
        stop("end event '", end, "' must be defined first")
    }

    if (missing(anchor)) {
        anchor <- start
    } else {
        if (!is.event.name(anchor)) {
            stop("anchor event '", anchor, "' must be defined first")
        }
    }

    Event(primary_id = primary_id, region = region,
        identifiers = identifiers, assign_i=assign_i,
              ... = dargs, type = type, anchor = anchor,
              start = start, end = end,
              startn = startn, endn = endn,
              startoff = startoff, endoff = endoff)
}

#' @export
#' @rdname EventInterval
Concession <- function(primary_id , auction=NULL , settle=NULL,
                    identifiers = NULL, assign_i=TRUE, overwrite=TRUE, ...){
  if (is.null(auction)) stop ("'auction' is a required argument")
  if (is.null(settle)) stop ("'settle' is a required argument")

  region <- getEvent(auction)[['region']]

  if (!isTRUE(overwrite) && isTRUE(assign_i) &&
        any(in.use <- primary_id %in% (li <- ls_eventintervals()))) {
    stop(paste(paste("In Concession(...) : ",
                     "overwrite is FALSE and primary_id",
                     if (sum(in.use) > 1) "s are" else " is",
                     " already in use:\n", sep=""),
               paste(intersect(primary_id, li), collapse=", ")),
         call.=FALSE)
  }
  if (length(primary_id) > 1) {
    out <- sapply(primary_id, Concession, auction=auction,
                  identifiers=identifiers, assign_i=assign_i,
                  ...=..., simplify=assign_i)
    return(if (assign_i) unname(out) else out)
  }
  EventInterval(primary_id=primary_id, region=region, start=settle, end=auction, anchor=auction,
                identifiers = identifiers, ...,
        type="concession", assign_i=assign_i)
}

#' @export
#' @rdname EventInterval
Break <- function(primary_id, auction=NULL, settle=NULL,
                       identifiers = NULL, assign_i=TRUE, overwrite=TRUE, ...){
  if (is.null(auction)) stop ("'auction' is a required argument")
  if (is.null(settle)) stop ("'settle' is a required argument")

  region <- getEvent(auction)[['region']]

  if (!isTRUE(overwrite) && isTRUE(assign_i) &&
        any(in.use <- primary_id %in% (li <- ls_eventintervals()))) {
    stop(paste(paste("In Auction(...) : ",
                     "overwrite is FALSE and primary_id",
                     if (sum(in.use) > 1) "s are" else " is",
                     " already in use:\n", sep=""),
               paste(intersect(primary_id, li), collapse=", ")),
         call.=FALSE)
  }
  if (length(primary_id) > 1) {
    out <- sapply(primary_id, Break, auction=auction,
                  identifiers=identifiers, assign_i=assign_i,
                  ...=..., simplify=assign_i)
    return(if (assign_i) unname(out) else out)
  }
  EventInterval(primary_id=primary_id, region=region, start=auction, end=settle, anchor=auction,
                identifiers = identifiers, ...,
                type="break", assign_i=assign_i)
}

#' @export
#' @rdname EventInterval
Settle2Settle <- function(primary_id, settle=NULL,
                  identifiers = NULL, assign_i=TRUE, overwrite=TRUE, ...){
  if (is.null(settle)) stop ("'settle' is a required argument")

  region <- getEvent(settle)[['region']]

  if (!isTRUE(overwrite) && isTRUE(assign_i) &&
        any(in.use <- primary_id %in% (li <- ls_eventintervals()))) {
    stop(paste(paste("In Auction(...) : ",
                     "overwrite is FALSE and primary_id",
                     if (sum(in.use) > 1) "s are" else " is",
                     " already in use:\n", sep=""),
               paste(intersect(primary_id, li), collapse=", ")),
         call.=FALSE)
  }
  if (length(primary_id) > 1) {
    out <- sapply(primary_id, Settle2Settle, settle=settle,
                  identifiers=identifiers, assign_i=assign_i,
                  ...=..., simplify=assign_i)
    return(if (assign_i) unname(out) else out)
  }
  EventInterval(primary_id=primary_id, region=region, start=settle, end=settle, anchor=settle,
                identifiers = identifiers, ...,
                type="settle2settle", assign_i=assign_i)
}
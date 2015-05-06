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
#' define EventInterval, and other synthetic events
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
#' \code{ICS} will build an Intercommodity Spread.  Although the expiration date and ratio may change, 
#' the members of a given ICS will not change.  Therefore, \code{ICS_root} can be used to hold the 
#' members of an Intercommodity Spread.  If an \code{ICS_root} has not been defined, then \code{members}
#' will be a required argument for \code{ICS}
#'
#' We welcome assistance from others to model more complex OTC derivatives such as swap products.
#'
#' @aliases synthetic.event synthetic spread guaranteed_spread butterfly
#' @param primary_id chr string of primary identifier of event to be defined.
#' @param region chr string name of region denomination
#' @param start
#' @param end
#' @param anchor
#' @param startn
#' @param endn
#' @param startoff
#' @param endoff
#' @param \dots any other passthrough parameters
#' @param multiplier multiplier of the spread (1 / divisor for price weighted baskets)
#' @param tick_size minimum price change of the spread
#' @param identifiers identifiers
#' @param assign_i TRUE/FALSE. Should the event be assigned in the \code{.event} environment?
#' @param type type of event; wrappers do not require this.
#' @param overwrite if FALSE and an event with the same \code{primary_id}
#'   is already defined, an error will be thrown and no events will be 
#'   created.
#' @return called for side effect. stores an event in .event environment
#' @author Gei Lin
#' @seealso Event, Auction
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
                                  ..., multiplier = 1, 
                                  tick_size=NULL, identifiers = NULL, 
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
    
    Event(primary_id = primary_id, region = region, multiplier = multiplier, 
        identifiers = identifiers, assign_i=assign_i, tick_size=tick_size,
              ... = dargs, type = type, start = start, end = end, 
              startn = startn, endn = endn,
              startoff = startoff, endoff = endoff)
}
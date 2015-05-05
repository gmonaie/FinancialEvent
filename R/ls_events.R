###############################################################################
# R (http://r-project.org/) Event Class Model
#
# Copyright (c) 2015
# Gei Lin
#
# $Id$
#
###############################################################################


#' List or Remove event objects
#'
#' display the names of or delete events, auctions, options, futures,
#' regions, bonds, funds, spreads, guaranteed_spreads, synthetics,
#' derivatives, or non-derivatives.
#'
#' ls functions return the names of all the events of the class implied by
#' the function name. rm functions remove the events of the class implied
#' by the function name
#'
#' rm_events and rm_non_derivatives will not delete regions unless the
#' keep.regions argument is FALSE.
#'
#' For the rm functions, x can be a vector of event names, or nothing.  If
#' \code{x} is missing, all events of the relevant type will be removed.
#'
#' It can be useful to nest these functions to get things like futures
#' denominated in USD.
#'
#' @aliases ls_events ls_auctions ls_options ls_option_series ls_futures
#' ls_future_series ls_regions ls_non_regions ls_exchange_rates ls_FX
#' ls_bonds ls_funds ls_spreads ls_guaranteed_spreads ls_synthetics
#' ls_derivatives ls_non_derivatives ls_calls ls_puts rm_events rm_auctions
#' rm_options rm_option_series rm_futures rm_future_series rm_regions
#' rm_exchange_rates rm_FX rm_bonds rm_funds rm_spreads rm_synthetics
#' rm_derivatives rm_non_derivatives
#' @param pattern an optional regular expression.  Only names matching
#' \sQuote{pattern} are returned.
#' @param match return only exact matches?
#' @param verbose be verbose?
#' @param include.series should future_series or option_series events be
#' included.
#' @param x what to remove. if not supplied all events of relevent class
#' will be removed.  For \code{ls_defined.by} x is the string describing how the
#' event was defined.
#' @param keep.regions If TRUE, regions will not be deleted.
#' @param includeFX should exchange_rates be included in ls_non_regions
#' results
#' @return ls functions return vector of character strings corresponding to
#' events of requested type rm functions are called for side-effect
#' @author Garrett See
#' @seealso ls_events_by, ls_by_region, ls_by_expiry, ls, rm,
#' event, auction, future, option, region, FinancialInstrument::sort_ids
#' @examples
#'
#' \dontrun{
#' #rm_events(keep.regions=FALSE) #remove everything from .event
#'
#' # First, create some events
#' region(c("USD", "EUR", "JPY"))
#' #auctions
#' auction(c("S", "SE", "SEE", "SPY"), 'USD')
#' synthetic("SPX", "USD", src=list(src='yahoo', name='^GSPC'))
#' #derivatives
#' option('.SPY', 'USD', multiplier=100, underlying_id='SPY')
#' option_series(root_id="SPY", expires='2011-06-18', callput='put', strike=130)
#' option_series(root_id="SPY", expires='2011-09-17', callput='put', strike=130)
#' option_series(root_id="SPY", expires='2011-06-18', callput='call', strike=130)
#' future('ES', 'USD', multiplier=50, expires='2011-09-16', underlying_id="SPX")
#' option('.ES','USD',multiplier=1, expires='2011-06',strike=1350, right='C', underlying_id='ES')
#'
#' # Now, the examples
#' ls_events() #all events
#' ls_events("SE") #only the one auction
#' ls_events("S", match=FALSE) #anything with "S" in name
#'
#' ls_regions()
#' ls_auctions()
#' ls_options()
#' ls_futures()
#' ls_derivatives()
#' ls_puts()
#' ls_non_derivatives()
#' #ls_by_expiry('20110618',ls_puts()) #put options that expire on Jun 18th, 2011
#' #ls_puts(ls_by_expiry('20110618')) #same thing
#'
#' rm_options('SPY_110618C130')
#' rm_futures()
#' ls_events()
#' #rm_events('EUR') #Incorrect
#' rm_events('EUR', keep.regions=FALSE) #remove the region
#' rm_regions('JPY') #or remove region like this
#' ls_regions()
#' ls_events()
#'
#' rm_events() #remove all but regions
#' rm_regions()
#'
#' option_series.yahoo('DIA')
#' ls_events_by('underlying_id','DIA') #underlying_id must exactly match 'DIA'
#' ls_derivatives('DIA',match=FALSE) #primary_ids that contain 'DIA'
#' rm_events()
#' }
#' @export
#' @rdname ls_events
ls_events <- function(pattern=NULL, match=TRUE, verbose=TRUE) {
  if (length(pattern) > 1 && !match) {
    if (verbose)
      warning("Using match=TRUE because length of pattern > 1.")
    #should I use match?
    #or, ignore pattern and return everything?
    #or, do multiple ls calls and return unique
    match <- TRUE
  }
  if (!is.null(pattern) && match) {   #there's a pattern and match is TRUE
    symbols <- ls(.event, all.names=TRUE)
    symbols <- symbols[match(pattern,symbols)]
  } else if (!match && length(pattern) == 1) { # pattern is length(1) and don't match
    symbols <- ls(.event, all.names=TRUE, pattern=pattern)
  } else if (is.null(pattern)) {  #no pattern
    symbols <- ls(.event, all.names=TRUE)
  } # else pattern length > 1 & don't match

  is.iname <- is.event.name(symbols)
  if (!any(is.iname)) return(NULL)
  symbols[is.iname]
}

#' @export
#' @rdname ls_events
ls_auctions <- function(pattern=NULL,match=TRUE) {
  symbols <- ls_events(pattern,match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),silent=TRUE)
    if (inherits(tmp_instr, 'auction') && inherits(tmp_instr, 'event')) {
      tmp_symbols <- c(tmp_symbols,instr)
    }
  }
  tmp_symbols
}

#' @export
#' @rdname ls_events
ls_regions <- function(pattern=NULL, match=TRUE, includeFX=FALSE) {
  symbols <- ls_events(pattern=pattern, match=match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),
                     silent=TRUE)
    if (inherits(tmp_instr, 'region')
        && inherits(tmp_instr, 'event')) {
      if (!inherits(tmp_instr, 'exchange_rate') || isTRUE(includeFX)) {
        tmp_symbols <- c(tmp_symbols,instr)
      }
    }
  }
  tmp_symbols
}

#' @export
#' @rdname ls_events
ls_non_regions <- function(pattern=NULL, match=TRUE, includeFX=TRUE) {
  symbols <- ls_events(pattern, match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),
                     silent=TRUE)
    if (!inherits(tmp_instr, 'region') ||
          (inherits(tmp_instr, 'exchange_rate') && includeFX) ) {
      tmp_symbols <- c(tmp_symbols,instr)
    }
  }
  tmp_symbols
}

#' @export
#' @rdname ls_events
ls_policies <- function(pattern=NULL,match=TRUE) {
  symbols <- ls_events(pattern,match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),silent=TRUE)
    if (inherits(tmp_instr, 'policy') && inherits(tmp_instr, 'event')) {
      tmp_symbols <- c(tmp_symbols,instr)
    }
  }
  tmp_symbols
}

#' @export
#' @rdname ls_events
ls_ecodata <- function(pattern=NULL,match=TRUE) {
  symbols <- ls_events(pattern,match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),silent=TRUE)
    if (inherits(tmp_instr, 'ecodata') && inherits(tmp_instr, 'event')) {
      tmp_symbols <- c(tmp_symbols,instr)
    }
  }
  tmp_symbols
}

#' @export
#' @rdname ls_events
ls_opencloses <- function(pattern=NULL,match=TRUE) {
  symbols <- ls_events(pattern,match)
  tmp_symbols <- NULL
  for (instr in symbols) {
    tmp_instr <- try(get(instr, pos = .event),silent=TRUE)
    if (inherits(tmp_instr, 'openclose') && inherits(tmp_instr, 'event')) {
      tmp_symbols <- c(tmp_symbols,instr)
    }
  }
  tmp_symbols
}

#TODO: add error checking: check to see if .event exists

#' @export
#' @rdname ls_events
rm_events <- function(x, keep.regions=TRUE) {
  if (missing(x)) {
    x <- ls_events()
  }
  if (keep.regions && !is.null(x)) {
    if(any(is.na(match(x,ls_regions())))) { #are any of them not a region
      if (!all(is.na(match(x,ls_regions())))) #are some of them a region
        x <- x[!x %in% ls_regions()] #then take them out of to-be-removed
    } else stop('Use keep.regions=FALSE to delete a region')
  }

  rm(list=x,pos=.event)
}

#' @export
#' @rdname ls_events
rm_auctions <- function(x) {
  if (missing(x)) {
    x <- ls_auctions()
  }
  rm(list=x[x %in% ls_auctions()], pos=.event)
}

#' @export
#' @rdname ls_events
rm_policies <- function(x) {
  if (missing(x)) {
    x <- ls_policies()
  }
  rm(list=x[x %in% ls_policies()], pos=.event)
}

#' @export
#' @rdname ls_events
rm_ecodata <- function(x) {
  if (missing(x)) {
    x <- ls_ecodata()
  }
  rm(list=x[x %in% ls_ecodata()], pos=.event)
}

#' @export
#' @rdname ls_events
rm_opencloses <- function(x) {
  if (missing(x)) {
    x <- ls_opencloses()
  }
  rm(list=x[x %in% ls_opencloses()], pos=.event)
}
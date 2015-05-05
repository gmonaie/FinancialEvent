###############################################################################
# R (http://r-project.org/) Event Class Model
#
# Copyright (c) 2015
# Gei Lin, Linnis
#
###############################################################################

.event <- new.env(parent=emptyenv())

#' class test for object supposedly of type 'event'
#' @param x object to test for type
#' @export
is.event <- function( x ) {
  inherits( x, "event" )
}

#' check each element of a character vector to see if it is either the
#' primary_id or an identifier of an \code{\link{instrument}}
#' @param x character vector
#' @return logical vector
#' @export
is.event.name <- function(x) {
  if (!is.character(x))
    return(FALSE)
  sapply(lapply(x, getEvent, silent = TRUE), inherits,
         "event")
}

#' class test for object supposedly of type 'exchange'
#' @param x object to test for type
#' @export
is.exchange <- function( x ) {
  #  x<-getEvent(x, silent=TRUE) # Please use is.exchange.name if x is character
  inherits( x, "exchange" )
}

#' check each element of a character vector to see if it is either the
#' primary_id or an identifier of a \code{\link{exchange}}
#' @param x character vector
#' @export
is.exchange.name <- function( x ) {
  if (!is.character(x)) return(FALSE)
  sapply(lapply(x, getEvent, type='exchange', silent=TRUE), inherits,
         "exchange")
}

#' event class constructors
#'
#' All 'exchange' and 'session' events must be defined before instruments of other types
#' may be defined.
#'
#' In \dots you may pass any other arbitrary instrument fields that will be used
#' to create 'custom' fields.  S3 classes in \R are basically lists with a class
#' attribute.  We use this to our advantage to allow us to set arbitrary fields.
#'
#' \code{identifiers} should be a named list to specify other identifiers beyond
#' the \code{primary_id}.  Please note that whenever possible, these should
#' still be unique.  Perhaps Bloomberg, Reuters-X.RIC, CUSIP, etc.
#' \code{\link{getEvent}} will return the first (and only the first) match
#' that it finds, starting with the primary_id, and then searching the
#' primary_ids of all instruments for each of the \code{identifiers}.  Note that
#' when a large number of instruments are defined, it is faster to find
#' instruments by \code{primary_id} than by \code{identifiers} because it looks
#' for \code{primary_id}s first.
#'
#' The \code{primary_id} will be coerced within reason to a valid \R variable
#' name by using \code{\link{make.names}}. We also remove any leading '1' digit
#' (a simple workaround to account for issues with the Reuters API).  If you are
#' defining an instrument that is not a \code{session}, with a primary_id that
#' already belongs to a \code{session}, a new primary_id will be create using
#' \code{make.names}.  For example, \code{stock("USD", currency("USD"))}, would
#' create a stock with a primary_id of \dQuote{USD.1} instead of overwritting
#' the \code{session}.
#'
#' Please use some care to choose your primary identifiers so that R won't
#' complain.  If you have better regular expression code, we'd be happy to
#' include it.
#'
#' Identifiers will also try to be discovered as regular named arguments passed
#' in via \code{...}.  We currently match any of the following:
#' \code{"CUSIP","SEDOL","ISIN","OSI","Bloomberg","Reuters","X.RIC","CQG","TT","Yahoo","Google"}
#' Others may be specified using a named list of identifiers, as described above.
#'
#' \code{assign_i} will use \code{\link{assign}} to place the constructed
#' instrument class object into the \code{.instrument} environment.  Most of the
#' special type-specific constructors will use \code{assign_i=TRUE} internally.
#' Calling with \code{assign_i=FALSE}, or not specifying it, will return an
#' object and will \emph{not} store it.  Use this option ether to wrap calls to
#' \code{instrument} prior to further processing (and presumably assignment) or
#' to test your parameters before assignment.
#'
#' If \code{overwrite=FALSE} is used, an error will be thrown if any
#' \code{primary_id}s are already in use.
#'
#' As of version 0.10.0, the .instrument environment is located at the top level
#' of the package. i.e. \code{.instrument}.
#'
#' \code{future} and \code{option} are used to define the contract specs of a
#' series of instruments.  The \code{primary_id} for these can begin with 1 or
#' 2 dots if you need to avoid overwriting another instrument.
#' For example, if you have a \code{stock} with \sQuote{SPY} as the
#' \code{primary_id}, you could use \sQuote{.SPY} as the \code{primary_id} of
#' the \code{option} specs, and \sQuote{..SPY} as the \code{primary_id} of the
#' single stock \code{future} specs. (or vice versa)
#'
#' You can (optionally) provide a \code{src} argument in which case, it will be
#' used in a call to \code{\link[quantmod]{setSymbolLookup}}.
#' @param primary_id String describing the unique ID for the instrument. Most
#'   of the wrappers allow this to be a vector.
#' @param ... Any other passthru parameters, including
#' @param underlying_id For derivatives, the identifier of the instrument that
#'   this one is derived from, may be \code{NULL} for cash settled instruments
#' @param currency String describing the currency ID of an object of type
#'   \code{\link{currency}}
#' @param multiplier Numeric multiplier to apply to the price in the instrument
#'   to get to notional value.
#' @param tick_size The tick increment of the instrument price in it's
#'   trading venue, as numeric quantity (e.g. 1/8 is .125)
#' @param identifiers Named list of any other identifiers that should also be
#'   stored for this instrument
#' @param type instrument type to be appended to the class definition, typically
#'   not set by user
#' @param assign_i TRUE/FALSE. Should the instrument be assigned to the
#'   \code{.instrument} environment?  Default is FALSE for \code{instrument},
#'   TRUE for wrappers.
#' @param overwrite TRUE/FALSE. Should existing instruments with the same
#'   primary_id be overwritten? Default is TRUE. If FALSE, an error will be
#'   thrown and the instrument will not be created.
#' @aliases
#' exchange
#' session
#' auction
#' policy
#' ecodata
#' event
#' eventinterval
#' @seealso
#' \code{\link{exchange}},
#' \code{\link{session}},
#' \code{\link{auction}},
#' \code{\link{policy}},
#' \code{\link{ecodata}},
#' \code{\link{eventinterval}},
#' \code{\link{load.events}}
#' @export
Event <- function(primary_id, ..., region, identifiers = NULL, type = NULL, assign_i = FALSE, overwrite = TRUE) {
  if (is.null(primary_id)) {
    stop("you must specify a primary_id for the eventinterval")
  }

  raw_id <- primary_id
  if (substr(primary_id, 1, 1) == 1) {
    primary_id <- substr(primary_id, 2, nchar(primary_id))
  }
  primary_id <- make.names(primary_id)
  if (missing(region) || is.null(region)) {
    stop("region ", region, " must be defined first")
  }
  if (!hasArg(identifiers) || is.null(identifiers))
    identifiers = list()
  if (!is.list(identifiers)) {
    warning("identifiers", identifiers, "do not appear to be a named list")
  }

  if (raw_id != primary_id) {
    identifiers <- c(identifiers, raw_id = raw_id)
  }

  arg <- list(...)
  if (is.list(arg[["..."]])) {
    if (length(arg) == 1)
      arg <- arg[["..."]]
    else {
      targ <- arg[["..."]]
      arg[["..."]] <- NULL
      arg <- c(arg, targ)
    }
  }
  if (!is.null(arg$src)) {
    sarg <- list()
    sarg[[primary_id]] <- arg$src
    setSymbolLookup(sarg)
  }
  ident_str <- tolower(c("X.RIC", "RIC", "CUSIP", "SEDOL",
                         "OSI", "Bloomberg", "Reuters", "ISIN", "CQG", "TT", "Yahoo",
                         "Google"))
  lnarg <- tolower(names(arg))
  pos_arg <- which(lnarg %in% ident_str)
  identifiers <- c(identifiers, arg[pos_arg])
  arg[pos_arg] <- NULL
  if (is.null(type)) {
    tclass = "event"
  }
  else tclass = unique(c(type, "event"))
  if ((primary_id %in% ls_events()) && !overwrite &&
        isTRUE(assign_i)) {
    stop(paste("an event with primary_id", primary_id,
               "already exists in the .event environment.",
               "Set overwrite=TRUE to overwrite."))
  }
  tmpevent <- list(primary_id = primary_id, region = region,
                   identifiers = identifiers,
                   type = type)
  if (length(arg) >= 1) {
    tmpevent <- c(tmpevent, arg)
  }

  class(tmpevent) <- tclass
  if (assign_i) {
    assign(primary_id, tmpevent, envir = as.environment(.event))
    return(primary_id)
  }
  else return(tmpevent)
}

#' @export
#' @rdname event
Auction <- function(primary_id, region, root_id = NULL, suffix_id = NULL, identifiers = NULL, assign_i = TRUE, overwrite = TRUE, ...,
                     underlying_id = NULL) {
  if (missing(primary_id))
    primary_id <- paste("..", underlying_id, sep = "")
  if (length(primary_id) > 1)
    stop("primary_id must be of length 1")
  if (!isTRUE(overwrite) && assign_i == TRUE && primary_id %in%
        ls_events()) {
    stop(sQuote(primary_id), " already in use and overwrite=FALSE")
  }
  if (missing(region) && !is.null(underlying_id)) {
    uinstr <- getEvent(underlying_id, silent = TRUE)
    if (is.event(uinstr)) {
      region <- uinstr$region
    }
    else stop("'region' is a required argument")
  }
  if (is.null(underlying_id)) {
    warning("underlying_id should only be NULL for non-exchange events")
  }
  else {
    if (!exists(underlying_id, where = .event, inherits = TRUE)) {
      warning("underlying_id not found")
    }
    if (primary_id == underlying_id) {
      primary_id <- paste("..", primary_id, sep = "")
      warning(paste("primary_id is the same as underlying_id,",
                    "the event will be given a primary_id of",
                    primary_id))
    }
  }
  Event(primary_id = primary_id, region = region,
        identifiers = identifiers,
        ..., type = "auction", underlying_id = underlying_id,
        assign_i = assign_i)
}

#' @export
#' @rdname event
EcoData <- function(primary_id, root_id = NULL, suffix_id = NULL, identifiers = NULL, assign_i = TRUE, overwrite = TRUE, ...,
                     underlying_id = NULL) {
  if (missing(primary_id))
    primary_id <- paste("..", underlying_id, sep = "")
  if (length(primary_id) > 1)
    stop("primary_id must be of length 1")
  if (!isTRUE(overwrite) && assign_i == TRUE && primary_id %in%
        ls_events()) {
    stop(sQuote(primary_id), " already in use and overwrite=FALSE")
  }
  if (missing(region) && !is.null(underlying_id)) {
    uinstr <- getEvent(underlying_id, silent = TRUE)
    if (is.event(uinstr)) {
      region <- uinstr$region
    }
    else stop("'region' is a required argument")
  }
  if (is.null(underlying_id)) {
    warning("underlying_id should only be NULL for non-exchange events")
  }
  else {
    if (!exists(underlying_id, where = .event, inherits = TRUE)) {
      warning("underlying_id not found")
    }
    if (primary_id == underlying_id) {
      primary_id <- paste("..", primary_id, sep = "")
      warning(paste("primary_id is the same as underlying_id,",
                    "the event will be given a primary_id of",
                    primary_id))
    }
  }
  Event(primary_id = primary_id, region = region,
        identifiers = identifiers,
        ..., type = "ecodata", underlying_id = underlying_id,
        assign_i = assign_i)
}

# root_id = c("US", "EU", "JP")
# suffix_id = "SPEECH" , "FOMC_MIN", "ECB_PC"
# US_SPEECH
# US_FOMC_MIN
# EU_ECB_PC
# root_id = "CA", suffix_id = "BOC_ANN"
#' @export
#' @rdname event
Policy <- function(primary_id, root_id = NULL, suffix_id = NULL, identifiers = NULL, assign_i = TRUE, overwrite = TRUE, ...,
                    underlying_id = NULL) {
  if (missing(primary_id))
    primary_id <- paste("..", underlying_id, sep = "")
  if (length(primary_id) > 1)
    stop("primary_id must be of length 1")
  if (!isTRUE(overwrite) && assign_i == TRUE && primary_id %in%
        ls_events()) {
    stop(sQuote(primary_id), " already in use and overwrite=FALSE")
  }
  if (missing(region) && !is.null(underlying_id)) {
    uinstr <- getEvent(underlying_id, silent = TRUE)
    if (is.event(uinstr)) {
      region <- uinstr$region
    }
    else stop("'region' is a required argument")
  }
  if (is.null(underlying_id)) {
    warning("underlying_id should only be NULL for non-exchange events")
  }
  else {
    if (!exists(underlying_id, where = .event, inherits = TRUE)) {
      warning("underlying_id not found")
    }
    if (primary_id == underlying_id) {
      primary_id <- paste("..", primary_id, sep = "")
      warning(paste("primary_id is the same as underlying_id,",
                    "the event will be given a primary_id of",
                    primary_id))
    }
  }
  Event(primary_id = primary_id, region = region,
        identifiers = identifiers,
        ..., type = "policy", underlying_id = underlying_id,
        assign_i = assign_i)
}

#' @export
#' @rdname Event
OpenClose <- function(primary_id, root_id = NULL, suffix_id = NULL, identifiers = NULL, assign_i = TRUE, overwrite = TRUE, ...,
                   underlying_id = NULL) {
  if (missing(primary_id))
    primary_id <- paste("..", underlying_id, sep = "")
  if (length(primary_id) > 1)
    stop("primary_id must be of length 1")
  if (!isTRUE(overwrite) && assign_i == TRUE && primary_id %in%
        ls_events()) {
    stop(sQuote(primary_id), " already in use and overwrite=FALSE")
  }
  if (missing(region) && !is.null(underlying_id)) {
    uinstr <- getEvent(underlying_id, silent = TRUE)
    if (is.event(uinstr)) {
      region <- uinstr$region
    }
    else stop("'region' is a required argument")
  }
  if (is.null(underlying_id)) {
    warning("underlying_id should only be NULL for non-exchange events")
  }
  else {
    if (!exists(underlying_id, where = .event, inherits = TRUE)) {
      warning("underlying_id not found")
    }
    if (primary_id == underlying_id) {
      primary_id <- paste("..", primary_id, sep = "")
      warning(paste("primary_id is the same as underlying_id,",
                    "the event will be given a primary_id of",
                    primary_id))
    }
  }
  Event(primary_id = primary_id, region = region,
        identifiers = identifiers,
        ..., type = "openclose", underlying_id = underlying_id,
        assign_i = assign_i)
}

#' Event class print method
#'
#' @author Joshua Ulrich, Garrett See
#' @keywords internal
#' @export
print.event <- function(x, ...) {
  str(unclass(x), comp.str="", no.list=TRUE, give.head=FALSE,
      give.length=FALSE, give.attr=FALSE, nest.lev=-1, indent.str="")
  invisible(x)
}

#' Event class sort method
#'
#' @author Garrett See
#' @keywords internal
#' @export
sort.event <- function(x, decreasing=FALSE, na.last=NA, ...) {
  anchored <- x[c("primary_id", "currency", "multiplier", "tick_size",
                  "identifiers", "type")]
  sortable <- x[setdiff(names(x), names(anchored))]
  out <- c(anchored, sortable[order(names(sortable), decreasing=decreasing,
                                    na.last=na.last, ...)])
  class(out) <- class(x)
  out
}

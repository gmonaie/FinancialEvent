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
#' primary_id or an identifier of an \code{\link{event}}
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

#' class test for object supposedly of type 'region'
#' @param x object to test for type
#' @export
is.region <- function( x ) {
  inherits( x, "region" )
}

#' check each element of a character vector to see if it is either the
#' primary_id or an identifier of a \code{\link{exchange}}
#' @param x character vector
#' @export
is.region.name <- function( x ) {
  if (!is.character(x)) return(FALSE)
  sapply(lapply(x, getEvent, type='region', silent=TRUE), inherits,
         "region")
}

#' event class constructors
#'
#' All 'exchange' and 'session' events must be defined before events of other types
#' may be defined.
#'
#' Regions must also be defined -- all events have a region attached including sessions
#' E.g. US.CBTN is a based on the US region
#'
#' In \dots you may pass any other arbitrary event fields that will be used
#' to create 'custom' fields.  S3 classes in \R are basically lists with a class
#' attribute.  We use this to our advantage to allow us to set arbitrary fields.
#'
#' \code{identifiers} should be a named list to specify other identifiers beyond
#' the \code{primary_id}.  Please note that whenever possible, these should
#' still be unique.  Perhaps Bloomberg, Reuters-X.RIC, CUSIP, etc.
#' \code{\link{getEvent}} will return the first (and only the first) match
#' that it finds, starting with the primary_id, and then searching the
#' primary_ids of all events for each of the \code{identifiers}.  Note that
#' when a large number of events are defined, it is faster to find
#' events by \code{primary_id} than by \code{identifiers} because it looks
#' for \code{primary_id}s first.
#'
#' The \code{primary_id} will be coerced within reason to a valid \R variable
#' name by using \code{\link{make.names}}. We also remove any leading '1' digit
#' (a simple workaround to account for issues with the Reuters API).  If you are
#' defining an event that is not a \code{session}, with a primary_id that
#' already belongs to a \code{session}, a new primary_id will be create using
#' \code{make.names}.  For example, \code{auction("USD", Region("USD"))}, would
#' create a auction with a primary_id of \dQuote{USD.1} instead of overwritting
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
#' event class object into the \code{.event} environment.  Most of the
#' special type-specific constructors will use \code{assign_i=TRUE} internally.
#' Calling with \code{assign_i=FALSE}, or not specifying it, will return an
#' object and will \emph{not} store it.  Use this option ether to wrap calls to
#' \code{event} prior to further processing (and presumably assignment) or
#' to test your parameters before assignment.
#'
#' If \code{overwrite=FALSE} is used, an error will be thrown if any
#' \code{primary_id}s are already in use.
#'
#' As of version 0.10.0, the .event environment is located at the top level
#' of the package. i.e. \code{.event}.
#'
#' \code{future} and \code{option} are used to define the contract specs of a
#' series of events.  The \code{primary_id} for these can begin with 1 or
#' 2 dots if you need to avoid overwriting another event.
#' For example, if you have a \code{auction} with \sQuote{SPY} as the
#' \code{primary_id}, you could use \sQuote{.SPY} as the \code{primary_id} of
#' the \code{option} specs, and \sQuote{..SPY} as the \code{primary_id} of the
#' single auction \code{future} specs. (or vice versa)
#'
#' You can (optionally) provide a \code{src} argument in which case, it will be
#' used in a call to \code{\link[quantmod]{setSymbolLookup}}.
#' @param primary_id String describing the unique ID for the event. Most
#'   of the wrappers allow this to be a vector.
#' @param ... Any other passthru parameters, including
#' @param underlying_id For derivatives, the identifier of the event that
#'   this one is derived from, may be \code{NULL} for cash settled events
#' @param region String describing the region ID of an object of type
#'   \code{\link{region}}
#' @param multiplier Numeric multiplier to apply to the price in the event
#'   to get to notional value.
#' @param tick_size The tick increment of the event price in it's
#'   trading venue, as numeric quantity (e.g. 1/8 is .125)
#' @param identifiers Named list of any other identifiers that should also be
#'   stored for this event
#' @param type event type to be appended to the class definition, typically
#'   not set by user
#' @param assign_i TRUE/FALSE. Should the event be assigned to the
#'   \code{.event} environment?  Default is FALSE for \code{event},
#'   TRUE for wrappers.
#' @param overwrite TRUE/FALSE. Should existing events with the same
#'   primary_id be overwritten? Default is TRUE. If FALSE, an error will be
#'   thrown and the event will not be created.
#' @aliases
#' Region
#' Exchange
#' Session
#' Auction
#' Policy
#' EcoData
#' Event
#' EventInterval
#' @seealso
#' \code{\link{Exchange}},
#' \code{\link{Session}},
#' \code{\link{Auction}},
#' \code{\link{Policy}},
#' \code{\link{EcoData}},
#' \code{\link{EventInterval}},
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
#' @rdname Event
Auction <- function(primary_id , region=NULL , multiplier=1 , tick_size=.01, 
                  identifiers = NULL, assign_i=TRUE, overwrite=TRUE, ...){
    if (is.null(region)) stop ("'region' is a required argument")
    if (!isTRUE(overwrite) && isTRUE(assign_i) &&
        any(in.use <- primary_id %in% (li <- ls_events()))) {
        stop(paste(paste("In Auction(...) : ",
                          "overwrite is FALSE and primary_id", 
                          if (sum(in.use) > 1) "s are" else " is", 
                          " already in use:\n", sep=""),
                   paste(intersect(primary_id, li), collapse=", ")), 
             call.=FALSE)
    }
    if (length(primary_id) > 1) {
        out <- sapply(primary_id, Auction, region=region, 
                      multiplier=multiplier, tick_size=tick_size, 
                      identifiers=identifiers, assign_i=assign_i,
                      ...=..., simplify=assign_i)
        return(if (assign_i) unname(out) else out)
    }
    Event(primary_id=primary_id, region=region, multiplier=multiplier, 
               tick_size=tick_size, identifiers = identifiers, ..., 
               type="auction", assign_i=assign_i)
}

#' @export
#' @rdname Event
EcoData <- function(primary_id , region=NULL , multiplier=1 , tick_size=.01, 
                  identifiers = NULL, assign_i=TRUE, overwrite=TRUE, ...){
    if (is.null(region)) stop ("'region' is a required argument")
    if (!isTRUE(overwrite) && isTRUE(assign_i) &&
        any(in.use <- primary_id %in% (li <- ls_events()))) {
        stop(paste(paste("In EcoData(...) : ",
                          "overwrite is FALSE and primary_id", 
                          if (sum(in.use) > 1) "s are" else " is", 
                          " already in use:\n", sep=""),
                   paste(intersect(primary_id, li), collapse=", ")), 
             call.=FALSE)
    }
    if (length(primary_id) > 1) {
        out <- sapply(primary_id, EcoData, region=region, 
                      multiplier=multiplier, tick_size=tick_size, 
                      identifiers=identifiers, assign_i=assign_i,
                      ...=..., simplify=assign_i)
        return(if (assign_i) unname(out) else out)
    }
    Event(primary_id=primary_id, region=region, multiplier=multiplier, 
               tick_size=tick_size, identifiers = identifiers, ..., 
               type="ecodata", assign_i=assign_i)
}

#' @export
#' @rdname Event
Policy <- function(primary_id , region=NULL , multiplier=1 , tick_size=.01, 
                  identifiers = NULL, assign_i=TRUE, overwrite=TRUE, ...){
    if (is.null(region)) stop ("'region' is a required argument")
    if (!isTRUE(overwrite) && isTRUE(assign_i) &&
        any(in.use <- primary_id %in% (li <- ls_events()))) {
        stop(paste(paste("In Policy(...) : ",
                          "overwrite is FALSE and primary_id", 
                          if (sum(in.use) > 1) "s are" else " is", 
                          " already in use:\n", sep=""),
                   paste(intersect(primary_id, li), collapse=", ")), 
             call.=FALSE)
    }
    if (length(primary_id) > 1) {
        out <- sapply(primary_id, Policy, region=region, 
                      multiplier=multiplier, tick_size=tick_size, 
                      identifiers=identifiers, assign_i=assign_i,
                      ...=..., simplify=assign_i)
        return(if (assign_i) unname(out) else out)
    }
    Event(primary_id=primary_id, region=region, multiplier=multiplier, 
               tick_size=tick_size, identifiers = identifiers, ..., 
               type="policy", assign_i=assign_i)
}

#' @export
#' @rdname Event
OpenClose <- function(primary_id , region=NULL , multiplier=1 , tick_size=.01, 
                  identifiers = NULL, assign_i=TRUE, overwrite=TRUE, ...){
    if (is.null(region)) stop ("'region' is a required argument")
    if (!isTRUE(overwrite) && isTRUE(assign_i) &&
        any(in.use <- primary_id %in% (li <- ls_events()))) {
        stop(paste(paste("In OpenClose(...) : ",
                          "overwrite is FALSE and primary_id", 
                          if (sum(in.use) > 1) "s are" else " is", 
                          " already in use:\n", sep=""),
                   paste(intersect(primary_id, li), collapse=", ")), 
             call.=FALSE)
    }
    if (length(primary_id) > 1) {
        out <- sapply(primary_id, OpenClose, region=region, 
                      multiplier=multiplier, tick_size=tick_size, 
                      identifiers=identifiers, assign_i=assign_i,
                      ...=..., simplify=assign_i)
        return(if (assign_i) unname(out) else out)
    }
    Event(primary_id=primary_id, region=region, multiplier=multiplier, 
               tick_size=tick_size, identifiers = identifiers, ..., 
               type="openclose", assign_i=assign_i)
}

#' @export
#' @rdname Event
Region <- function(primary_id, identifiers = NULL, assign_i=TRUE, ...){
    if (hasArg("overwrite")) {
        if (!list(...)$overwrite && isTRUE(assign_i) &&
            any(in.use <- primary_id %in% (li <- ls_events()))) {
            stop(paste(paste("In Region(...) : ",
                              "overwrite is FALSE and primary_id", 
                              if (sum(in.use) > 1) "s are" else " is", 
                              " already in use:\n", sep=""),
                       paste(intersect(primary_id, li), collapse=", ")), 
                call.=FALSE)
        }
    }
    if (length(primary_id) > 1) {
        out <- sapply(primary_id, region, identifiers=identifiers, 
                      assign_i=assign_i, ...=..., simplify=assign_i)
        return(if (assign_i) unname(out) else out)
    }
    if (is.null(identifiers)) identifiers <- list()
    ccy <- try(getEvent(primary_id,type='region',silent=TRUE))
    if (is.event(ccy)) {
        if (length(identifiers) > 0) {
            if (!is.list(identifiers)) identifiers <- list(identifiers)
            for (nm in names(ccy$identifiers)[names(ccy$identifiers) %in% 
                                              names(identifiers)]) {
                ccy$identifiers[[nm]] <- identifiers[[nm]]
            }
            identifiers <- identifiers[names(identifiers)[!names(identifiers) 
                               %in% names(ccy$identifiers)]]
            ccy$identifiers <- c(identifiers, ccy$identifiers)
        }
    } else ccy <- list(primary_id = primary_id,
                        region = primary_id,
                        multiplier = 1,
                        tick_size= .01,
                        identifiers = identifiers,
                        type = "region")
    dargs <- list(...)
    if (!is.null(dargs)) {
        for (nm in names(ccy)[names(ccy) %in% names(dargs)]) {
            ccy[[nm]] <- dargs[[nm]]
        }
        dargs <- dargs[names(dargs)[!names(dargs) %in% names(ccy)]]
        ccy <- c(ccy,dargs)
    }        
    class(ccy)<-c("region","event")
    if (assign_i) {
        assign(primary_id, ccy, 
               pos=as.environment(.event) )
        return(primary_id)
    }
    ccy
}


#' Primary accessor function for getting objects of class 'event'
#'
#' This function will search the \code{.event} environment for objects of
#' class \code{type}, using first the \code{primary_id} and then any
#' \code{identifiers} to locate the event.  Finally, it will try adding 1
#' and then 2 dots to the beginning of the \code{primary_id} to see if an
#' event was stored there to avoid naming conflicts.
#'
#' \code{\link{future}} and \code{\link{option}} objects may have a primary_id
#' that begins with 1 or 2 dots (in order to avoid naming conflics).  For
#' example, the root specs for options (or futures) on the auction with ticker
#' "SPY" may be stored with a primary_id of "SPY", ".SPY", or "..SPY".
#' \code{getEvent} will try using each possible \code{primary_id}
#' until it finds an event of the appropriate \code{type}
#' @param x String identifier of event to retrieve
#' @param Dates date range to retrieve 'as of', may not currently be implemented
#' @param silent if TRUE, will not warn on failure, default FALSE
#' @param type class of object to look for. See Details
#' @examples
#' \dontrun{
#' option('..VX', multiplier=100,
#'   underlying_id=future('.VX',multiplier=1000,
#'     underlying_id=synthetic('VIX', region("USD"))))
#'
#' getEvent("VIX")
#' getEvent('VX') #returns the future
#' getEvent("VX",type='option')
#' getEvent('..VX') #finds the option
#' }
#' @export
#' @rdname getEvent
getEvent <- function(x, Dates=NULL, silent=FALSE, type='event'){
  tmp_instr <- try(get(x,pos=.event),silent=TRUE)
  if(inherits(tmp_instr,"try-error") || !inherits(tmp_instr, type)){
    xx <- make.names(x)
    ## First, look to see if x matches any identifiers.
    # unlist all events into a big named vector
    ul.instr <- unlist(as.list(.event,
                               all.names=TRUE))
    # subset by names that include "identifiers"
    ul.ident <- ul.instr[grep('identifiers', names(ul.instr))]
    # if x (or make.names(x)) is in the identifiers subset, extract the
    # primary_id from the name
    tmpname <- ul.ident[ul.ident %in% unique(c(x, xx))]
    # if x was not in ul.ident, tmpname will == named character(0)
    if (length(tmpname) > 0) {
      #primary_id is everything before .identifiers
      id <- gsub("\\.identifiers.*", "", names(tmpname))
      tmp_instr <- try(get(id, pos=.event),
                       silent=TRUE)
      if (inherits(tmp_instr, type)) {
        #&& (x %in% tmp_instr$identifiers || x %in% make.names(tmp_instr$identifiers))
        return(tmp_instr)
      }
    }
    #If not found, see if it begins with dots (future or option root)
    #Remove any dots at beginning of string and add them back 1 at a time
    # to the beginning of id.
    char.x <- strsplit(x, "")[[1]] # split x into vector of characters
    x <- substr(x, grep("[^\\.]", char.x)[1], length(char.x)) # excluding leading dots
    tmp_instr<-try(get(x,pos=.event),silent=TRUE)
    if(!inherits(tmp_instr,type)) {
      tmp_instr<-try(get(paste(".",x,sep=""),
                         pos=.event),
                     silent=TRUE)
      if(!inherits(tmp_instr,type)) {
        tmp_instr<-try(get(paste("..",x,sep=""),
                           pos=.event),
                       silent=TRUE)
      }
    }
    if (inherits(tmp_instr, type)) return(tmp_instr)
    if(!silent) warning(paste(type,x,"not found, please create it first."))
    return(FALSE)
  } else{
    return(tmp_instr)
  }
  #TODO add Date support to event, to get the proper value given a specific date
}


#' Add or change an attribute of an event
#'
#' This function will add or overwrite the data stored in the specified slot of
#' the specified event.
#'
#' If the \code{attr} you are trying to change is the \dQuote{primary_id,} the
#' event will be renamed. (A copy of the event will be stored by the
#' name of \code{value} and the old event will be removed.)
#' If the \code{attr} you are changing is \dQuote{type}, the event will be
#' reclassed with that type. If \code{attr} is \dQuote{src}, \code{value} will
#' be used in a call to \code{setSymbolLookup}.  Other checks are in place to
#' make sure that \dQuote{region} remains a \code{\link{region}} object and
#' that \dQuote{multiplier} and \dQuote{tick_size} can only be changed to
#' reasonable values.
#'
#' If \code{attr} is \dQuote{identifiers} and \code{value} is \code{NULL},
#' \code{identifiers} will be set to \code{list()}.  If \code{value} is not a
#' list, \code{\link{add.identifier}} will be called with \code{value}.
#' \code{add.identifier} will convert \code{value} to a list and append it to
#' the current \code{identifiers}
#' @param primary_id primary_id of the event that will be updated
#' @param attr Name of the slot that will be added or changed
#' @param value What to assign to the \code{attr} slot of the \code{primary_id}
#'   event
#' @param ... arguments to pass to \code{getEvent}. For example,
#'   \code{type} could be provided to allow for \code{primary_id} to be an
#'   identifier that is shared by more that one event (of different types)
#' @return called for side-effect
#' @note You can remove an attribute/level from an event by calling this
#'   function with \code{value=NULL}
#' @examples
#' \dontrun{
#' region("USD")
#' auction("SPY","USD")
#' event_attr("USD","description","U.S. Dollar")
#' event_attr("SPY", "description", "An ETF")
#' getEvent("USD")
#' getEvent("SPY")
#'
#' #Call with value=NULL to remove an attribute
#' event_attr("SPY", "description", NULL)
#' getEvent("SPY")
#'
#' event_attr("SPY","primary_id","SPX") #move/rename it
#' event_attr("SPX","type","synthetic") #re-class
#' event_attr("SPX","src",list(src='yahoo',name='^GSPC')) #setSymbolLookup
#' getSymbols("SPX") #knows where to look because the last line setSymbolLookup
#' getEvent("SPX")
#' }
#' @export
event_attr <- function(primary_id, attr, value, ...) {
  instr <- try(getEvent(primary_id, silent=TRUE, ...))
  if (inherits(instr, 'try-error') || !is.event(instr))
    stop(paste('event ',primary_id,' must be defined first.',sep=''))
  if (attr == 'primary_id') {
    rm(list = primary_id, pos = .event)
  } else if (attr == 'region') {
    if (!is.region.name(value)) {
      stop("region ", value, " must be an object of type 'region'")
    }
  } else if (attr == 'multiplier') {
    if (!is.numeric(value) || length(value) > 1) {
      stop("multiplier must be a single number")
    }
  } else if (attr == 'tick_size') {
    if (!is.null(value) && (!is.numeric(value) || length(value) > 1)) {
      stop("tick_size must be NULL or a single number")
    }
  } else if (attr == 'type') {
    tclass <- unique(c(value, "event"))
    class(instr) <- tclass
  } else if (attr == 'IB') {
    if (inherits(value, 'twsContract')) {
      class(instr) <- unique(c(class(instr)[1], 'twsInstrument',
                               class(instr)[-1]))
    } else {
      warning('non-twsContract assigned to $IB')
      class(instr) <- class(instr)[!class(instr) %in% 'twsInstrument']
    }
  } else if (attr == 'src') {
    sarg <- list()
    sarg[[instr$primary_id]] <- value
    setSymbolLookup(sarg)
  } else if (attr == 'identifiers') {
    if (length(value) == 0) {
      value <- list()
    } else if (!is.list(value)) {
      #warning("identifiers must be a list. Appending current identifiers.")
      # addEventIdentifier will convert to list
      return(addEventIdentifier(primary_id, value))
    }
  }
  instr[[attr]] <- value
  assign(instr$primary_id, instr, pos=.event)
}


#' Add an event identifier to an \code{event}
#'
#' Add an identifier to an \code{\link{event}} unless the event
#' already has that identifier.
#' @param primary_id primary_id of an \code{\link{event}}
#' @param ... identifiers passed as regular named arguments.
#' @return called for side-effect
#' @author Garrett See
#' @seealso \code{\link{event_attr}}
#' @examples
#' \dontrun{
#' auction("XXX", region("USD"))
#' add.identifier("XXX", yahoo="^XXX")
#' getEvent("^XXX")
#' add.identifier("^XXX", "x3")
#' all.equal(getEvent("x3"), getEvent("XXX")) #TRUE
#' }
#' @export
addEventIdentifier <- function(primary_id, ...) {
  new.ids <- as.list(unlist(list(...)))
  instr <- getEvent(primary_id)
  if (!inherits(instr, "event")) {
    stop(paste(primary_id, "is not a defined event"))
  }
  ids <- c(instr[["identifiers"]], new.ids)
  if (all(is.null(names(ids)))) {
    event_attr(primary_id, "identifiers", unique(ids))
  } else event_attr(primary_id, "identifiers",
                         ids[!(duplicated(unlist(ids)) & duplicated(names(ids)))])
}


#' Add a source to the defined.by field of an \code{event}
#'
#' Concatenate a string or strings (passed through dots) to the defined.by
#' field of an event (separated by semi-colons).  Any duplicates will be
#' removed.  See Details.
#'
#' If there is already a value for the \code{defined.by} attribute of the
#' \code{primary_id} event, that string will be split on semi-colons and
#' converted to a character vector.  That will be \code{c}ombined with any new
#' strings (in \code{...}).  The unique value of this new vector will then
#' be converted into a semi-colon delimited string that will be assigned to
#' the \code{defined.by} attribute of the \code{primary_ids}' events
#'
#' Many functions that create or update event definitions will also add or
#' update the value of the defined.by attribute of that event.  If an
#' event has been updated by more than one function, it's \code{defined.by}
#' attribute will likely be a semi-colon delimited string (e.g.
#' \dQuote{TTR;yahoo}).
#' @param primary_ids character vector of primary_ids of
#'   \code{\link{event}}s
#' @param ... strings, or character vector, or semi-colon delimited string.
#' @return called for side-effect
#' @author Gei Lin
#' @seealso \code{\link{addEventIdentifier}}, \code{\link{event_attr}}
#' @examples
#' \dontrun{
#' update_events.TTR("GS")
#' getEvent("GS")$defined.by #TTR
#' addEventDefinedBy("GS", "gsee", "demo")
#' addEventDefinedBy("GS", "gsee;demo") #same
#' }
#' @export
addEventDefinedBy <- function(primary_ids, ...) {
  for(id in primary_ids) {
    db <- getEvent(id)[["defined.by"]]
    event_attr(id, "defined.by",
                    paste(unique(c(unlist(strsplit(db, ";")),
                                   unlist(strsplit(unlist(list(...)), ";")))), collapse=";"))
  }
}

#' Event class print method
#'
#' @author Gei Lin
#' @keywords internal
#' @export
print.event <- function(x, ...) {
  str(unclass(x), comp.str="", no.list=TRUE, give.head=FALSE,
      give.length=FALSE, give.attr=FALSE, nest.lev=-1, indent.str="")
  invisible(x)
}

#' Event class sort method
#'
#' @author Gei Lin
#' @keywords internal
#' @export
sort.event <- function(x, decreasing=FALSE, na.last=NA, ...) {
  anchored <- x[c("primary_id", "region",
                  "identifiers", "type")]
  sortable <- x[setdiff(names(x), names(anchored))]
  out <- c(anchored, sortable[order(names(sortable), decreasing=decreasing,
                                    na.last=na.last, ...)])
  class(out) <- class(x)
  out
}

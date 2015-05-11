###############################################################################
# R (http://r-project.org/) Exchange Class Model
#
# Copyright (c) 2015
# Gei Lin, Linnis
#
###############################################################################

.exchange <- new.env(parent=emptyenv())

#' class test for object supposedly of type 'exchange'
#' @param x object to test for type
#' @export
is.exchange <- function( x ) {
  inherits( x, "exchange" )
}

#' check each element of a character vector to see if it is either the
#' primary_id or an identifier of a \code{\link{Exchange}}
#' @param x character vector
#' @export
is.exchange.name <- function( x ) {
  if (!is.character(x)) return(FALSE)
  sapply(lapply(x, getExchange, type='exchange', silent=TRUE), inherits,
         "exchange")
}

#' exchange class constructors
#'
#' All regions and exchanges must be defined before sessions can be created
#'
#' Regions must also be defined -- all exchanges have a region attached including sessions
#' E.g. CBOT is a based in the US region
#'
#' In \dots you may pass any other arbitrary exchange fields that will be used
#' to create 'custom' fields.  S3 classes in \R are basically lists with a class
#' attribute.  We use this to our advantage to allow us to set arbitrary fields.
#'
#' \code{identifiers} should be a named list to specify other identifiers beyond
#' the \code{primary_id}.  Please note that whenever possible, these should
#' still be unique.  Perhaps Bloomberg, Reuters-X.RIC, CUSIP, etc.
#' \code{\link{getEvent}} will return the first (and only the first) match
#' that it finds, starting with the primary_id, and then searching the
#' primary_ids of all exchanges for each of the \code{identifiers}.  Note that
#' when a large number of exchanges are defined, it is faster to find
#' exchanges by \code{primary_id} than by \code{identifiers} because it looks
#' for \code{primary_id}s first.
#'
#' The \code{primary_id} will be coerced within reason to a valid \R variable
#' name by using \code{\link{make.names}}. We also remove any leading '1' digit
#' (a simple workaround to account for issues with the Reuters API).  If you are
#' defining an exchange that is not a \code{session}, with a primary_id that
#' already belongs to a \code{session}, a new primary_id will be create using
#' \code{make.names}.  For example, \code{auction("USD", Region("USD"))}, would
#' create a auction with a primary_id of \dQuote{USD.1} instead of overwritting
#' the \code{session}.
#'
#' Please use some care to choose your primary identifiers so that R won't
#' complain.  If you have better regular expression code, we'd be happy to
#' include it.
#'
#' \code{assign_i} will use \code{\link{assign}} to place the constructed
#' exchange class object into the \code{.exchange} environment.  Most of the
#' special type-specific constructors will use \code{assign_i=TRUE} internally.
#' Calling with \code{assign_i=FALSE}, or not specifying it, will return an
#' object and will \emph{not} store it.  Use this option ether to wrap calls to
#' \code{exchange} prior to further processing (and presumably assignment) or
#' to test your parameters before assignment.
#'
#' If \code{overwrite=FALSE} is used, an error will be thrown if any
#' \code{primary_id}s are already in use.
#'
#' You can (optionally) provide a \code{src} argument in which case, it will be
#' used in a call to \code{\link[quantmod]{setSymbolLookup}}.
#' @param primary_id String describing the unique ID for the exchange. Most
#'   of the wrappers allow this to be a vector.
#' @param ... Any other passthru parameters, including
#' @param exchange String describing the exchange ID of an object of type
#'   \code{\link{exchange}}
#' @param identifiers Named list of any other identifiers that should also be
#'   stored for this exchange
#' @param type exchange type to be appended to the class definition, typically
#'   "continuous" or "broken"
#' @param assign_i TRUE/FALSE. Should the exchange be assigned to the
#'   \code{.exchange} environment?  Default is TRUE for \code{exchange},
#'   TRUE for wrappers.
#' @param overwrite TRUE/FALSE. Should existing exchanges with the same
#'   primary_id be overwritten? Default is TRUE. If FALSE, an error will be
#'   thrown and the exchange will not be created.
#' @aliases
#' Exchange
#' @seealso
#' \code{\link{Exchange}},
#' \code{\link{load.markets}}
#' @export
Exchange <- function(primary_id, region, ..., identifiers = NULL, assign_i = TRUE, overwrite = TRUE) {
	if (is.null(primary_id)) {
		stop("you must specify a primary_id for the exchange")
	}
	if (missing(region) || is.null(region) || 
		(!missing(region) && !is.region.name(region))) {
			stop("region ", region, " must be defined first")
	}
  raw_id <- primary_id
  if (substr(primary_id, 1, 1) == 1) {
    primary_id <- substr(primary_id, 2, nchar(primary_id))
  }
  primary_id <- make.names(primary_id)
  if (missing(region) || is.null(region) || 
        (!missing(region) && !is.region.name(region))) {
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

  tclass = "exchange"

  if ((primary_id %in% ls_exchanges()) && !overwrite &&
        isTRUE(assign_i)) {
    stop(paste("an exchange with primary_id", primary_id,
               "already exists in the .exchange environment.",
               "Set overwrite=TRUE to overwrite."))
  }
  tmpmarket <- list(primary_id = primary_id, region = region,
                   identifiers = identifiers)
  if (length(arg) >= 1) {
    tmpmarket <- c(tmpmarket, arg)
  }

  class(tmpmarket) <- tclass
  if (assign_i) {
    assign(primary_id, tmpmarket, envir = as.environment(.exchange))
    return(primary_id)
  }
  else return(tmpmarket)
}

#' Primary accessor function for getting objects of class 'exchange'
#'
#' This function will search the \code{.exchange} environment for objects of
#' class \code{type}, using first the \code{primary_id} and then any
#' \code{identifiers} to locate the exchange.
#'
#' \code{getRegion} will try using each possible \code{primary_id}
#' until it finds an exchange of the appropriate \code{type}
#' @param x String identifier of exchange to retrieve
#' @param silent if TRUE, will not warn on failure, default FALSE
#' @param type class of object to look for. See Details
#' @examples
#' \dontrun{
#' Exchange('CBOT', region = 'US')
#'
#' getExchange("CBOT", Region("US"))
#' }
#' @export
#' @rdname getExchange
getExchange <- function(x, silent=FALSE, type='exchange'){
  tmp_instr <- try(get(x,pos=.exchange),silent=TRUE)
  if(inherits(tmp_instr,"try-error") || !inherits(tmp_instr, type)){
    xx <- make.names(x)
    ## First, look to see if x matches any identifiers.
    # unlist all exchanges into a big named vector
    ul.instr <- unlist(as.list(.exchange,
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
      tmp_instr <- try(get(id, pos=.exchange),
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
    tmp_instr<-try(get(x,pos=.exchange),silent=TRUE)
    if(!inherits(tmp_instr,type)) {
      tmp_instr<-try(get(paste(".",x,sep=""),
                         pos=.exchange),
                     silent=TRUE)
      if(!inherits(tmp_instr,type)) {
        tmp_instr<-try(get(paste("..",x,sep=""),
                           pos=.exchange),
                       silent=TRUE)
      }
    }
    if (inherits(tmp_instr, type)) return(tmp_instr)
    if(!silent) warning(paste(type,x,"not found, please create it first."))
    return(FALSE)
  } else{
    return(tmp_instr)
  }
}
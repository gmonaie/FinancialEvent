###############################################################################
# R (http://r-project.org/) Market Class Model
#
# Copyright (c) 2015
# Gei Lin, Linnis
#
###############################################################################

.market <- new.env(parent=emptyenv())

#' class test for object supposedly of type 'market'
#' @param x object to test for type
#' @export
is.market <- function( x ) {
  inherits( x, "market" )
}

#' check each element of a character vector to see if it is either the
#' primary_id or an identifier of a \code{\link{market}}
#' @param x character vector
#' @export
is.market.name <- function( x ) {
  if (!is.character(x)) return(FALSE)
  sapply(lapply(x, getMarket, type='market', silent=TRUE), inherits,
         "market")
}

#' class test for object supposedly of type 'session'
#' @param x object to test for type
#' @export
is.session <- function( x ) {
  inherits( x, "session" )
}

#' check each element of a character vector to see if it is either the
#' primary_id or an identifier of a \code{\link{Session}}
#' @param x character vector
#' @export
is.session.name <- function( x ) {
  if (!is.character(x)) return(FALSE)
  sapply(lapply(x, getMarket, type='session', silent=TRUE), inherits,
         "session")
}

#' market class constructors
#'
#' All 'exchange' and 'session' events must be defined before events of other types
#' may be defined.
#'
#' Regions must also be defined -- all events have a region attached including sessions
#' E.g. US.CBTN is a based on the US region
#'
#' In \dots you may pass any other arbitrary market fields that will be used
#' to create 'custom' fields.  S3 classes in \R are basically lists with a class
#' attribute.  We use this to our advantage to allow us to set arbitrary fields.
#'
#' \code{identifiers} should be a named list to specify other identifiers beyond
#' the \code{primary_id}.  Please note that whenever possible, these should
#' still be unique.  Perhaps Bloomberg, Reuters-X.RIC, CUSIP, etc.
#' \code{\link{getEvent}} will return the first (and only the first) match
#' that it finds, starting with the primary_id, and then searching the
#' primary_ids of all markets for each of the \code{identifiers}.  Note that
#' when a large number of markets are defined, it is faster to find
#' markets by \code{primary_id} than by \code{identifiers} because it looks
#' for \code{primary_id}s first.
#'
#' The \code{primary_id} will be coerced within reason to a valid \R variable
#' name by using \code{\link{make.names}}. We also remove any leading '1' digit
#' (a simple workaround to account for issues with the Reuters API).  If you are
#' defining an market that is not a \code{session}, with a primary_id that
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
#' market class object into the \code{.market} environment.  Most of the
#' special type-specific constructors will use \code{assign_i=TRUE} internally.
#' Calling with \code{assign_i=FALSE}, or not specifying it, will return an
#' object and will \emph{not} store it.  Use this option ether to wrap calls to
#' \code{market} prior to further processing (and presumably assignment) or
#' to test your parameters before assignment.
#'
#' If \code{overwrite=FALSE} is used, an error will be thrown if any
#' \code{primary_id}s are already in use.
#'
#' As of version 0.10.0, the .market environment is located at the top level
#' of the package. i.e. \code{.market}.
#'
#' \code{Continuous} and \code{Broken} are used to define the contract specs of a
#' series of markets.  The \code{primary_id} for these can begin with 1 or
#' 2 dots if you need to avoid overwriting another market.
#' For example, if you have a \code{session} with \sQuote{CBTN} as the
#' \code{primary_id}, you could use \sQuote{.CBTN} as the \code{primary_id} of
#' the regular session \code{REG} specs, and \sQuote{..CBTN} as the \code{primary_id} of the
#' electronic session \code{ALL} specs. (or vice versa)
#'
#' You can (optionally) provide a \code{src} argument in which case, it will be
#' used in a call to \code{\link[quantmod]{setSymbolLookup}}.
#' @param primary_id String describing the unique ID for the market. Most
#'   of the wrappers allow this to be a vector.
#' @param ... Any other passthru parameters, including
#' @param exchange String describing the exchange ID of an object of type
#'   \code{\link{exchange}}
#' @param identifiers Named list of any other identifiers that should also be
#'   stored for this market
#' @param type market type to be appended to the class definition, typically
#'   "continuous" or "broken"
#' @param assign_i TRUE/FALSE. Should the market be assigned to the
#'   \code{.market} environment?  Default is FALSE for \code{market},
#'   TRUE for wrappers.
#' @param overwrite TRUE/FALSE. Should existing markets with the same
#'   primary_id be overwritten? Default is TRUE. If FALSE, an error will be
#'   thrown and the market will not be created.
#' @aliases
#' Session
#' Exchange
#' Region
#' @seealso
#' \code{\link{Continuous}},
#' \code{\link{Broken}},
#' \code{\link{MarketSession}},
#' \code{\link{load.markets}}
#' @export
Market <- function(primary_id, ..., exchange, identifiers = NULL, type = NULL, assign_i = FALSE, overwrite = TRUE) {
	if (is.null(primary_id)) {
		stop("you must specify a primary_id for the market")
	}

	if (missing(exchange) || is.null(exchange) || 
		(!missing(exchange) && !is.exchange.name(exchange))) {
			stop("exchange ", exchange, " must be defined first")
	}

  raw_id <- primary_id
  if (substr(primary_id, 1, 1) == 1) {
    primary_id <- substr(primary_id, 2, nchar(primary_id))
  }
  primary_id <- make.names(primary_id)

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

  if (is.null(type)) {
    tclass = "market"
  }
  else tclass = unique(c(type, "market"))
  if ((primary_id %in% ls_markets()) && !overwrite &&
        isTRUE(assign_i)) {
    stop(paste("an market with primary_id", primary_id,
               "already exists in the .market environment.",
               "Set overwrite=TRUE to overwrite."))
  }
  tmpmarket <- list(primary_id = primary_id,
  					exchange = exchange,
                   identifiers = identifiers,
                   type = type)
  if (length(arg) >= 1) {
    tmpmarket <- c(tmpmarket, arg)
  }

  class(tmpmarket) <- tclass
  if (assign_i) {
    assign(primary_id, tmpmarket, envir = as.environment(.market))
    return(primary_id)
  }
  else return(tmpmarket)
}

#' @export
#' @rdname Market
Continuous <- function(primary_id, exchange, 
                   identifiers = NULL, assign_i=TRUE, overwrite=TRUE, ...){
    if (length(primary_id) > 1) stop('primary_id must be of length 1')
    if (!isTRUE(overwrite) && assign_i==TRUE && 
            primary_id %in% ls_instruments()) {
        stop(sQuote(primary_id), " already in use and overwrite=FALSE")
    }
    if (missing(exchange)) {
		stop("'exchange' is a required argument")
    }

    Market(primary_id=primary_id, exchange=exchange, identifiers = identifiers, ... , 
               type="continuous", assign_i=assign_i)
}

#' @export
#' @rdname Market
Broken <- function(primary_id, exchange, 
                   identifiers = NULL, assign_i=TRUE, overwrite=TRUE, ...){
    if (length(primary_id) > 1) stop('primary_id must be of length 1')
    if (!isTRUE(overwrite) && assign_i==TRUE && 
            primary_id %in% ls_instruments()) {
        stop(sQuote(primary_id), " already in use and overwrite=FALSE")
    }
    if (missing(exchange)) {
		stop("'exchange' is a required argument")
    }

    Market(primary_id=primary_id, exchange=exchange, identifiers = identifiers, ... , 
               type="broken", assign_i=assign_i)
}

#' Constructors for market sessions
#' 
#' Constructors for sessions on markets such as ALL, REG, PRE, and POST
#'
#' The root \code{instrument} (e.g. the \code{future} or \code{option}) must be
#' defined first.
#'
#' In custom parameters for these series contracts, we have often found it
#' useful to store attributes such as local roll-on and roll-off dates
#' (rolling not on the \code{first_listed} or \code{expires}.  
#'
#' For \code{future_series} and \code{option_series} you may either provide a 
#'   \code{primary_id} (or vector of \code{primary_id}s), 
#' OR both a \code{root_id} and \code{suffix_id}.
#'
#' Note that the code for \code{bond} and \code{bond_series} has not been 
#' updated recently and may not support all the features supported for
#' \code{option_series} and \code{future_series}.  Patches welcome.
#'
#' @param primary_id String describing the unique ID for the instrument. May be 
#'   a vector for \code{future_series} and \code{option_series}
#' @param root_id String product code or underlying_id, usually something like 
#'   'ES' or 'CL' for futures, or the underlying stock symbol (maybe preceded 
#'   with a dot) for equity options.
#' @param suffix_id String suffix that should be associated with the series, 
#'   usually something like 'Z9' or 'Mar10' denoting expiration and year.
#' @param first_traded String coercible to Date for first trading day.
#' @param expires String coercible to Date for expiration date
#' @param maturity String coercible to Date for maturity date of bond series.
#' @param callput Right of option; call or put
#' @param strike Strike price of option
#' @param payment_schedule Not currently being implemented
#' @param identifiers Named list of any other identifiers that should also be 
#'   stored for this instrument.
#' @param assign_i TRUE/FALSE. Should the instrument be assigned in the 
#'   \code{.instrument} environment?
#' @param overwrite TRUE/FALSE. If FALSE, only \code{first_traded} and 
#'   \code{expires} will be updated.
#' @param ... any other passthru parameters
#' @aliases 
#' option_series
#' future_series
#' bond_series
#' @examples
#' \dontrun{
#' currency("USD")
#' future("ES","USD",multiplier=50, tick_size=0.25)
#' MarketSession('ES_U1')
#' MarketSession(root_id='ES',suffix_id='Z11')
#' stock('SPY','USD')
#' option('.SPY','USD',multiplier=100,underlying_id='SPY')
#' #can use either .SPY or SPY for the root_id. 
#' #it will find the one that is option specs.
#' option_series('SPY_110917C125', expires='2011-09-16')
#' option_series(root_id='SPY',suffix_id='111022P125')
#' option_series(root_id='.SPY',suffix_id='111119C130')
#' #multiple series instruments at once.
#' continuous_session(c("ES_H12","ES_M12"))
#' broken_session(c("SPY_110917C115","SPY_110917P115"))
#' ERXB_ALL
#' ERXB_REG
#' CBTN_ALL
#' CBTN_REG
#' _PRE
#' _POST
#' _REG
#' _ALL
#' }
#' @export
#' @rdname Session
Session <- function(primary_id, market_id=NULL, suffix_id=NULL, 
                          identifiers = NULL, 
                          assign_i=TRUE, overwrite=TRUE, ...){
  # if overwrite==FALSE and assign_i==TRUE, we'll need to know what instruments
  # are already defined.  Don't bother doing this if we're overwriting anyway
  if (!isTRUE(overwrite) && isTRUE(assign_i)) li <- ls_markets()
  if (missing(primary_id)) {
      if (is.null(market_id) || is.null(suffix_id)) {
          stop(paste('must provide either a primary_id or',
                     'both a market_id and a suffix_id'))
      } else {
          primary_id <- paste(gsub("\\.","",market_id), suffix_id, sep="_")
      }
  } else if (length(primary_id) > 1) {
      if (!isTRUE(overwrite) && isTRUE(assign_i) &&
          any(in.use <- primary_id %in% li)) {
          stop(paste(paste("In Session(...) : ",
                            "overwrite is FALSE and primary_id", 
                            if (sum(in.use) > 1) "s are" else " is", 
                            " already in use:\n", sep=""),
                   paste(intersect(primary_id, li), collapse=", ")), 
               call.=FALSE)
      }
      out <- sapply(primary_id, MarketSession, market_id=market_id, 
                    suffix_id=suffix_id, identifiers = identifiers, 
                    assign_i=assign_i, ...=..., simplify=assign_i)
      return(if (assign_i) unname(out) else out)
  }
  if (!isTRUE(overwrite) && isTRUE(assign_i) && primary_id %in% li) {
      stop(sQuote(primary_id), " already in use and overwrite=FALSE")
  }

  tmpid <- unlist(strsplit(x = "CBTN_ALL", split = "_"))

  pid <- NULL
  pid$market <- tmpid[1]
  pid$suffix <- tmpid[2]

  if (is.null(market_id)) market_id <- pid$market
  if (is.null(suffix_id)) suffix_id <- pid$suffix

  contract <- getMarket(market_id,type='market')

  if (!isTRUE(overwrite)) {
      temp_series<-try(getMarket(primary_id, silent=TRUE),silent=TRUE)
      if(inherits(temp_series,"session")) {
          message("updating existing first_traded and expires for ",primary_id)
          assign(primary_id, temp_series, 
                 envir=as.environment(.market))
          return(primary_id)
      } else warning("No contract found to update. A new one will be created.")
  }
  args <- list()
  args$primary_id <- primary_id
  args$market_id <- market_id
  args$suffix_id=suffix_id
  args$exchange = contract$exchange
  args$identifiers = identifiers
  args$type=c("session", "market")
  args$assign_i=assign_i
  dargs<-list(...)
  dargs$type=NULL
  if (is.null(dargs$src) && !is.null(contract$src)){
      dargs$src <- contract$src
  }
  args <- c(args, dargs)

  do.call(Market, args)
}

#' Primary accessor function for getting objects of class 'market'
#'
#' This function will search the \code{.market} environment for objects of
#' class \code{type}, using first the \code{primary_id} and then any
#' \code{identifiers} to locate the market.  Finally, it will try adding 1
#' and then 2 dots to the beginning of the \code{primary_id} to see if an
#' market was stored there to avoid naming conflicts.
#'
#' \code{\link{future}} and \code{\link{option}} objects may have a primary_id
#' that begins with 1 or 2 dots (in order to avoid naming conflics).  For
#' example, the root specs for options (or futures) on the auction with ticker
#' "SPY" may be stored with a primary_id of "SPY", ".SPY", or "..SPY".
#' \code{getEvent} will try using each possible \code{primary_id}
#' until it finds an market of the appropriate \code{type}
#' @param x String identifier of market to retrieve
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
#' @rdname getMarket
getMarket <- function(x, Dates=NULL, silent=FALSE, type='market'){
  tmp_instr <- try(get(x,pos=.market),silent=TRUE)
  if(inherits(tmp_instr,"try-error") || !inherits(tmp_instr, type)){
    xx <- make.names(x)
    ## First, look to see if x matches any identifiers.
    # unlist all markets into a big named vector
    ul.instr <- unlist(as.list(.market,
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
      tmp_instr <- try(get(id, pos=.market),
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
    tmp_instr<-try(get(x,pos=.market),silent=TRUE)
    if(!inherits(tmp_instr,type)) {
      tmp_instr<-try(get(paste(".",x,sep=""),
                         pos=.market),
                     silent=TRUE)
      if(!inherits(tmp_instr,type)) {
        tmp_instr<-try(get(paste("..",x,sep=""),
                           pos=.market),
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
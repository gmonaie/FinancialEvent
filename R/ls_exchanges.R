###############################################################################
# R (http://r-project.org/) Exchange Class Model
#
# Copyright (c) 2015
# Gei Lin
#
# $Id$
#
###############################################################################

#' List or Remove exchange objects
#'
#' display the names of or delete exchanges, auctions, options, futures,
#' exchanges, bonds, funds, spreads, guaranteed_spreads, synthetics,
#' derivatives, or non-derivatives.
#'
#' ls functions return the names of all the exchanges of the class implied by
#' the function name. rm functions remove the exchanges of the class implied
#' by the function name
#'
#' rm_exchanges and rm_non_derivatives will not delete exchanges unless the
#' keep.exchanges argument is FALSE.
#'
#' For the rm functions, x can be a vector of exchange names, or nothing.  If
#' \code{x} is missing, all exchanges of the relevant type will be removed.
#'
#' It can be useful to nest these functions to get things like futures
#' denominated in USD.
#'
#' @aliases ls_exchanges rm_exchanges
#' @param pattern an optional regular expression.  Only names matching
#' \sQuote{pattern} are returned.
#' @param match return only exact matches?
#' @param verbose be verbose?
#' @param x what to remove. if not supplied all exchanges of relevent class
#' will be removed.  For \code{ls_defined.by} x is the string describing how the
#' exchange was defined.
#' @return ls functions return vector of character strings corresponding to
#' exchanges of requested type rm functions are called for side-effect
#' @author Gei Lin
#' @seealso ls, rm, Exchange
#' @examples
#'
#' \dontrun{
#'
#' # First, create some exchanges
#' Exchange(c("NYSE", "COMEX", "ICE", "CBOT"))
#'
#' # Now, the examples
#' ls_exchanges() #all exchanges
#' rm_exchanges()
#' }
#' @export
#' @rdname ls_exchanges
ls_exchanges <- function(pattern=NULL, match=TRUE, verbose=TRUE) {
  if (length(pattern) > 1 && !match) {
    if (verbose)
      warning("Using match=TRUE because length of pattern > 1.")
    #should I use match?
    #or, ignore pattern and return everything?
    #or, do multiple ls calls and return unique
    match <- TRUE
  }
  if (!is.null(pattern) && match) {   #there's a pattern and match is TRUE
    symbols <- ls(.exchange, all.names=TRUE)
    symbols <- symbols[match(pattern,symbols)]
  } else if (!match && length(pattern) == 1) { # pattern is length(1) and don't match
    symbols <- ls(.exchange, all.names=TRUE, pattern=pattern)
  } else if (is.null(pattern)) {  #no pattern
    symbols <- ls(.exchange, all.names=TRUE)
  } # else pattern length > 1 & don't match

  is.iname <- is.exchange.name(symbols)
  if (!any(is.iname)) return(NULL)
  symbols[is.iname]
}

#' @export
#' @rdname ls_markets
rm_exchanges <- function(x) {
  if (missing(x)) {
    x <- ls_exchanges()
  }
  rm(list=x[x %in% ls_exchanges()], pos=.exchange)
}

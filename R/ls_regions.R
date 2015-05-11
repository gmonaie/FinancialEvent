###############################################################################
# R (http://r-project.org/) Region Class Model
#
# Copyright (c) 2015
# Gei Lin
#
# $Id$
#
###############################################################################

#' List or Remove region objects
#'
#' display the names of or delete regions, auctions, options, futures,
#' regions, bonds, funds, spreads, guaranteed_spreads, synthetics,
#' derivatives, or non-derivatives.
#'
#' ls functions return the names of all the regions of the class implied by
#' the function name. rm functions remove the regions of the class implied
#' by the function name
#'
#' rm_regions and rm_non_derivatives will not delete regions unless the
#' keep.regions argument is FALSE.
#'
#' For the rm functions, x can be a vector of region names, or nothing.  If
#' \code{x} is missing, all regions of the relevant type will be removed.
#'
#' It can be useful to nest these functions to get things like futures
#' denominated in USD.
#'
#' @aliases ls_regions rm_regions
#' @param pattern an optional regular expression.  Only names matching
#' \sQuote{pattern} are returned.
#' @param match return only exact matches?
#' @param verbose be verbose?
#' @param x what to remove. if not supplied all regions of relevent class
#' will be removed.  For \code{ls_defined.by} x is the string describing how the
#' region was defined.
#' @return ls functions return vector of character strings corresponding to
#' regions of requested type rm functions are called for side-effect
#' @author Gei Lin
#' @seealso ls, rm, Region
#' @examples
#'
#' \dontrun{
#'
#' # First, create some regions
#' Region(c("US", "EU", "JP", "FR"))
#'
#' # Now, the examples
#' ls_regions() #all regions
#' rm_regions()
#' }
#' @export
#' @rdname ls_regions
ls_regions <- function(pattern=NULL, match=TRUE, verbose=TRUE) {
  if (length(pattern) > 1 && !match) {
    if (verbose)
      warning("Using match=TRUE because length of pattern > 1.")
    #should I use match?
    #or, ignore pattern and return everything?
    #or, do multiple ls calls and return unique
    match <- TRUE
  }
  if (!is.null(pattern) && match) {   #there's a pattern and match is TRUE
    symbols <- ls(.region, all.names=TRUE)
    symbols <- symbols[match(pattern,symbols)]
  } else if (!match && length(pattern) == 1) { # pattern is length(1) and don't match
    symbols <- ls(.region, all.names=TRUE, pattern=pattern)
  } else if (is.null(pattern)) {  #no pattern
    symbols <- ls(.region, all.names=TRUE)
  } # else pattern length > 1 & don't match

  is.iname <- is.region.name(symbols)
  if (!any(is.iname)) return(NULL)
  symbols[is.iname]
}

#' @export
#' @rdname ls_markets
rm_regions <- function(x) {
  if (missing(x)) {
    x <- ls_regions()
  }
  rm(list=x[x %in% ls_regions()], pos=.region)
}

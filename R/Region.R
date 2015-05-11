###############################################################################
# R (http://r-project.org/) Exchange Class Model
#
# Copyright (c) 2015
# Gei Lin, Linnis
#
###############################################################################

.region <- new.env(parent=emptyenv())

#' class test for object supposedly of type 'region'
#' @param x object to test for type
#' @export
is.region <- function( x ) {
  inherits( x, "region" )
}

#' check each element of a character vector to see if it is either the
#' primary_id or an identifier of a \code{\link{Region}}
#' @param x character vector
#' @export
is.region.name <- function( x ) {
  if (!is.character(x)) return(FALSE)
  sapply(lapply(x, getRegion, type='region', silent=TRUE), inherits,
         "region")
}

#' region class constructors
#'
#' All regions must be defined before markets can be created
#'
#' All exchanges have a region attached including sessions
#' E.g. CBOT is a based in the US region
#'
#' \code{\link{getRegion}} will return the first (and only the first) match
#' that it finds, starting with the primary_id, and then searching the
#' primary_ids of all exchanges for each of the \code{identifiers}.  Note that
#' when a large number of exchanges are defined, it is faster to find
#' exchanges by \code{primary_id} than by \code{identifiers} because it looks
#' for \code{primary_id}s first.
#'
#' The \code{primary_id} will be coerced within reason to a valid \R variable
#' name by using \code{\link{make.names}}.
#'
#' Please use some care to choose your primary identifiers so that R won't
#' complain.  If you have better regular expression code, we'd be happy to
#' include it.
#'
#' \code{assign_i} will use \code{\link{assign}} to place the constructed
#' region class object into the \code{.region} environment.  Most of the
#' special type-specific constructors will use \code{assign_i=TRUE} internally.
#' Calling with \code{assign_i=FALSE}, or not specifying it, will return an
#' object and will \emph{not} store it.  Use this option ether to wrap calls to
#' \code{region} prior to further processing (and presumably assignment) or
#' to test your parameters before assignment.
#'
#' If \code{overwrite=FALSE} is used, an error will be thrown if any
#' \code{primary_id}s are already in use.
#'
#' @param primary_id String describing the unique ID for the region. Most
#'   of the wrappers allow this to be a vector.
#' @param assign_i TRUE/FALSE. Should the region be assigned to the
#'   \code{.region} environment?  Default is TRUE for \code{Region}, and
#'   TRUE for wrappers.
#' @param overwrite TRUE/FALSE. Should existing regions with the same
#'   primary_id be overwritten? Default is TRUE. If FALSE, an error will be
#'   thrown and the region will not be created.
#' @aliases
#' Region
#' @seealso
#' \code{\link{Region}},
#' \code{\link{load.regions}}
#' @export
Region <- function(primary_id, assign_i = TRUE, overwrite = TRUE) {
	if (is.null(primary_id)) {
		stop("you must specify a primary_id for the region")
	}
  raw_id <- primary_id
  primary_id <- make.names(primary_id)
 
  tclass = "region"

  if ((primary_id %in% ls_regions()) && !overwrite &&
        isTRUE(assign_i)) {
    stop(paste("an region with primary_id", primary_id,
               "already exists in the .region environment.",
               "Set overwrite=TRUE to overwrite."))
  }
  tmpmarket <- list(primary_id = primary_id)
  class(tmpmarket) <- tclass
  if (assign_i) {
    assign(primary_id, tmpmarket, envir = as.environment(.region))
    return(primary_id)
  }
  else return(tmpmarket)
}

#' Primary accessor function for getting objects of class 'region'
#'
#' This function will search the \code{.region} environment for objects of
#' class \code{type}, using first the \code{primary_id} and then any
#' \code{identifiers} to locate the region.
#'
#' \code{getRegion} will try using each possible \code{primary_id}
#' until it finds an region of the appropriate \code{type}
#' @param x String identifier of region to retrieve
#' @param silent if TRUE, will not warn on failure, default FALSE
#' @param type class of object to look for. See Details
#' @examples
#' \dontrun{
#' Region('US')
#'
#' getRegion("US")
#' getRegion('EU')
#' }
#' @export
#' @rdname getRegion
getRegion <- function(x, silent=FALSE, type='region'){
  tmp_instr <- try(get(x,pos=.region),silent=TRUE)
  if(inherits(tmp_instr,"try-error") || !inherits(tmp_instr, type)){
    xx <- make.names(x)
    ## First, look to see if x matches any identifiers.
    # unlist all regions into a big named vector
    ul.instr <- unlist(as.list(.region,
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
      tmp_instr <- try(get(id, pos=.region),
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
    tmp_instr<-try(get(x,pos=.region),silent=TRUE)
    if(!inherits(tmp_instr,type)) {
      tmp_instr<-try(get(paste(".",x,sep=""),
                         pos=.region),
                     silent=TRUE)
      if(!inherits(tmp_instr,type)) {
        tmp_instr<-try(get(paste("..",x,sep=""),
                           pos=.region),
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
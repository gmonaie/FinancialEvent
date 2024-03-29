% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Exchange.R
\name{Exchange}
\alias{Exchange}
\title{exchange class constructors}
\usage{
Exchange(primary_id, region, ..., identifiers = NULL, assign_i = TRUE,
  overwrite = TRUE)
}
\arguments{
\item{primary_id}{String describing the unique ID for the exchange. Most
of the wrappers allow this to be a vector.}

\item{...}{Any other passthru parameters, including}

\item{identifiers}{Named list of any other identifiers that should also be
stored for this exchange}

\item{assign_i}{TRUE/FALSE. Should the exchange be assigned to the
\code{.exchange} environment?  Default is TRUE for \code{exchange},
TRUE for wrappers.}

\item{overwrite}{TRUE/FALSE. Should existing exchanges with the same
primary_id be overwritten? Default is TRUE. If FALSE, an error will be
thrown and the exchange will not be created.}

\item{exchange}{String describing the exchange ID of an object of type
\code{\link{exchange}}}

\item{type}{exchange type to be appended to the class definition, typically
"continuous" or "broken"}
}
\description{
All regions and exchanges must be defined before sessions can be created
}
\details{
Regions must also be defined -- all exchanges have a region attached including sessions
E.g. CBOT is a based in the US region

In \dots you may pass any other arbitrary exchange fields that will be used
to create 'custom' fields.  S3 classes in \R are basically lists with a class
attribute.  We use this to our advantage to allow us to set arbitrary fields.

\code{identifiers} should be a named list to specify other identifiers beyond
the \code{primary_id}.  Please note that whenever possible, these should
still be unique.  Perhaps Bloomberg, Reuters-X.RIC, CUSIP, etc.
\code{\link{getEvent}} will return the first (and only the first) match
that it finds, starting with the primary_id, and then searching the
primary_ids of all exchanges for each of the \code{identifiers}.  Note that
when a large number of exchanges are defined, it is faster to find
exchanges by \code{primary_id} than by \code{identifiers} because it looks
for \code{primary_id}s first.

The \code{primary_id} will be coerced within reason to a valid \R variable
name by using \code{\link{make.names}}. We also remove any leading '1' digit
(a simple workaround to account for issues with the Reuters API).  If you are
defining an exchange that is not a \code{session}, with a primary_id that
already belongs to a \code{session}, a new primary_id will be create using
\code{make.names}.  For example, \code{auction("USD", Region("USD"))}, would
create a auction with a primary_id of \dQuote{USD.1} instead of overwritting
the \code{session}.

Please use some care to choose your primary identifiers so that R won't
complain.  If you have better regular expression code, we'd be happy to
include it.

\code{assign_i} will use \code{\link{assign}} to place the constructed
exchange class object into the \code{.exchange} environment.  Most of the
special type-specific constructors will use \code{assign_i=TRUE} internally.
Calling with \code{assign_i=FALSE}, or not specifying it, will return an
object and will \emph{not} store it.  Use this option ether to wrap calls to
\code{exchange} prior to further processing (and presumably assignment) or
to test your parameters before assignment.

If \code{overwrite=FALSE} is used, an error will be thrown if any
\code{primary_id}s are already in use.

You can (optionally) provide a \code{src} argument in which case, it will be
used in a call to \code{\link[quantmod]{setSymbolLookup}}.
}
\seealso{
\code{\link{Exchange}},
\code{\link{load.markets}}
}


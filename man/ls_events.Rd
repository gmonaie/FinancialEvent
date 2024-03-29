% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/ls_eventintervals.R, R/ls_events.R, R/ls_markets.R
\name{ls_eventintervals}
\alias{ls_FX}
\alias{ls_auctions}
\alias{ls_bonds}
\alias{ls_breaks}
\alias{ls_calls}
\alias{ls_concessions}
\alias{ls_derivatives}
\alias{ls_ecodata}
\alias{ls_eventintervals}
\alias{ls_events}
\alias{ls_exchange_rates}
\alias{ls_funds}
\alias{ls_future_series}
\alias{ls_futures}
\alias{ls_guaranteed_spreads}
\alias{ls_non_derivatives}
\alias{ls_non_regions}
\alias{ls_opencloses}
\alias{ls_option_series}
\alias{ls_options}
\alias{ls_policies}
\alias{ls_puts}
\alias{ls_regions}
\alias{ls_settle2settles}
\alias{ls_spreads}
\alias{ls_synthetics}
\alias{rm_FX}
\alias{rm_auctions}
\alias{rm_bonds}
\alias{rm_breaks}
\alias{rm_concessions}
\alias{rm_derivatives}
\alias{rm_ecodata}
\alias{rm_eventintervals}
\alias{rm_events}
\alias{rm_exchange_rates}
\alias{rm_funds}
\alias{rm_future_series}
\alias{rm_futures}
\alias{rm_markets}
\alias{rm_non_derivatives}
\alias{rm_opencloses}
\alias{rm_option_series}
\alias{rm_options}
\alias{rm_policies}
\alias{rm_regions}
\alias{rm_spreads}
\alias{rm_synthetics}
\title{List or Remove event objects}
\usage{
ls_eventintervals(pattern = NULL, match = TRUE)

ls_concessions(pattern = NULL, match = TRUE)

ls_breaks(pattern = NULL, match = TRUE)

ls_settle2settles(pattern = NULL, match = TRUE)

rm_eventintervals(x)

rm_concessions(x)

rm_breaks(x)

ls_events(pattern = NULL, match = TRUE, verbose = TRUE,
  includeEI = FALSE)

ls_auctions(pattern = NULL, match = TRUE)

ls_regions(pattern = NULL, match = TRUE, verbose = TRUE)

ls_non_regions(pattern = NULL, match = TRUE, includeFX = TRUE)

ls_policies(pattern = NULL, match = TRUE)

ls_ecodata(pattern = NULL, match = TRUE)

ls_opencloses(pattern = NULL, match = TRUE)

rm_events(x, keep.regions = TRUE)

rm_auctions(x)

rm_policies(x)

rm_ecodata(x)

rm_opencloses(x)

rm_markets(x, keep.regions = TRUE)
}
\arguments{
\item{pattern}{an optional regular expression.  Only names matching
\sQuote{pattern} are returned.}

\item{match}{return only exact matches?}

\item{x}{what to remove. if not supplied all events of relevent class
will be removed.  For \code{ls_defined.by} x is the string describing how the
event was defined.}

\item{verbose}{be verbose?}

\item{includeFX}{should exchange_rates be included in ls_non_regions
results}

\item{keep.regions}{If TRUE, regions will not be deleted.}

\item{include.series}{should future_series or option_series events be
included.}
}
\value{
ls functions return vector of character strings corresponding to
events of requested type rm functions are called for side-effect
}
\description{
display the names of or delete events, auctions, options, futures,
regions, bonds, funds, spreads, guaranteed_spreads, synthetics,
derivatives, or non-derivatives.
}
\details{
ls functions return the names of all the events of the class implied by
the function name. rm functions remove the events of the class implied
by the function name

rm_events and rm_non_derivatives will not delete regions unless the
keep.regions argument is FALSE.

For the rm functions, x can be a vector of event names, or nothing.  If
\code{x} is missing, all events of the relevant type will be removed.

It can be useful to nest these functions to get things like futures
denominated in USD.
}
\examples{
\dontrun{
#rm_events(keep.regions=FALSE) #remove everything from .event

# First, create some events
region(c("USD", "EUR", "JPY"))
#auctions
auction(c("S", "SE", "SEE", "SPY"), 'USD')
synthetic("SPX", "USD", src=list(src='yahoo', name='^GSPC'))
#derivatives
option('.SPY', 'USD', multiplier=100, underlying_id='SPY')
option_series(root_id="SPY", expires='2011-06-18', callput='put', strike=130)
option_series(root_id="SPY", expires='2011-09-17', callput='put', strike=130)
option_series(root_id="SPY", expires='2011-06-18', callput='call', strike=130)
future('ES', 'USD', multiplier=50, expires='2011-09-16', underlying_id="SPX")
option('.ES','USD',multiplier=1, expires='2011-06',strike=1350, right='C', underlying_id='ES')

# Now, the examples
ls_events() #all events
ls_events("SE") #only the one auction
ls_events("S", match=FALSE) #anything with "S" in name

ls_regions()
ls_auctions()
ls_options()
ls_futures()
ls_derivatives()
ls_puts()
ls_non_derivatives()
#ls_by_expiry('20110618',ls_puts()) #put options that expire on Jun 18th, 2011
#ls_puts(ls_by_expiry('20110618')) #same thing

rm_options('SPY_110618C130')
rm_futures()
ls_events()
#rm_events('EUR') #Incorrect
rm_events('EUR', keep.regions=FALSE) #remove the region
rm_regions('JPY') #or remove region like this
ls_regions()
ls_events()

rm_events() #remove all but regions
rm_regions()

option_series.yahoo('DIA')
ls_events_by('underlying_id','DIA') #underlying_id must exactly match 'DIA'
ls_derivatives('DIA',match=FALSE) #primary_ids that contain 'DIA'
rm_events()
}
}
\author{
Garrett See
}
\seealso{
ls_events_by, ls_by_region, ls_by_expiry, ls, rm,
event, auction, future, option, region, FinancialInstrument::sort_ids
}


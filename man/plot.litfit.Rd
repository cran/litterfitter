% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output-fxns.R
\name{plot.litfit}
\alias{plot.litfit}
\title{Plot decomposition trajectory and curve fit}
\usage{
\method{plot}{litfit}(x,formulae.cex,...)
}
\arguments{
\item{x}{litfit object}

\item{formulae.cex}{how big do you want your formula?}

\item{...}{additional arguments passed to plot.default}
}
\value{
Plot of litfit object, returns invisibly
}
\description{
Plot a fit of a curve to a litter decomposition trajectory
}
\details{
The data and the line fit plotted from a litFit object. 
 Designed to give a quick visual check if model fitting is adequate.
}
\examples{
fit <- fit_litter(
time=c(0,1,2,3,4,5,6),
mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),
'neg.exp',
iters=250
)

plot(fit)
}
\seealso{
\code{\link{fit_litter}}
}
\author{
Will Cornwell
}

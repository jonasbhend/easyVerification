% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ens2AFC.R
\name{Ens2AFC}
\alias{Ens2AFC}
\alias{rank.ensembles}
\title{Generalized Discrimination Score}
\usage{
Ens2AFC(ens, obs, ...)

rank.ensembles(ens)
}
\arguments{
\item{ens}{n x m matrix of n forecasts for m ensemble members}

\item{obs}{vector of n verifying observations}

\item{...}{additional arguments not used in function (for compatibility)}
}
\description{
Computes the generalized discrimination score for ensemble
forecasts after (Weigel and Mason, 2011).
}
\details{
This function computes the generalized discrimination score for
ensemble forecasts with continuous observations as described in Weigel and
Mason (2011).
}
\examples{
tm <- toymodel()
Ens2AFC(tm$fcst, tm$obs)

}
\references{
Weigel, A.P., and S.J. Mason (2011). The Generalized
Discrimination Score for Ensemble Forecasts. Monthly Weather Review, 139(9),
3069-3074. doi:10.1175/MWR-D-10-05069.1
}
\seealso{
\code{\link{veriApply}}
}

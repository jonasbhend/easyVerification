% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toymodel.R
\name{toymodel}
\alias{toymodel}
\alias{toyarray}
\title{Create Example Forecast-Observation Pairs}
\usage{
toymodel(N = 35, nens = 51, alpha = 0.5, beta = 0)

toyarray(dims = c(10, 5), ...)
}
\arguments{
\item{N}{number of forecast instances}

\item{nens}{number of ensemble members}

\item{alpha}{nominal correlation skill of forecasts}

\item{beta}{overconfidence parameter (see details)}

\item{dims}{independent (e.g. spatial) dimensions for the toy model}

\item{...}{additional arguments passed to \code{\link{toymodel}}}
}
\description{
This toy model lets you create forecast-observation pairs with specified
ensemble and forecast size, correlation skill, and overconfidence
(underdispersion) for application with the verification functionality
provided as part of the easyVerification package.
}
\details{
The toy model is the TM2 model as introduced by Weigel and Bowler
(2009) with a slight modification to allow for forecasts with negative
correlation skill. In this toy model, the observations \eqn{x} and forecasts
\eqn{f_i} are defined as follows:

\deqn{x = \mu_x + \epsilon_x}
\deqn{f_i = \alpha / |\alpha| \mu_x + \epsilon_{\beta} + \epsilon_i}
where
\deqn{\mu_x ~ N(0, \alpha^2)}
\deqn{\epsilon_x ~ N(0, 1 - \alpha^2)}
\deqn{\epsilon_{\beta} ~ N(0, \beta^2)}
\deqn{\epsilon_i ~ N(0, 1 - \alpha^2 - \beta^2)}
\deqn{\alpha^2 \le 1}
\deqn{0 \le \beta \le 1 - \alpha^2}
}
\note{
This toy model is intended to provide example forecast observation
pairs and not to serve as a conceptual model to study real forecasts. For
models to do the latter, please refer to Siegert et al. (2015).
}
\examples{
## compute the correlation for a toy forecast with default parameters
tm <- toyarray()
f.corr <- veriApply("EnsCorr", fcst = tm$fcst, obs = tm$obs)

}
\references{
A. Weigel and N. Bowler (2009). Comment on 'Can multi-model
combination really enhance the prediction skill of probabilistic ensemble
forecasts?'. \emph{Quarterly Journal of the Royal Meteorological Society},
135, 535-539.

S. Siegert \emph{et al.} (2015). A Bayesian framework for verification and
recalibration of ensemble forecasts: How uncertain is NAO predictability?
Preprint on ArXiv, \url{https://arxiv.org/abs/1504.01933}.
}
\keyword{utilities}

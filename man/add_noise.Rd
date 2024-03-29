% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_noise.R
\name{add_noise}
\alias{add_noise}
\title{Add noise to antigenic distances}
\usage{
add_noise(
  dists,
  titre_noise_rdistribution = stats::rnorm,
  titre_noise_param = c(0, 1),
  antigen_noise_rdistribution = stats::rnorm,
  antigen_noise_param = c(0, 1),
  serum_noise_rdistribution = stats::rnorm,
  serum_noise_param = c(0, 1),
  seed
)
}
\arguments{
\item{dists}{A matrix of antigenic distances}

\item{titre_noise_rdistribution}{Random distribution function for tire noise}

\item{titre_noise_param}{Arguments for the titre_noise_rdistribution function}

\item{antigen_noise_rdistribution}{Random distribution function for antigen noise}

\item{antigen_noise_param}{Arguments for the antigen_noise_rdistribution function}

\item{serum_noise_rdistribution}{Random distribution function for serum noise}

\item{serum_noise_param}{Arguments for the serum_noise_rdistribution function}

\item{seed}{Random seed}
}
\value{
list
}
\description{
Adds up to three different types of noise to the antigenic distances: per titre, per antigen and per serum. The default noise distributions are normal, but can be
}
\examples{
m <- map_maker_random(5, 5, 10)
noise_dists <- add_noise(m$dist)
}

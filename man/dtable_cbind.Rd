% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtable-bind_order_prune_subset.R
\name{dtable_cbind}
\alias{dtable_cbind}
\title{cbind for dtables}
\usage{
dtable_cbind(x, y, groups = NULL)
}
\arguments{
\item{x}{object 1}

\item{y}{object 2}

\item{groups}{add meta info to the groups}
}
\description{
cbind dtables
}
\details{
wrapper for \code{cbind} which keeps dtable attributes sane
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtable-list_latex.R, R/dtable.R
\name{dtable_list}
\alias{dtable_list}
\alias{dtable}
\title{create descriptive table}
\usage{
dtable_list(data, types, ...)

dtable(
  data,
  type = NULL,
  guide = NULL,
  desc = NULL,
  desc.flist = NULL,
  comp = NULL,
  comp.flist = NULL,
  test = NULL,
  test.flist = NULL,
  glist = NULL,
  w = NULL,
  useNA = "ifany",
  ...
)
}
\arguments{
\item{data}{a \code{data.frame} or such object}

\item{types}{a vector of the types wanted}

\item{...}{arguments passed}

\item{type}{what kind of variables to you want to describe? 'real', 'bnry',
'cat', 'date' and 'surv' are available}

\item{guide}{point to guide or get the default}

\item{desc}{if, and how, to describe variables}

\item{desc.flist}{list of describers, i.e. describing functions}

\item{comp}{if, and how, to compare variables (requires a glist)}

\item{comp.flist}{list of comparers, i.e. comparing functions}

\item{test}{if to test variables}

\item{test.flist}{list of testers, i.e. testing functions}

\item{glist}{grouping list, if wanted. This can either be a list of logical
vectors equal in length to the numbers of rows (i.e. logical indexes), in
which case overlapping groups can be made, or the name of a variable in
the data frame (in which case that variable will be removed from output)
or just any variable in the global workspace}

\item{w}{weights, if wanted}

\item{useNA}{how to handle \code{NA} (requires that functions in
flist has this argument)}
}
\description{
create descriptive table
}
\details{
create a description of variables of a given type in your
    data. see the vignette for examples
}
\section{Functions}{
\itemize{
\item \code{dtable_list()}: create a list of dtable, one for each type wanted

}}

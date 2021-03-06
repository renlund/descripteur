% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtable-other.R
\name{latex.dtable_other}
\alias{latex.dtable_other}
\title{latex method for dtable_other objects}
\usage{
latex.dtable_other(
  x,
  file = "",
  where = "htb",
  rowname = NULL,
  sep = "\\\\\\\\",
  ...
)
}
\arguments{
\item{x}{object}

\item{file}{\code{Hmisc::latex}}

\item{where}{for \code{Hmisc::latex}}

\item{rowname}{for \code{Hmisc::latex}}

\item{sep}{if multiple dtable_other attributes, how to separate them}

\item{...}{arguments passed to \code{Hmisc::latex}}
}
\description{
dtable_other objects are objects with an additional attribute 'dtable_other'
    which will be printed alongside the inherited latex method
}

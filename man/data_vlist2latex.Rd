% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtables.R
\name{data_vlist2latex}
\alias{data_vlist2latex}
\title{data + vlist -> latex table (experimental)}
\usage{
data_vlist2latex(
  data,
  guide = NULL,
  var.list = NULL,
  caption = NULL,
  caption.lot = caption,
  label = NULL,
  longtable = FALSE,
  ...,
  format = TRUE,
  format.param = as.list(NULL),
  n = c(n = "size"),
  tot.name = "All",
  attach = FALSE,
  attach.path = "table",
  attach.name = NULL
)
}
\arguments{
\item{data}{a data frame to be described}

\item{guide}{a dtable guide}

\item{var.list}{a variable list}

\item{caption}{caption}

\item{caption.lot}{caption for list of tables}

\item{label}{label}

\item{longtable}{use longtable?}

\item{...}{arguments passed to \code{dtables}}

\item{format}{logical; want formatting?}

\item{format.param}{list; formatting parameters}

\item{n}{size indicator in table (set to NULL to suppress this)}

\item{tot.name}{name of single column (if no glist)}
}
\description{
Create a dtables from data and make a nice table with "rgroup:s" using a
list that groups variables
}
\examples{
n = 100
d <- data.frame(
    id = sprintf("id\%s", 1:n),
    age = rpois(n, 65),
    sex = sample(c("M", "F"), n, replace = TRUE),
    meas1 = rnorm(n, 10),
    meas2 = rnorm(n, 20),
    dag = as.Date("2020-01-01") + runif(n, min = 0, max = 365)
)
vlist = list(
    "ID" = c(id = "Identity code"),
    "Foo bar" = c(age = 'Age (years)',
                  sex = 'Biological gender',
                  dag = 'Index date'),
    "Baz quuz" = c(meas1 = 'Hiphopinin',
                   meas2 = 'Jazzerum')
)
g <- dtable_guide(d, unit.id = "id", no.bnry = TRUE)
data_vlist2latex(data = d, guide = g, var.list = vlist)
gl <- make_glist(x = "sex", ref = d)
data_vlist2latex(data = d, guide = g, var.list = vlist,
                 glist = gl, comp = TRUE, test = TRUE)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.rename.R
\name{ds.rename}
\alias{ds.rename}
\title{Clientside dplyr rename function}
\usage{
ds.rename(
  df.name = NULL,
  tidy_select = NULL,
  newobj = NULL,
  datasources = NULL
)
}
\arguments{

\item{tidy_select}{List of Tidyselect syntax to be passed to dplyr::rename}

\item{newobj}{Optionally, character specifying name for new server-side data frame. Default is
to overwrite original object.}

\item{datasources}{datashield connections object.}

}
\value{
the object specified by the \code{newobj} argument or
as default same name as input object is written to the serverside.
}
\description{
This function is similar to R function \code{rename}.
}
\details{
Performs dplyr rename
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootmlm_example_2levelonly.R
\name{bootmlm2}
\alias{bootmlm2}
\title{Multi-level Bootstrapping}
\usage{
bootmlm2(id, group1, data)
}
\arguments{
\item{id}{character of level 1 grouping variable/identifier}

\item{group1}{character of level 2 grouping variable (ex: classrooms)}

\item{data}{dataset from which to bootstrap}
}
\description{
Bootstrapping for 2-level multi-level data
}
\examples{
bootmlm2(id = "ID", group1 = "schoolid", data = dat)
}

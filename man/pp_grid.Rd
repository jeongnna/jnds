% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_grid.R
\name{pp_grid}
\alias{pp_grid}
\alias{pp_switch}
\alias{pp_step}
\title{Create pp_grid module.}
\usage{
pp_grid(..., data, default = pp(), type = "grid")

pp_switch(..., data, default = pp())

pp_step(..., data, default = pp())
}
\arguments{
\item{...}{pp modules.}

\item{data}{A data frame. It should be a training data.}

\item{default}{The baseline pp module. It will precede
other modules in all cases in grid.}

\item{type}{If `"grid"`, all possible subsets will be considered.
If `"switch"`, each module will be added to default indepentently.
If `"step"`, modules will be added to default one by one.}
}
\description{
Create pp_grid module.
}

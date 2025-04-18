% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataClasses.R
\name{dataClasses}
\alias{dataClasses}
\alias{dataClasses.default}
\alias{dataClasses.terms}
\alias{dataClasses.merMod}
\alias{dataClasses.zeroinfl}
\title{dataClasses}
\usage{
dataClasses(x)

\method{dataClasses}{default}(x)

\method{dataClasses}{terms}(x)

\method{dataClasses}{merMod}(x)

\method{dataClasses}{zeroinfl}(x)
}
\arguments{
\item{x}{a regression model}
}
\value{
S3 generic function \code{\link[=dataClasses]{dataClasses()}} returns a named \link[base]{character} \link[base]{vector}.
}
\description{
..
}
\details{
Function \code{\link[=dataClasses]{dataClasses()}} is inspired by the attribute \code{'dataClasses'} of
\link[stats]{terms} object.
There are two possible pipelines to achieve this purpose,
\describe{

\item{\emph{\strong{Default}} \code{\link[=dataClasses.default]{dataClasses.default()}}}{\code{x |> terms() |> dataClasses.terms()}.
Minor variation(s) include,
\itemize{
\item{\code{\link[=dataClasses.zeroinfl]{dataClasses.zeroinfl()}} for \link[pscl]{zeroinfl} object,
as \code{pscl:::terms.zeroinfl()} return is not desired}
}
}

\item{\emph{\strong{Alternative}}}{\code{x |> model.frame() |> attr(which = 'terms', exact = TRUE) |> dataClasses.terms()},
e.g.,
\itemize{
\item{\code{\link[=dataClasses.merMod]{dataClasses.merMod()}} for \link[lme4]{merMod} object,
as \code{lme4:::terms.merMod()} return does not have \code{'dataClasses'} attribute}
}
}
}

Therefore,
\itemize{
\item{For object that has \link[stats]{terms} dispatch but no \link[stats]{model.frame} dispatch,
we use the default pipeline and do \emph{not} write a \verb{model.frame.*} function, e.g., \link[nlme]{lme} and \link[nlme]{gls} objects}
}
}

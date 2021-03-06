% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tisefka_multiple_UI.R
\name{SA_tisefka_multiple_mod}
\alias{SA_tisefka_multiple_mod}
\title{Saldae Dashboard Module Server}
\usage{
SA_tisefka_multiple_mod(
  input,
  output,
  session,
  tisefka,
  div_width = "col-xs-6 col-sm-12 col-md-4"
)
}
\arguments{
\item{input}{input shinydashboard elements containing information to use for output generation}

\item{output}{output shinydashboard element}

\item{session}{shiny session}

\item{tisefka}{reactive object containing data}

\item{div_width}{dimension information about the framework(html object)}
}
\value{
output objects to be displayed in corresponding UI module
}
\description{
Saldae Dashboard module SERVER : render and generate multiple output objects (chart/table)
}
\author{
Farid Azouaou
}

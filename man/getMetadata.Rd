% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dhis2api.R
\name{getMetadata}
\alias{getMetadata}
\title{getMetadata(end_point, filters, fields)}
\usage{
getMetadata(
  end_point,
  filters = NULL,
  fields = NULL,
  base_url = getOption("baseurl")
)
}
\arguments{
\item{end_point}{string - api endpoint for the metadata of interest e.g. dataElements, 
organisationUnits}

\item{filters}{- list of strings - the parameters for  the DHIS2 metadata filter, 
e.g. c("id:eq:1234","name:in:Kenya,Rwanda")}

\item{fields}{- string for the fields to return structured as DHIS 2 expects,
e.g. "name,id,items[name,id]"}

\item{base_url}{string - base address of instance (text before api/ in URL)}
}
\value{
list of metadata details
}
\description{
General utility to get metadata details from DATIM
}

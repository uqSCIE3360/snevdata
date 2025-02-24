% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{volcanoes}
\alias{volcanoes}
\title{Volcanic compositions from ocean islands}
\format{
A list containing two tibbles:
\describe{
  \item{dat}{The actual data. This table has columns for location, source of data (columns 'source' and 'citation'), geographical coordinates, rock characteristics, and chemical compositions (all in weight percent).}
  \item{references}{References in which the data were reported. The first column, 'key', corresponds to the 'citation' column in the dat table, for all citations derived from the GEOROC database. The second column contains the citation for the article.}
  }
}
\source{
Teresa Ubide, Patricia Larrea, Laura Becerril, Carlos Galé. Volcanic plumbing filters on ocean-island basalt geochemistry. Geology 50 (1): 26–31 (2022). doi: https://doi.org/10.1130/G49224.1
}
\usage{
volcanoes
}
\description{
Global database of volcanic compositions from ocean islands, collated from the literature. The database contains bulk rock and glass compositions from volcanic samples in Atlantic, Pacific and Indian ocean islands.
}
\details{
Data was sourced from GEOROC (Geochemistry of Rocks of the Oceans and Continents, http://georoc.mpch-mainz.gwdg.de/georoc/) as well as previous compilations by Larrea et al. (2018) and Jeffery and Gertisser (2018) for Atlantic OIBs.
}
\keyword{datasets}

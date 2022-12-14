% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ThreeSpecies_Map.R
\name{ThreeSpecies_map}
\alias{ThreeSpecies_map}
\title{Produces range map for three species}
\usage{
ThreeSpecies_map(
  dataframe,
  background_map,
  greatlakes_map,
  crs_map,
  color_background = "White",
  alpha_range = 0.4
)
}
\arguments{
\item{dataframe}{an sf dataframe generated by ThreeSpecies_df}

\item{background_map}{an sf object (map of a country, state, etc)}

\item{greatlakes_map}{an sf object of the Great Lakes}

\item{crs_map}{CRS to use for ranges and background maps}

\item{color_background}{fill color of background, defaults to white, wrap with ""}

\item{alpha_range}{transparency of range, defaults to 0.4}
}
\value{
a ggplot object
}
\description{
Produces range map for three species
}
\examples{
'TwoSpecies_map(ranges, NorthAmerica, GreatLakes, 5070)'
}

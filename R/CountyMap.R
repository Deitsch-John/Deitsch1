#' Title map county-level occurence of a species in any US state
#'
#' @param dataframe an sf dataframe with a geometry column consisting of coordinates for each observation
#' @param species_col column in the dataframe identifying the species of each observation
#' @param species_to_map species to be mapped, wrap in quotes -> "Example sp."
#' @param county_shapefile a sf object of the counties of the United States
#' @param state_col state column in the counties object
#' @param state_to_map state that is to be mapped
#'
#' @return a ggplot object
#' @export
#'
#' @examples CountyMap(frog_obs_iNat, Species, "Green Tree Frog", USAcounties, State, "GA")
CountyMap <- function(dataframe, species_col, species_to_map,
                      county_shapefile, state_col, state_to_map) {

  Species_observations <- dataframe %>%
    dplyr::filter(species_col==species_to_map)

  Counties <- county_shapefile %>%
    filter(state_col==state_to_map)

  Species_Counties <- sf::st_join(Species_observations, Counties)%>%
    sf::st_drop_geometry()%>%
    dplyr::group_by(COUNTYNAME)%>%
    dplyr::summarize(Observations = n())%>%
    dplyr::filter(!is.na(COUNTYNAME))

  Species_presence <- Counties %>%
    dplyr::left_join(Species_Counties)%>%
    mutate(Present = ifelse(is.na(Observations), "No", "Yes"))

  SpeciesMap <- ggplot2::ggplot()+
    geom_sf(data = Species_presence,
            fill = ifelse(Present=="Yes", "seagreen3", "ivory1"),
            color = "grey41",
            size = 0.10)+
    geom_sf(data = sf::st_union(Counties),
            fill = NA,
            color = "grey10",
            size = 0.5)+
    theme(
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      panel.border = element_rect(fill = NA, size = 2))+
    annotation_scale(location="bl")+
    annotation_north_arrow(which_north = "true",
                           style = north_arrow_nautical,
                           location = "tr")
  return(SpeciesMap)
}

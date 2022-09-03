#' produces sf datafrane with county level presence/absence data
#'
#' @param dataframe an sf dataframe with a geometry column consisting of coordinates for each observation
#' @param species_col column in the dataframe identifying the species of each observation
#' @param species_to_map species to be mapped, wrap in quotes -> "Example sp."
#' @param county_shapefile a sf object of the counties of the United States
#' @param state_col state column in the counties object
#' @param state_to_map state that is to be mapped
#' @param crs_map CRS to use for observation data and county shapefile
#' @param ... additional states to be mapped, optional
#' @return a spatial dataframe
#' @export
#'
#' @examples 'CountyMap_df(frog_obs_iNat, Species, "Green Tree Frog", 4269, USAcounties, State, "GA")'
CountyMap_df <- function(dataframe, species_col, species_to_map, crs_map,
                      county_shapefile, state_col, state_to_map, ...) {

  sf::st_crs(dataframe)=4269
  sf::st_crs(county_shapefile)=4269

  Species_observations <- dataframe %>%
    dplyr::filter({{species_col}}==species_to_map)

  Counties <- county_shapefile %>%
    dplyr::filter({{state_col}}%in%c(state_to_map, ...))%>%
    dplyr::mutate(CountyState = stringr::str_c(COUNTYNAME, STATE, sep = ", "))

  Counties.t <- sf::st_transform(Counties, crs = crs_map)
  Species_observations.t <- sf::st_transform(Species_observations, crs = crs_map)

  Species_Counties <- sf::st_join(Species_observations.t, Counties.t)%>%
    sf::st_drop_geometry()%>%
    dplyr::group_by(CountyState)%>%
    dplyr::summarize(Observations = dplyr::n())%>%
    dplyr::filter(!is.na(CountyState))

  Species_presence <- Counties.t %>%
    dplyr::left_join(Species_Counties)%>%
    dplyr::mutate(Presence = ifelse(is.na(Observations), "No", "Yes"))

  return(Species_presence)
}


#' maps county level presence/absence data
#'
#' @param dataframe an sf dataframe with presence/absence data for each county, generated with CountyMap_df
#' @param state_col state column in the counties object
#' @param state_to_map state that is to be mapped, wrap with ""
#' @param county_shapefile a sf object of the counties of the United States
#' @param ... additional states to be mapped, optional
#' @param color_present fill color for counties with species, defaults to green, wrap with ""
#' @param color_absent fill color for counties without species, defaults to ivory, wrap with ""
#' @param color_background fill color of background, defaults to white, wrap with ""
#' @param color_county_border color of county borders, defaults to light gray, wrap with ""
#' @param color_state_border color of state borders, defaults to dark gray, wrap with ""
#' @param crs_map CRS to use for observation data and county shapefile, needs to match value inputted into CountyMap_df if two functions used in tandem
#'
#' @return a ggplot object
#' @export
#'
#' @examples 'CountyMap_map(frog_pres, USAcounties, State, "GA", c("NC", "SC"))'
CountyMap_map <- function(dataframe, county_shapefile, crs_map, state_col, state_to_map, ...,
                          color_present = "seagreen3", color_absent = "ivory1",
                          color_background = "White", color_county_border = "grey41",
                          color_state_border = "grey10"){

  State_Outlines <- county_shapefile %>%
    sf::st_transform(., crs = crs_map)%>%
    dplyr::filter({{ state_col }}%in%c(state_to_map, ...))%>%
    dplyr::group_by({{ state_col }})%>%
    dplyr::summarise()

  attach(dataframe)

  Map <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = dataframe,
                     fill = ifelse({{Presence}}=="Yes", color_present, color_absent),
                     color = color_county_border,
                     size = 0.10)+
    ggplot2::geom_sf(data = State_Outlines,
                     fill = NA,
                     color = color_state_border,
                     size = 0.5)+
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = color_background),
      panel.border = ggplot2::element_rect(fill = NA, size = 2))

  detach(dataframe)

  return(Map)
}

#' map the location of species observations
#'
#' @param dataframe an sf dataframe with a geometry column consisting of coordinates for each observation
#' @param species_col column in the dataframe identifying the species of each observation
#' @param species_to_map species to be mapped, wrap in quotes -> "Example sp."
#' @param background_map an sf object (map of a country, state, etc)
#' @param greatlakes_map an sf object of the Great Lakes
#' @param crs_map CRS system to use for observation data and background maps
#' @param color_background fill color of background, defaults to white, wrap with ""
#' @param color_points color of points, defaults to black, wrap with ""
#' @param alpha_points transparency of points, defaults to 0.8
#'
#' @return a ggplot object
#' @export
#'
#' @examples 'RangeMap_points(frog_obs_iNat, Species, "Green Tree Frog", NorthAmerica, GreatLakes, 26917)'
RangeMap_points <- function(dataframe, species_col, species_to_map,
                     background_map, greatlakes_map, crs_map,
                     color_background = "White", color_points = "Black",
                     alpha_points = 0.8) {

  sf::sf_use_s2(FALSE)

  sf::st_crs(background_map)=4326
  sf::st_crs(greatlakes_map)=4326
  sf::st_crs(dataframe)=4326

  background_map.s <- background_map %>%
    sf::st_transform(., 26917)%>%
    dplyr::filter(country%in%c("CAN", "USA", "MEX"))%>%
    sf::st_simplify(., dTolerance = 3000)%>%
    sf::st_transform(., crs = crs_map)

  greatlakes_map.s <- greatlakes_map %>%
    sf::st_transform(., 26917)%>%
    dplyr::filter(NAME %in% c("Lake Michigan", "Lake Erie", "Lake Superior", "Lake Huron", "Lake", "Lake Ontario"))%>%
    sf::st_simplify(., dTolerance = 3000)%>%
    sf::st_transform(., crs = crs_map)

  Species_observations <- dataframe %>%
    dplyr::filter(species_col==species_to_map)%>%
    sf::st_transform(., crs = sf::st_crs(background_map.s))

  sf::sf_use_s2(TRUE)

  box <- sf::st_bbox(Species_observations)

  Map <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = background_map.s,
                     fill = "white",
                     color = "grey41",
                     size = 0.10)+
    ggplot2::geom_sf(data = Species_observations,
                     color = color_points,
                     alpha = alpha_points)+
    ggplot2::geom_sf(data = greatlakes_map.s,
                     fill = "lightskyblue",
                     color = NA)+
    ggplot2::coord_sf(xlim = c(box[1], box[3]),
                      ylim = c(box[2], box[4]))+
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = color_background),
      panel.border = ggplot2::element_rect(fill = NA, size = 2))
  return(Map)

}

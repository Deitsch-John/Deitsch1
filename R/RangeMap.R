#' map a polygon of species occurence
#'
#' @param dataframe an sf dataframe with a geometry column consisting of coordinates for each observation
#' @param species_col column in the dataframe identifying the species of each observation
#' @param species_to_map species to be mapped, wrap in quotes -> "Example sp."
#' @param background_map an sf object (map of a country, state, etc)
#' @param greatlakes_map an sf object of the Great Lakes
#' @param crs_map CRS system to use for observation data and background maps
#'
#' @return a ggplot object
#' @export
#'
#' @examples 'RangeMap(frog_obs_iNat, Species, "Green Tree Frog", NorthAmerica, GreatLakes, 26917)'
RangeMap <- function(dataframe, species_col, species_to_map,
                      background_map, greatlakes_map, crs_map) {

  sf::sf_use_s2(FALSE)

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
    dplyr::filter(species_col==species_to_map)

  Species_range <- Species_observations %>%
    dplyr::summarise()%>%
    sf::st_convex_hull()%>%
    sf::st_transform(., crs = sf::st_crs(background_map.s))

  Range_landx <- sf::st_intersection(Species_range,
                                    sf::st_union(background_map.s))
  Range_land <- sf::st_difference(Range_landx,
                                    sf::st_union(greatlakes_map.s))

  sf::sf_use_s2(TRUE)

  box <- sf::st_bbox(Species_range)

  Map <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = background_map.s,
            fill = "white",
            color = "grey41",
            size = 0.10)+
    ggplot2::geom_sf(data = Range_land,
            fill = "forestgreen",
            alpha = 0.5,
            color = "NA")+
    ggplot2::geom_sf(data = greatlakes_map.s,
            fill = "lightskyblue",
            color = NA)+
    ggplot2::coord_sf(xlim = c(box[1], box[3]),
             ylim = c(box[2], box[4]))+
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "aliceblue"),
      panel.border = ggplot2::element_rect(fill = NA, size = 2))+
    ggspatial::annotation_scale(location="bl")+
    ggspatial::annotation_north_arrow(which_north = "true",
                                      style = north_arrow_nautical,
                                      location = "tr")

  return(Map)

}

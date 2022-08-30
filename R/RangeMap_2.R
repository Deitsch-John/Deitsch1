#' Title
#'
#' @param dataframe an sf dataframe with a geometry column consisting of coordinates for each observation
#' @param species_col column in the dataframe identifying the species of each observation
#' @param species1 1st species, wrap in quotes -> "Example sp."
#' @param species2 2nd species, wrap in quotes -> "Example sp."
#' @param background_map an sf object (map of a country, state, etc)
#' @param greatlakes_map an sf object of the Great Lakes
#' @param crs_map CRS system to use for observation data and background maps
#'
#' @return ggplot object
#' @export
#'
#' @examples `RangeMap_2(frog_obs_iNat, Species, "Green Tree Frog", "Gray Tree Frog", NorthAmerica, GreatLakes, 26917)`
RangeMap_2 <- function(dataframe, species_col, species1, species2,
                     background_map, greatlakes_map, crs_map) {

  sf::sf_use_s2(FALSE)

  background_map.s <- background_map %>%
    sf::st_transform(., 26917)%>%
    dplyr::filter(country%in%c("CAN", "USA", "MEX"))%>%
    sf::st_simplify(., dTolerance = 3000)%>%
    sf::st_transform(., crs = crs_map)

  st_crs(dataframe)=4269

  greatlakes_map.s <- greatlakes_map %>%
    sf::st_transform(., 26917)%>%
    dplyr::filter(NAME %in% c("Lake Michigan", "Lake Erie", "Lake Superior", "Lake Huron", "Lake", "Lake Ontario"))%>%
    sf::st_simplify(., dTolerance = 3000)%>%
    sf::st_transform(., crs = crs_map)

  print("Formatted background maps.")

  Range1 <- dataframe %>%
    dplyr::filter(species_col==species1)%>%
    dplyr::summarise()%>%
    sf::st_convex_hull()%>%
    sf::st_transform(., crs = crs_map)

  Range2 <- dataframe %>%
    dplyr::filter(species_col==species2)%>%
    dplyr::summarise()%>%
    sf::st_convex_hull()%>%
    sf::st_transform(., crs = crs_map)

  Landx1 <- sf::st_intersection(Range1, sf::st_union(background_map.s))
  Land1 <- sf::st_difference(Landx1, sf::st_union(greatlakes_map.s))

  print(stringr::str_c("Created map for ", species1, ".", sep = ""))

  Landx2 <- sf::st_intersection(Range2, sf::st_union(background_map.s))
  Land2 <- sf::st_difference(Landx2, sf::st_union(greatlakes_map.s))

  print(stringr::str_c("Created map for ", species2, ".", sep = ""))

  Both <- sf::st_intersection(Land1, Land2)
  Only1 <- sf::st_difference(Land1, Land2)
  Only2 <- sf::st_difference(Land2, Land1)

  print(stringr::str_c("Combined range of ", species1, " and ", species2, ".", sep = ""))

  Ranges <- dplyr::tibble(
    polys = c(Both$geometry, Only1$geometry, Only2$geometry),
    Species = c(stringr::str_c(species1, "and", species2, sep = " "),
                species1, species2)
  )

  Ranges.sf <- sf::st_as_sf(Ranges, geometry = Ranges$polys)%>%
    sf::st_transform(., crs = sf::st_crs(background_map.s))

  sf::sf_use_s2(TRUE)

  box <- sf::st_bbox(sf::st_union(Range1, Range2))

  print("Returning finished map!")

  Map <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = background_map.s,
                     fill = "white",
                     color = "grey41",
                     size = 0.10)+
    ggplot2::geom_sf(data = Ranges.sf,
                     alpha = 0.3,
                     color = "NA",
                     aes(fill = Species))+
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

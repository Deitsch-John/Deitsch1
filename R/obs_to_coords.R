

#' convert a dataframe with lat and long columns to an sf object
#'
#' @param df dataframe with coordinates column
#' @param coord_col column with coordinates
#' @param crs_add desired coordinate reference system
#'
#' @return simple features dataframe
#' @export
#'
#' @examples example.sf <- obs_to_coords(example.df, coords, 4269)
obs_to_coords <- function(df, coord_col, crs_add){
  coords.sfg <- df$coords
  coords.sfc <- st_sfc(coords.sfg, crs = crs_add)

  df %>%
    st_as_sf(geometry = coords.sfc)%>%
    st_transform(., crs = crs_add)

}

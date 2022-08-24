

obs_to_coords <- function(df, coord_col, crs_add){
  coords.sfg <- df$coords
  coords.sfc <- st_sfc(coords.sfg, crs = crs_add)

  df %>%
    st_as_sf(geometry = coords.sfc)%>%
    st_transform(., crs = crs_add)

}

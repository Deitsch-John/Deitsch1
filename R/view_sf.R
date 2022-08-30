
#' view a spatial dataframe in the R studio viewer
#'
#' @param df an sf dataframe with a geometry column
#'
#' @return nothing
#' @export
#'
#' @examples view_sf(USA_counties)
view_sf <- function(df){

  df %>%
    sf::st_drop_geometry( )%>%
    utils::View()
}

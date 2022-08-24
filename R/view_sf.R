
view_sf <- function(df){
  df%>%
    st_drop_geometry()%>%
    View()
}

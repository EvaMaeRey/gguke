stamp_fretboard <- function(){
  
  list(
    annotate(geom = "segment", x = 1:4, y = 0, 
           xend = 1:4, yend = 5, linewidth = 3,
  lineend = "round"),
  annotate(geom = "segment", y = 0:4, 
           yend = 0:4, x = 1, xend = 4, linewidth = 3,
  lineend = "butt"),
  scale_y_reverse(),
  scale_x_continuous(expand = expansion(.2)),
  theme_void(),
  coord_equal(),
  scale_fill_viridis_c(limits = c(1,4), guide = F) 
  )
  
}

gguke <- function(data){ 
  
 ggplot(data = data) + stamp_fretboard()
  
}

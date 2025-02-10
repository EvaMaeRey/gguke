strings <- data.frame(x = 1:4, y = 0, 
           xend = 1:4, yend = 5, lineend = "round")

frets <- data.frame(y = 0:4, 
           yend = 0:4, x = 1, xend = 4, lineend = "butt")

fretboard <- bind_rows(strings, frets)

fretboard %>% 
  ggplot() + 
  aes(x = x, y = y, yend = yend, xend = xend) + 
  geom_segment() + 
  scale_y_reverse()

compute_fretboard <- function(data, scales){
  
  data %>% 
    mutate(phrase = row_number()) %>% 
    crossing(fretboard)
  
}

cars |> slice(1:3) |>
  compute_fretboard()


StatFretboard <- ggproto("StatFretboard", Stat, 
                         compute_panel = compute_fretboard,
                         default_aes = aes(wrap = after_stat(phrase),
                                           color = after_stat(phrase))
                         )

compute_layer_wrap <- function(data, params, panel) {
    
    wrap <- as.numeric(as.factor(data$wrap))
    
    wrapping_x <- ((wrap - 1) %% 3)
    wrapping_y <- ((wrap - 1) %/% 3)
    
    range_x <- 4
    range_y <- 6
    # if(range_x == 0){range_x <- 1}
    # if(range_y == 0){range_y <- 1}
    
    ggplot2::transform_position(
      df = data,
      trans_x = function(x) {x/(range_x) + wrapping_x}, 
      trans_y = function(y) {y/(range_y) + wrapping_y}
    )
  }

PositionWrap <- ggproto(`_class` = 'PositionWrap', `_inherit` = Position,
                        required_aes = c('x', 'y', 'wrap'),
                        compute_layer = compute_layer_wrap)


position_wrap <- function() {
  ggproto(NULL, PositionWrap)
}


ggplot(cars |> slice(1:4)) + 
  geom_segment(stat = StatFretboard, position = "wrap") + 
  coord_trans(y = "reverse")
  
layer_data()

geom_fretboard <- function(...){geom_segment(stat = StatFretboard, position = "wrap", ...)}

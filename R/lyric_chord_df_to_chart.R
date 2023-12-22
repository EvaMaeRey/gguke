
parse_chord2 <- function(chord){parse_chord(chord)[[2]]}


lyric_chord_df_to_chart <- function(lyric_chord_df, bw = F){
  
  lyric_chord_df %>% 
  mutate(phrase = row_number()) %>% 
  mutate(fingering_str = map(chord_name, get)) ->
lyric_chord_df_flat

lyric_chord_df_flat %>% 
  unnest(cols = c(fingering_str)) %>% 
  mutate(fingering = map(fingering_str, parse_chord2)) %>% 
  unnest(cols = c(fingering)) ->
lyric_chord_df_fingering
  
lyric_chord_df_fingering %>% 
  ggplot() +
  annotate(geom = "segment", x = 1:4, y = .5, 
           xend = 1:4, yend = 5, linewidth = 1) + 
  annotate(geom = "segment", y = 0:4 +.5, 
           yend = 0:4 +.5, x = 1, xend = 4, linewidth = 1) +
  aes(x = string, y = fret, label = finger) + 
  geom_point(size = 6, color = "white") +
  geom_point(size = 6, pch= 21, aes(fill = finger), alpha = .7 , show.legend = F) +
  geom_text() +
  facet_wrap(~ paste0("phrase ", phrase,": ", chord_name) %>% fct_inorder()) +
  geom_text(data = lyric_chord_df_flat,
            x = 1, y = -5.5, size = 3,
            aes(label = lyric %>% str_wrap(28)),
            hjust = 0,
            vjust = 1) + 
  scale_y_reverse(limits = c(7, .5)) + 
  coord_equal() + 
  scale_fill_viridis_c(end = .9) + 
  scale_x_continuous(expand = expansion(.6)) + 
  theme_void() ->
plot


if(bw){
  plot$layers[[4]] <- NULL
 plot + 
    geom_point(size = 6, pch= 21, fill = alpha("white", 0), show.legend = F)->
   plot
}
  
plot

}


add_lyric <- function(lyric = "Come stop your cryin', it'll be all right"){
  
    list(labs(caption = lyric %>% str_wrap(28)),
         theme(text = element_text(size = 23)))
  
  
}

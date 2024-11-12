
- [gguke is under construction! 🚧 Feedback
  welcome!](#gguke-is-under-construction--feedback-welcome)
  - [**Chord info input**](#chord-info-input)
  - [Build up slightly larger **chord
    library**](#build-up-slightly-larger-chord-library)
    - [Translate finger_str to fret, finger, string
      info](#translate-finger_str-to-fret-finger-string-info)
    - [Make it a function…](#make-it-a-function)
- [use parsing to expand dataframe to have row per finger
  placement.](#use-parsing-to-expand-dataframe-to-have-row-per-finger-placement)
  - [now we’ll place **fingers positions** w/ point and
    text](#now-well-place-fingers-positions-w-point-and-text)
  - [Part I.b](#part-ib)
    - [step 1. computation (will be left join of a chord
      library)](#step-1-computation-will-be-left-join-of-a-chord-library)
    - [step 2. pass to ggproto](#step-2-pass-to-ggproto)
    - [step 3. write geom_chord](#step-3-write-geom_chord)
  - [then we need a **fret board**.](#then-we-need-a-fret-board)
    - [make it once](#make-it-once)
    - [looks good; make it a function](#looks-good-make-it-a-function)
  - [from **lyric-chord data frame to
    chart**…](#from-lyric-chord-data-frame-to-chart)
    - [use a lyrics-chord data frame.](#use-a-lyrics-chord-data-frame)
  - [from text to chord-lyric data frame (w/ let it
    snow)](#from-text-to-chord-lyric-data-frame-w-let-it-snow)
  - [gershwin](#gershwin)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# gguke is under construction! 🚧 Feedback welcome!

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

{gguke} will provide tools for visualizing ukelele chords with lyrics.
This is for people that aren’t so good at ukelele; they may not really
know the chords yet. It’s a chord-first approach to song charts, which
might have only chord names or small finger position.

And because we make it with ggplot2, we can do a lot with customization.
For example, fingers get assigned a color using the viridis palette.

This is currently a [{knitrExtra}]() project; we’re writing in a kind-of
stream of consciousness way, bundling up useful code into functions as
we go in our README.rmd narrative, and then sending written-up functions
to our R folder with knitr/rmd magic. Our commitment to keeping things
as they are is low at this point, but it is a real package. Lifecycle is
*early* experimental. See also [{litr}]().

Beyond the descriptions of our work, we interject comments on our
hesitations 🤔 and areas that need some work 🚧, for your consideration
marked with emoji.

## **Chord info input**

First, something I’m very excited about, a new-chord ingestion… - it’s
just text. Here’s a C major fingering, where first line is whether
string is played or not. There are four strings across, the number
indicates where on the fret board the specific finger should be placed:

``` r
CM <- 
"pppp
----
----
---3"

GM <- 
"pppp
----
-1-2
--3-"

chord_library <- data.frame(chord_name = 
                              c("CM", "GM"),
                            fingering_str = 
                              c(CM, GM)) 

chord_library
#>   chord_name          fingering_str
#> 1         CM pppp\n----\n----\n---3
#> 2         GM pppp\n----\n-1-2\n--3-
```

## Build up slightly larger **chord library**

``` r
# easy to write
Dm <- 
"pppp
--1-
23--
----"

# re-writen to save space
GM <- "pppp\n----\n-1-2\n--3-"
E7M <- "pppp\n1---\n-2-3\n----"
AM <- "pppp\n-1--\n2---\n----"
DM <- "pppp\n----\n123-\n----"
Dbm <- "pppp\n12--\n----\n----"
`F#m` <- "pppp\n-1--\n2-3-\n----"
D7M <- "pppp\n----\n1111\n---2"
A7M <- "pppp\n-1--\n----\n----"
Am <- "pppp\n1---\n----\n----"
Am7 <- "pppp\n----\n----\n----"
Bm <- "pppp\n----\n-111\n----\n3---"
Em <- "pppp\n----\n---1\n----\n-3--"
```

``` r
chord_library <- tibble(chord_name = 
c("CM", "GM", "Dm", "E7M", "AM","DM", "Dbm", "F#m", "D7M", "A7M", "Am", "Am7", "Bm", "Em"),
fingering_str = 
c(CM, GM, Dm, E7M, AM, DM, Dbm, `F#m`, D7M, A7M, Am, Am7, Bm, Em))
chord_library
#> # A tibble: 14 × 2
#>    chord_name fingering_str                 
#>    <chr>      <chr>                         
#>  1 CM         "pppp\n----\n----\n---3"      
#>  2 GM         "pppp\n----\n-1-2\n--3-"      
#>  3 Dm         "pppp\n--1-\n23--\n----"      
#>  4 E7M        "pppp\n1---\n-2-3\n----"      
#>  5 AM         "pppp\n-1--\n2---\n----"      
#>  6 DM         "pppp\n----\n123-\n----"      
#>  7 Dbm        "pppp\n12--\n----\n----"      
#>  8 F#m        "pppp\n-1--\n2-3-\n----"      
#>  9 D7M        "pppp\n----\n1111\n---2"      
#> 10 A7M        "pppp\n-1--\n----\n----"      
#> 11 Am         "pppp\n1---\n----\n----"      
#> 12 Am7        "pppp\n----\n----\n----"      
#> 13 Bm         "pppp\n----\n-111\n----\n3---"
#> 14 Em         "pppp\n----\n---1\n----\n-3--"
```

### Translate finger_str to fret, finger, string info

Then translate that to data frame that records string, fret, and finger
that will play each note. I’m just doing the first thing that came to
mind - some string manipulation, but might move to read.delim…

``` r

CM |> stringr::str_split("") %>%  .[[1]] %>% .[1:4]
#> [1] "p" "p" "p" "p"
```

``` r

chart <- CM |> stringr::str_split("") %>%  .[[1]] %>% .[5:length(.)] 
num_frets <- length(chart)/5
string <- rep(1:4, num_frets)
fret <- sort(rep(1:num_frets, 4))

data.frame(finger = chart) %>% 
  dplyr::filter(finger != "\n") %>% 
  dplyr::mutate(fret = fret) %>% 
  dplyr::mutate(string = string) %>% 
  dplyr::mutate(finger = ifelse(.data$finger == "-", NA, finger) %>% as.numeric()) %>% 
  dplyr::filter(!is.na(finger)) 
#>   finger fret string
#> 1      3    3      4
```

🤔 maybe read.delim(delim = ““), would work better and feel more
grokable that this string split business! :-)

### Make it a function…

``` r
parse_chord <- function(chord = CM){

play_TF <- chord |> stringr::str_split("") %>%  .[[1]] %>% .[1:4]

chart <- chord |> stringr::str_split("") %>%  .[[1]] %>% .[5:length(.)] 
num_frets <- length(chart)/5
string <- rep(1:4, num_frets)
fret <- sort(rep(1:num_frets, 4))

data.frame(finger = chart) %>% 
  dplyr::filter(finger != "\n") %>% 
  dplyr::mutate(fret = fret) %>% 
  dplyr::mutate(string = string) %>% 
  dplyr::mutate(finger = ifelse(.data$finger == "-", NA, finger) %>% as.numeric()) %>% 
  dplyr::filter(!is.na(finger)) ->
fingering_df

list(play_tf = play_TF, 
     fingering_df = fingering_df)

}
```

``` r

# play or not for each string
parse_chord()[[1]]
#> [1] "p" "p" "p" "p"
```

``` r

# dataframe with finger placement info
parse_chord()[[2]]
#>   finger fret string
#> 1      3    3      4
```

# use parsing to expand dataframe to have row per finger placement.

``` r
parse_chord_fingering <- function(chord){
  
  parse_chord(chord)[[2]]
  
}

chord_library_parse_chords <- function(chord_library){

parsed_chords <- list()

for (i in 1:nrow(chord_library)){
  
  parsed_chords[[i]] <- parse_chord_fingering(chord_library$fingering_str[i])
  
}

chord_library$phrase_chord_id <- 1:nrow(chord_library)
chord_library$parsed_chords <- parsed_chords

chord_library %>% 
  unnest(parsed_chords)

}
```

``` r
chord_library_parsed <- chord_library %>% chord_library_parse_chords() 

chord_library_parsed
#> # A tibble: 33 × 6
#>    chord_name fingering_str            phrase_chord_id finger  fret string
#>    <chr>      <chr>                              <int>  <dbl> <int>  <int>
#>  1 CM         "pppp\n----\n----\n---3"               1      3     3      4
#>  2 GM         "pppp\n----\n-1-2\n--3-"               2      1     2      2
#>  3 GM         "pppp\n----\n-1-2\n--3-"               2      2     2      4
#>  4 GM         "pppp\n----\n-1-2\n--3-"               2      3     3      3
#>  5 Dm         "pppp\n--1-\n23--\n----"               3      1     1      3
#>  6 Dm         "pppp\n--1-\n23--\n----"               3      2     2      1
#>  7 Dm         "pppp\n--1-\n23--\n----"               3      3     2      2
#>  8 E7M        "pppp\n1---\n-2-3\n----"               4      1     1      1
#>  9 E7M        "pppp\n1---\n-2-3\n----"               4      2     2      2
#> 10 E7M        "pppp\n1---\n-2-3\n----"               4      3     2      4
#> # ℹ 23 more rows
```

``` r
usethis::use_pipe()
#> ✔ Setting active project to '/Users/evangelinereynolds/Google
#> Drive/r_packages/gguke'
```

``` r
knitrExtra:::chunk_to_r("parse_chord")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

## now we’ll place **fingers positions** w/ point and text

``` r
data.frame(lyric = c("hello", "goodbye"), 
           chord_name = c("CM", "GM")) %>% 
  inner_join(chord_library %>% chord_library_parse_chords()) %>%
  ggplot() + 
  facet_wrap(~paste(phrase_chord_id, lyric, chord_name)) +
  geom_point(size = 15, 
             aes(x = string, y = fret + .5),
             color = "white"
             ) +
  geom_point(size = 15, pch = 21, alpha = .6,
             aes(x = string, y = fret + .5,
                 fill = finger), 
             ) +
  geom_text(size = 10,
            aes(x = string, y = fret + .5, label = finger)
            )  + 
  scale_y_reverse() + 
  coord_equal(xlim = c(1,4), ylim = c(4, 1))
#> Joining with `by = join_by(chord_name)`
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Part I.b

Make it more ggplot2 grammatical

### step 1. computation (will be left join of a chord library)

``` r
chord_library %>% 
  chord_library_parse_chords() ->
chord_library_parsed

compute_group_uke_fingering <- function(data, scales, chord_library = chord_library_parsed){

  data %>% 
    mutate(row = row_number()) %>% 
    left_join(chord_library_parsed %>% 
                rename(chord = chord_name)) %>% 
    mutate(x = string)
  
}
```

### step 2. pass to ggproto

``` r
StatUkefingers <- ggplot2::ggproto(
  `_class` = "StatUkefingers",
  `_inherit` = ggplot2::Stat,
  required_aes = c("chord"),
  compute_panel = compute_group_uke_fingering,
  default_aes = ggplot2::aes(label = after_stat(finger), 
                             # color = after_stat(finger),
                             fill  = after_stat(finger),
                             y = after_stat(fret+.5)))
```

### step 3. write geom_chord

``` r
library(statexpress)


stat_chord <- function(...){
  
  qlayer(stat = StatUkefingers,
         geom = qproto_update(GeomPoint, 
                              aes(fill = "white", 
                                  size = 15, 
                                  shape = 21)), ...)
  
}

geom_chord <- stat_chord

geom_chord_text <- function(...){
  
    qlayer(stat = StatUkefingers,
           geom = qproto_update(GeomText, 
                                aes(size = 10)),...)

}
```

``` r
tribble(~lyric, ~chord_name,
"Mary had a little lamb,",  "CM",   
"little lamb,", "GM",
"little lamb.", "CM",
"Mary had a little lamb...",    "CM") %>% 
  ggplot() + 
  scale_color_viridis_c() +
  aes(chord = chord_name) + 
  geom_chord(fill = "white") +
  geom_chord(alpha = .6, color = "black") + 
  geom_chord_text(color = "black", size = 10) + 
  facet_wrap(~fct_inorder(lyric)) + 
  scale_y_reverse() + 
  coord_equal(xlim = c(1,4), ylim = c(4, 1))
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## then we need a **fret board**.

### make it once

``` r
library(ggplot2)
ggplot() + 
  annotate(geom = "segment", x = 1:4, y = 0, 
           xend = 1:4, yend = 5, linewidth = 3,
  lineend = "round") + 
  annotate(geom = "segment", y = 0:4, 
           yend = 0:4, x = 1, xend = 4, linewidth = 3) + 
  scale_y_reverse() + 
  scale_x_continuous(expand = expansion(.2)) +
  # theme_void() + 
  coord_equal() +
  scale_fill_viridis_c(limits = c(1,4), guide = F)
#> Warning: The `guide` argument in `scale_*()` cannot be `FALSE`. This was deprecated in
#> ggplot2 3.3.4.
#> ℹ Please use "none" instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### looks good; make it a function

``` r
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
```

``` r
tribble(~lyric, ~chord_name,
"Mary had a little lamb,",  "CM",   
"little lamb,", "GM",
"little lamb.", "CM",
"Mary had a little lamb...",    "CM") %>% 
  gguke() + 
  scale_color_viridis_c() +
  aes(chord = chord_name) + 
  geom_chord(fill = "white") +
  geom_chord(alpha = .6, color = "black") + 
  geom_chord_text(color = "black", size = 10) + 
  facet_wrap(~fct_inorder(lyric))
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
#> Joining with `by = join_by(chord)`
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

🤔 maybe gguke() would be better 🤔 maybe a coord_uke could be a
long-term goal.

``` r
knitrExtra:::chunk_to_r("uke_fretboard")
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

## from **lyric-chord data frame to chart**…

### use a lyrics-chord data frame.

``` r
library(tidyverse)
library(gguke)
lyric_chord_df <- tibble::tribble(~lyric, ~chord_name,
        "Come stop your cryin', it'll be all right", "CM",
"Just take my hand, hold it tight", "CM",
"I will protect you, from all around you", "FM",
"I will be here, don't you ","Dm",
"...cry" , "GM" ,
" ", "E7M",
"'Cause you'll be in my ... heart", "AM",
"... heart", "DM",
"Yes, you'll be in my heart", "E7M",
"... heart", "Dbm", 
"From this day on", "AM",
"Now and forever... more", "DM",
"... more", "GM",
"...", "E7M",
"You'll be in my ...", "AM",
"... heart", "DM",
"No matter what they ...", "E7M",
"say", "Dbm", 
"You'll be here in my... ", "AM", 
" heart", "DM",
"Always", "GM")
```

## from text to chord-lyric data frame (w/ let it snow)

``` r
song <- readLines("Untitled.txt")
#> Warning in readLines("Untitled.txt"): incomplete final line found on
#> 'Untitled.txt'
```

``` r
song_line <- sort(rep(1:(length(song)/2),2))
characters <- song %>% stringr::str_split("") 
element <- rep(c("chord", "lyric"), length(song)/2)


rep_spaces <- function(x){
  
  rep("", x)
}

combine <- function(x, y){
  
  
  c(x, y)
  
}

tibble(song_line, characters, element)  %>% 
  mutate(num_char = map_dbl(characters, length)) %>% 
  group_by(song_line) %>% 
  mutate(max_char = max(num_char)) %>% 
  ungroup() %>% 
  mutate(diff_char = max_char - num_char) %>% 
  mutate(spaces_to_add = map(diff_char, rep_spaces)) %>% 
  mutate(chars_evened_up = map2(characters, spaces_to_add, combine) ) %>% 
  pivot_wider(id_cols = song_line, 
              names_from = element, 
              values_from = chars_evened_up) %>% 
  unnest() %>% 
  mutate(ind_chord_space = chord == " ") %>% 
  mutate(ind_init_chord = !ind_chord_space & lag(ind_chord_space)) %>% 
  mutate(ind_init_chord = replace_na(ind_init_chord, FALSE) ) %>% 
  mutate(id_chord_phrase = cumsum(ind_init_chord)) %>% 
  group_by(id_chord_phrase) %>% 
  summarize(lyric = paste(lyric, collapse = ""),
            chord_name = paste(chord, collapse = "") %>% str_trim()) ->
snow_from_txt
#> Warning: `cols` is now required when using `unnest()`.
#> ℹ Please use `cols = c(chord, lyric)`.
```

``` r

txt_chord_lyrics_to_df <- function(path){

song <- readLines(path)
song_line <- sort(rep(1:(length(song)/2),2))
characters <- song %>% stringr::str_split("") 
element <- rep(c("chord", "lyric"), length(song)/2)


rep_spaces <- function(x){
  
  rep("", x)
}

combine <- function(x, y){
  
  
  c(x, y)
  
}

tibble(song_line, characters, element)  %>% 
  mutate(num_char = map_dbl(characters, length)) %>% 
  group_by(song_line) %>% 
  mutate(max_char = max(num_char)) %>% 
  ungroup() %>% 
  mutate(diff_char = max_char - num_char) %>% 
  mutate(spaces_to_add = map(diff_char, rep_spaces)) %>% 
  mutate(chars_evened_up = map2(characters, spaces_to_add, combine) ) %>% 
  pivot_wider(id_cols = song_line, 
              names_from = element, 
              values_from = chars_evened_up) %>% 
  unnest() %>% 
  mutate(ind_chord_space = chord == " ") %>% 
  mutate(ind_init_chord = !ind_chord_space & lag(ind_chord_space)) %>% 
  mutate(ind_init_chord = replace_na(ind_init_chord, FALSE) ) %>% 
  mutate(id_chord_phrase = cumsum(ind_init_chord)) %>% 
  group_by(id_chord_phrase) %>% 
  summarize(lyric = paste(lyric, collapse = ""),
            chord_name = paste(chord, collapse = "") %>% str_trim())

}
```

## gershwin

- There may have been a problem with phrases that don’t start with a new
  chord. I worked on it in the text file.

``` r
gershwin_ly_ch_df <- txt_chord_lyrics_to_df("gershwin.txt")
#> Warning in readLines(path): incomplete final line found on 'gershwin.txt'
#> Warning: `cols` is now required when using `unnest()`.
#> ℹ Please use `cols = c(chord, lyric)`.
```

Test…

``` r
gershwin_ly_ch_df[1:4,] %>% 
  rename(chord = chord_name) %>% 
  compute_group_uke_fingering(chord_library = chord_library_parsed)
#> Joining with `by = join_by(chord)`
#> # A tibble: 6 × 10
#>   id_chord_phrase lyric   chord   row fingering_str phrase_chord_id finger  fret
#>             <int> <chr>   <chr> <int> <chr>                   <int>  <dbl> <int>
#> 1               0 "Its v… D7        1  <NA>                      NA     NA    NA
#> 2               1 "Our l… FM        2  <NA>                      NA     NA    NA
#> 3               2 " here… Dm        3 "pppp\n--1-\…               3      1     1
#> 4               2 " here… Dm        3 "pppp\n--1-\…               3      2     2
#> 5               2 " here… Dm        3 "pppp\n--1-\…               3      3     2
#> 6               3 " stay" Am7       4  <NA>                      NA     NA    NA
#> # ℹ 2 more variables: string <int>, x <int>
```

``` r
knitr::knit_exit()
```

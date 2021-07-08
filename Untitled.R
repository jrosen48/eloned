library(tidyverse)
library(googlesheets4)

d <- read_sheet("https://docs.google.com/spreadsheets/d/1ANwNwhFTUmn3fQGN2MySlIcBEfB1YAGE/edit#gid=933320791")

prep_otter_transcript <- function(d, length_less_than_100_min = TRUE) {
  
  d2 <- d %>% rename(x1 = 1)
  
  #d2 <- d %>% slice(-nrow(d))
  
  d3 <- d2 %>%
    filter(!is.na(x1)) %>%
    mutate(text = lead(x1)) %>%
    mutate(to_filter = rep(c(TRUE, FALSE), nrow(.)/2)) %>%
    filter(to_filter)
  
  if (length_less_than_100_min) {
    d3 %>%
      mutate(time = str_sub(text, start = -5)) %>%
      mutate(speaker = str_sub(text, end = -5)) %>% 
      select(time, speaker, text = x1) %>%
      mutate_all(str_trim)
  } else {
    d3 %>%
      mutate(time = str_sub(text, start = -6)) %>%
      mutate(speaker = str_sub(text, end = -6)) %>% 
      select(time, speaker, text = x1) %>%
      mutate_all(str_trim)
  }
  
}

prep_otter_transcript(d)

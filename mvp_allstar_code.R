library(pacman)
p_load(rvest,tidyverse)

mvp <- read_html("https://en.wikipedia.org/wiki/NBA_Most_Valuable_Player_Award") %>%
  html_nodes(xpath = "//*[@id='mw-content-text']/div[1]/table[4]") %>%
  html_table() %>%
  data.frame()
mvp

allstars <- read_html("https://en.wikipedia.org/wiki/List_of_NBA_All-Stars") %>%
  html_nodes(xpath = "//*[@id='mw-content-text']/div[1]/table[2]") %>%
  html_table() %>%
  data.frame()
allstars



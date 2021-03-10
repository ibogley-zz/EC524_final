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


logit_df%>% filter(allstar>.9, year==2006)%>%ggplot(aes(x=allstar, y= reorder(player,(allstar)),fill=player))+geom_col()+theme_classic(base_size = 12)+scale_fill_viridis_d() +xlab("Prediction") +ylab("Player")+ ggtitle("2006 Allstar Predictions Logistic Regression Model")




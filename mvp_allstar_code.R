library(pacman)
p_load(rvest,tidyverse)

allstars
test <- allstars$selections[[1]]
test
test_seq <- gsub("[^[:alnum:]]",":",gsub(" ","",test))
unlist(lapply(test_seq,function(x) {eval(parse(text = x))}))

allstars %>% dim()

allstars
#Create a boolean column for binary allstar membership value
full_df$allstar <- FALSE
for (i in 1:nrow(allstars)) {
  
  #Pickup selections in their source format from webscrapped table
  selections <- allstars$selections[[i]]
  
  #Turn the format into yyyy:yyyy for further use
  selections_seq <- gsub("[^[:alnum:]]",":",gsub(" ","",selections))
  
  #put years selected into a useable format: a df
  #player= player selected; selections = years selected for allstar game
  years_selected <- data.frame(
    player = allstars$player[i],
    selections = unlist(lapply(selections_seq,function(x) {eval(parse(text = x))}))
  )
  
  #turn values of full_df's allstar column to true based on membership in years_selected
  full_df[(full_df$player == years_selected$player[1] & 
             full_df$year %in% years_selected$selections),"allstar"] <- TRUE
}

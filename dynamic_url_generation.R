# rm(simpsons)
load("U:/zhelf2/simpsons.RData") # RData vs. RDS -- var name(s) already chosen for RData


host = "https://www.wcoforever.net/anime/the-simpsons" # host website
dynamic_url_generation = function(df, host){
  # df must have episode Name, Season, Episode
  # append url to df
  #
  #browser()
  
  df$url = host
  # season 7-11 need an additional "-season-#"
  i_before7 = which(df$Season<7) # season 7 and beyond
  i_after7 = which(df$Season>=7) # season 7 and beyond
  df$url[i_after7] = paste0(df$url[i_after7], "-season-",df$Season[i_after7])
  # append the episode #
  df$url = paste0(df$url, "-episode-")
  # season 1-6: put the Season#Episode#, 7+ just episode number
  df$url[i_before7] = paste0(df$url[i_before7], df$Season[i_before7], if_else(df$Episode[i_before7]<=9, paste0("0", df$Episode[i_before7]), paste(df$Episode[i_before7])))
  df$url[i_after7] = paste0(df$url[i_after7], df$Episode[i_after7])
  # append episode title, remove punctuaiton
  Name_clean = tolower(removePunctuation(df$Name)) %>% gsub("\\s", "-", .)
  ## hardcoded idiosyncratic Episode titles
  # Name_clean[1] = "simpsons-roasting"
  # ...
  
  df$url = paste0(df$url,"-",Name_clean)
  return(df)
}
dynamic_url_generation(simpsons, host=host)

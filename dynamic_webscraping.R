# idea: web scrape the season/tv show landping page --> using episode and season # find the reference link
# return to user

library(rvest)

dynamic_webscraping = function(s=1,e=1){
  # based on season s, and episode e numbers return the url to the video
  #
  #
  # this function is highly specific to host_url below
  host_url = "https://www.wcoforever.net/anime/the-simpsons" # host website
  
  e_selector = paste0(s, " Episode ", e)
  if(s>=7){
    host_url = paste0(host_url,"-season-",s)
  } else if (s<=5){
    e_selector = paste0(s,if_else(nchar(e)>1,paste0(e),paste0("0",e)))
  }
  
  u = read_html(host_url)
  eps=u %>% html_elements("div.cat-eps a")
  ep_i=which(grepl(e_selector, eps %>% html_attr(.,name="title")))
  a = eps[ep_i] %>% html_attr(., name="href")
  return(a)
}

## NOT RUN
#season_no=6
#episode_no=8
#dynamic_webscraping(s=season_no, e=episode_no)

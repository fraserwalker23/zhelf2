# R Web Scraping

library(rvest)
library(tidyverse)
library(magrittr)
library(scales)
library(knitr)
library(lubridate)
library(tibble)

# defining our URLs
url <- "https://www.imdb.com/title/tt0096697/episodes?season="
szn <- c(1:11)

finalurl <- paste0(url,as.character(szn))

imdbScrape <- function(x){
  page <- x
  name <- page %>%
    read_html() %>% 
    html_nodes('#episodes_content strong a') %>% 
    html_text() %>% 
    as.data.frame()
  rating <- page %>%
    read_html() %>% 
    html_nodes('.ipl-rating-widget > .ipl-rating-star .ipl-rating-star__rating') %>% 
    html_text() %>% 
    as.data.frame()
  details <- page %>%
    read_html() %>% 
    html_nodes('.zero-z-index div') %>% 
    html_text() %>% 
    as.data.frame()
  
  chart <- cbind(name, rating, details)
  names(chart) <- c("Name", "Rating", "Details")
  chart <- as.tibble(chart)
  return(chart)
  Sys.sleep(5)
}

simpsons <- map_df(finalurl, imdbScrape)

simpsons$Season <- str_extract(simpsons$Details, "S[0-9]+")
simpsons$Season <- as.numeric(str_extract(simpsons$Season, "[0-9]+"))
simpsons$Episode <- str_extract(simpsons$Details, "Ep[0-9]+")
simpsons$Episode <- as.numeric(str_extract(simpsons$Episode, "[0-9]+"))

# save(simpsons, file = "C:/Users/fwalker/Documents/R/Zhelfenstein 2/simpsons.RData")
# load(file.choose())

# Link construction ----

simpsons$titleURL <- removePunctuation(simpsons$Name, preserve_intra_word_dashes = TRUE) %>% 
  tolower() %>%
  { gsub(" ", "-", .) }

simpsons <- mutate(simpsons,
                   epURL = if_else(Episode <= 9, paste0("0",Episode), as.character(Episode),"-"),
                   link1 = paste0(const,"episode-",Season,epURL,"-",titleURL,"-2"),
                   link2 = paste0(const,"season-",Season,"-episode-",Episode,"-",titleURL,"-2"),
                   link3 = paste0(const2,Season,"-episode-",Episode,"-",titleURL,"/")
                   )

const <- c("https://www.watchcartoononline.io/the-simpsons-")
const2 <- c("https://watchcartoonsonline.la/watch/the-simpsons-season-")

# works for seasons 1-6. worth using even though not every episode!
# https://www.watchcartoononline.io/the-simpsons-episode-101-simpsons-roasting-2
# https://www.watchcartoononline.io/the-simpsons-episode-216-barts-dog-gets-an-f-2
# https://www.watchcartoononline.io/the-simpsons-episode-220-the-war-of-the-simpsons-2
# https://www.watchcartoononline.io/the-simpsons-episode-515-deep-space-homer-2
# https://www.watchcartoononline.io/the-simpsons-season-6-episode-16-bart-vs-australia-2
# https://www.watchcartoononline.io/the-simpsons-season-6-episode-2-lisas-rival-2

# season 7 and later
# https://watchcartoonsonline.la/watch/the-simpsons-season-7-episode-4-bart-sells-his-soul/
# https://watchcartoonsonline.la/watch/the-simpsons-season-8-episode-20-the-canine-mutiny/
# https://watchcartoonsonline.la/watch/the-simpsons-season-9-episode-19-simpson-tide/
# https://watchcartoonsonline.la/watch/the-simpsons-season-10-episode-19-mom-and-pop-art/
# https://watchcartoonsonline.la/watch/the-simpsons-season-11-episode-11-faith-off/

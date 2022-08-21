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
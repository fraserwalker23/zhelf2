# Zhelfenstein 2
# random simpsons episode generator with links

library(shiny)
library(rvest)
library(tidyverse)
library(magrittr)
library(scales)
library(knitr)
library(lubridate)
library(tibble)
library(tm)

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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Random Simpsons Episode Generator"),
   
   # Sidebar Layout
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId = "seasonInput",
                     label = "Seasons:",
                     min = 1,
                     max = 11,
                     value = c(3,4)),
         actionButton(inputId = "runInput",
                      label = "Go")
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        img(src="simpsons.jpg", width="50%", align = "center"),
        uiOutput(outputId = "linkOutput")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  load("simpsons.RData")
  
  # # defining our URLs
  # url <- "https://www.imdb.com/title/tt0096697/episodes?season="
  # finalurl <- paste0(url,1:11)
  # 
  # simpsons <- map_df(finalurl, imdbScrape)
  # simpsons$Season <- str_extract(simpsons$Details, "S[0-9]+")
  # simpsons$Season <- as.numeric(str_extract(simpsons$Season, "[0-9]+"))
  # simpsons$Episode <- str_extract(simpsons$Details, "Ep[0-9]+")
  # simpsons$Episode <- as.numeric(str_extract(simpsons$Episode, "[0-9]+"))
    
  simpsons$titleURL <- simpsons$Name %>%
    tolower() %>% 
    removePunctuation(., preserve_intra_word_dashes = TRUE) %>%
    { gsub(" ", "-", .) }
  
  const <- c("https://www.watchcartoononline.io/the-simpsons-")
  const2 <- c("https://watchcartoonsonline.la/watch/the-simpsons-season-")

  simpsons_reactive <- reactive({
    simpsonsURL <-filter(simpsons, 
                         Season <= input$seasonInput[2] & 
                         input$seasonInput[1] <= Season) %>%
      mutate(epURL = if_else(Episode <= 9, paste0("0",Episode), as.character(Episode),"-"),
                     link1 = paste0(const,"episode-",as.character(Season),epURL,"-",titleURL,"-2"),
                     link2 = paste0(const,"season-",Season,"-episode-",Episode,"-",titleURL,"-2"),
                     link3 = paste0(const2,Season,"-episode-",Episode,"-",titleURL,"/"),
                     link = if_else(Season == 6, link2, link1),
                     link = if_else(Season >= 7, link3, link)) %>%
      mutate(link = if_else(Name == "Simpsons Roasting on an Open Fire",
                            "https://www.watchcartoononline.io/the-simpsons-episode-101-simpsons-roasting-2", link),
             link = if_else(Name == "Summer of 4'2",
                            "https://watchcartoonsonline.la/watch/the-simpsons-season-7-episode-25-summer-of-4-ft-2/", link)
      )
  })
  
  
  episode <- eventReactive(input$runInput, {
    df <- simpsons_reactive()
    df[sample(nrow(df),1),]
  })
  
  link <- reactive({
    dfLink <- episode()
    a(dfLink$Name, href = dfLink$link)
  })

  output$linkOutput <- renderUI({
    tagList("URL Link:", link())
  })
  
  #observe({print(finalurl())})
  #observe({print(dim(simpsons()))})
  #observe({print(input$runInput)})
  observe({print(input$seasonInput[1])})
  observe({print(simpsons_reactive()[1,11])})
  observe({print(episode())})
  #observe({print(link())})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# "https://www.watchcartoononline.io/the-simpsons-episode-101-simpsons-roasting-2"


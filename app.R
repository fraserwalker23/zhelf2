# Zhelfenstein 2
# random simpsons episode generator with links

library(shiny)
library(dplyr)

source("dynamic_webscraping.R")

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
  
  load("simpsons.RData") # simpsons data frame
  
  simpsons_reactive = reactive({
    filter(simpsons, Season <= input$seasonInput[2] & 
             input$seasonInput[1] <= Season)
  })
  
  episode <- eventReactive(input$runInput, {
    df <- simpsons_reactive()
    df[sample(nrow(df),1),]
  })
  
  link <- reactive({
    dfLink <- episode()
    a(dfLink$Name, href = dynamic_webscraping(s=dfLink$Season,e=dfLink$Episode))
  })
  
  output$linkOutput <- renderUI({
    tagList("URL Link:", link())
  })
  
  observe({print(episode())})
}

# Run the application 
shinyApp(ui = ui, server = server)

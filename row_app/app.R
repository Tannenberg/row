#A Shiny RoW Map

# Load packages and data
library(shiny)
library(rworldmap)
library(tidyverse)
library(readstata13)

my_col <-  c("#e66101", "#fdb863", "#b2abd2", "#5e3c99")
load("./www/row.RData")

# Define UI for application that draws a map
ui <- fluidPage(
  
  # Application title
  titlePanel("Regimes of the World"),
  
  p("This is an app that displays regime types across the world from 1900 to today. 
    The measure is based on a categorization developed by", 
    a("Lührmann, Tannenberg, and Lindberg (2018)", 
      href = "https://www.cogitatiopress.com/politicsandgovernance/article/view/1214"), 
    "that utilizes",
    a("V-Dem's", href = "https://www.v-dem.net"), 
    "excellent data to code any given country and year as either a",
    tags$strong(tags$span(style="color:#e66101", "Closed Autocracy")), ",", 
    tags$strong(tags$span(style="color:#fdb863", "Electoral Autocracy")), ",", 
    tags$strong(tags$span(style="color:#b2abd2", "Electoral Democracy")), "or a",
    tags$strong(tags$span(style="color:#5e3c99", "Liberal Democracy")), ".", 
    " ", "The article describing our coding schema, and comparing RoW to extant data sets is freely available from open access journal", 
    a("Politics and Governance", 
      href = "https://www.cogitatiopress.com/politicsandgovernance/index"), 
     ".", tags$hr()
  ), 
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("animation", "Looping Animation:",
                  inputId = "year",
                  label = "Pick a year:",
                  min = 1900, max = 2016,
                  value = 1990, step = 1,
                  animate = animationOptions(interval = 500, loop = FALSE),
                  sep = ""
      ),
      
      selectInput(inputId = "region", 
                  label = "Select region:",
                  choices = c("World" = "world", "Africa" = "africa", "Asia" = "asia", 
                              "Europe" = "eurasia", "North America" = "North America", 
                              "Latin America" = "latin america", "Oceania" = "oceania")
      ),
      
      
      helpText("Data from: Lührmann, A, Tannenberg, M., Lindberg, S. I., (2018). 
               Regimes of the World (RoW): Opening New Avenues for the Comparative Study
               of Political Regimes. Politics and Governance, 6(1), p. 60–77"), 
      
      # Button
      downloadButton("downloadData", "Download Data - RoW")
      #tags$img(height = "auto", width = "auto", src = "pres_typology2.png")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "map")
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- renderPlot({
    
    
    mapdata <- joinCountryData2Map(row_v71 %>% filter(year== input$year),
                                   joinCode = "ISO3", 
                                   nameJoinColumn = "country_text_id"
    )
    
    # Draw the map
    mapCountryData(mapdata, nameColumnToPlot="v2x_regime", mapTitle = "", 
                   colourPalette = my_col, numCats = 4, oceanCol = NA, catMethod = "cathegorical", 
                   addLegend = F, borderCol = "#666666", mapRegion = input$region)
    
    #legend("bottom", c("Closed Autocracy", "Electoral Autocracy","Electoral Democracy", "Liberal Democracy"), 
    #     bg="white", fill = bwsafe, bty = "n", ncol = 2, title = "", cex = 1.2) 
    
  }, height="auto", width = "auto")
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('row-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(row_v71, con)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(tmap)
library(sf)
library(plotly)
library(dplyr)
library(ggplot2)
library(stringr)
library(hrbrthemes)
library(shinyWidgets)

setwd("/Users/franbelac/Downloads")
tmap_mode("view")
options(scipen=999)
hr <- st_read("fileZaCrtanjeHrv/gadm40_HRV_1.shp")

ui <- fluidPage(
  setBackgroundColor(
    color = c("#2171B5","#F7FBFF","#FF0000"),
    gradient = "linear",
    direction = "left"
  ),
  titlePanel(title=h1("CroStats", align="center",style = "font-size:60px")),
  sidebarLayout(
  mainPanel(
    tmapOutput(outputId = "map", width = "100%", height = 900)
  ),
  sidebarPanel(
    h1("Zagrebačka županija"),
    hr(),
    plotOutput("plot"),
    br(),br(),br(),
    h2("Fun Fact 1"),
    helpText("Serman je jedina vredela od svih"),
    h2("Fun Fact 2"),
    helpText("Serman je jedina vredela od svih"),
    h2("Fun Fact 3"),
    helpText("Serman je jedina vredela od svih")
  )
)
)

server <- function(input, output, session) {
  
  output$map <- renderTmap({
    tm_shape(hr) + tm_fill("NAME_1") + tm_borders() +tm_layout(title="Republika Hrvatska")
  })
  
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    
    print(click)

  })
  
  output$plot <- renderPlot(plot(mtcars$wt, mtcars$mpg))

}

shinyApp(ui = ui, server = server)

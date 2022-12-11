library(shiny)
library(tmap)
library(sf)
library(plotly)
library(dplyr)
library(ggplot2)
library(stringr)
library(hrbrthemes)
library(shinyWidgets)



tmap_mode("view")
options(scipen=999)
hr <- st_read("fileZaCrtanje2/zupanije.shp")

zupanije <- c("Grad Zagreb","Medimurska","Krapinsko-zagorska","Varazdinska","Viroviticko-podravska","Pozesko-slavonska","Koprivnicko-krizevacka","Bjelovarsko-bilogorska","Vukovarsko-srijemska","Brodsko-posavska","Karlovacka","Osjecko-baranjska","Sisacko-moslavacka","Licko-senjska","Istarska","Zagrebacka","Sibensko-kninska","Dubrovacko-neretvanska","Splitsko-dalmatinska","Primorsko-goranska","Zadarska")
hr$Zupanija <- zupanije

sjediste <- c("Zagreb","Cakovec","Krapina","Varazdin","Virovitica","Pozega","Koprivnica","Bjelovar","Vukovar","Slavonski Brod","Karlovac","Osijek","Sisak","Gospic","Pazin","Zagreb","Sibenik","Dubrovnik","Split","Rijeka","Zadar")
hr$SjedisteZupanije <- sjediste

brojstanovnika <- c(777183,107615,121934,161820,71432,65158,102564,103448,147022,134283,114254,262852,142613,43439,198155,304348,98460,117242,431213,269508,162481)
hr$brojStanovnika <- brojstanovnika

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
      h1("Zagrebacka zupanija"),
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
    tm_shape(hr) + tm_fill("Zupanija",popup.vars=c("Sjediste"="SjedisteZupanije","Broj stanovnika"="brojStanovnika")) + tm_borders() +tm_layout(title="Republika Hrvatska")
  })
  
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    
    print(click)
    
  })
  
  output$plot <- renderPlot(plot(mtcars$wt, mtcars$mpg))
  
}

shinyApp(ui = ui, server = server)
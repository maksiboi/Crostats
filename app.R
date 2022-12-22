library(shiny)
library(tmap)
library(sf)
library(plotly)
library(dplyr)
library(ggplot2)
library(stringr)
library(hrbrthemes)
library(shinyWidgets)
library(readxl)
library(tidyr)
library(gridExtra)

stanovnistvo <- read_excel("dataset.xlsx", sheet = "Stanovnistvo")
narodnost <- read_excel("dataset.xlsx", sheet = "Narodnost")
spol <- read_excel("dataset.xlsx", sheet = "Spol")
starost <- read_excel("dataset.xlsx", sheet = "Starost")
rodeni <- read_excel("dataset.xlsx", sheet = "Zivorodeni")
umrli <- read_excel("dataset.xlsx", sheet = "Umrli")
brakovi <- read_excel("dataset.xlsx", sheet = "Brakovi")
razvodi <- read_excel("dataset.xlsx", sheet = "Razvodi")
e_gradani <- read_excel("dataset.xlsx", sheet = "E-gradani")
zupanije_id <- c("Grad.Zagreb","Medimurska","Krapinsko.zagorska","Varazdinska","Viroviticko.podravska","Pozesko.slavonska","Koprivnicko.krizevacka","Bjelovarsko.bilogorska","Vukovarsko.srijemska","Brodsko.posavska","Karlovacka","Osjecko.baranjska","Sisacko.moslavacka","Licko.senjska","Istarska","Zagrebacka","Sibensko.kninska","Dubrovacko.neretvanska","Splitsko.dalmatinska","Primorsko.goranska","Zadarska")
stanovnistvo <- cbind(zupanije_id, stanovnistvo[-1,])

#narodnost <- cbind(zupanije_id, narodnost[-1,])
#spol <- cbind(zupanije_id, spol[-1,])
#starost <- cbind(zupanije_id, starost[-1,])
rodeni <- cbind(zupanije_id, rodeni[-1,])

umrli <- cbind(zupanije_id, umrli[-1,])

brakovi <- cbind(zupanije_id, brakovi[-1,])

razvodi <- cbind(zupanije_id, razvodi[-1,])

#rodeni1 <- rodeni[-1,]
rodeni1 <- pivot_longer(rodeni, "1998":"2021" ,names_to="Godine",values_to = "rodeni")

#umrli1 <- umrli[-1,]
umrli1 <- pivot_longer(umrli, "1998":"2021" ,names_to="Godine",values_to = "umrli")


umrli_rodeni <- cbind(rodeni1,"umrli"=umrli1$umrli)

filtriranaZupanija <-umrli_rodeni %>% filter(Zupanija %in% c("Grad Zagreb","Varazdinska"))

rodeni_barplot <- ggplot(filtriranaZupanija,aes(x=Godine,y=rodeni,color=Zupanija)) +
  geom_point() +
  geom_line(aes(group=1))  +
  theme(axis.text.x = element_text(angle=45)) 

p2 <- ggplotly(rodeni_barplot) 

tmap_mode("view")
options(scipen=999)
hr <- st_read("fileZaCrtanje2/zupanije.shp")

zupanije <- c("Grad Zagreb","Medimurska","Krapinsko-zagorska","Varazdinska","Viroviticko-podravska","Pozesko-slavonska","Koprivnicko-krizevacka","Bjelovarsko-bilogorska","Vukovarsko-srijemska","Brodsko-posavska","Karlovacka","Osjecko-baranjska","Sisacko-moslavacka","Licko-senjska","Istarska","Zagrebacka","Sibensko-kninska","Dubrovacko-neretvanska","Splitsko-dalmatinska","Primorsko-goranska","Zadarska")
hr$Zupanija <- zupanije

sjediste <- c("Zagreb","Cakovec","Krapina","Varazdin","Virovitica","Pozega","Koprivnica","Bjelovar","Vukovar","Slavonski Brod","Karlovac","Osijek","Sisak","Gospic","Pazin","Zagreb","Sibenik","Dubrovnik","Split","Rijeka","Zadar")
hr$SjedisteZupanije <- sjediste

brojstanovnika <- c(777183,107615,121934,161820,71432,65158,102564,103448,147022,134283,114254,262852,142613,43439,198155,304348,98460,117242,431213,269508,162481)
hr$brojStanovnika <- brojstanovnika

hr$zupanije_id <- zupanije_id

ui <- fluidPage(
  setBackgroundColor(
    color = c("#2171B5","#F7FBFF","#FF0000"),
    gradient = "linear",
    direction = "left"
  ),
  titlePanel(title=h1("CroStats", align="center",style = "font-size:60px")),
  sidebarLayout(
    mainPanel(
      tmapOutput(outputId = "map", height = 900),
      width = 7
    ),
    sidebarPanel(
      h1("Zagrebacka zupanija"),
      hr(),
      plotlyOutput("plot"),
      br(),br(),br(),
      h2("Fun Fact 1",style = "font-size:30px"),
      helpText("Serman je jedina vredela od svih",style = "font-size:20px"),
      h2("Fun Fact 2",style = "font-size:30px"),
      helpText("Serman je jedina vredela od svih",style = "font-size:20px"),
      h2("Fun Fact 3",style = "font-size:30px"),
      helpText("Serman je jedina vredela od svih",style = "font-size:20px"),
      width = 5
    )
  )
)

server <- function(input, output, session) {
  
  #tu treba ici logika ista logika da se na ODABRANI STUPAC oznaci, a ostali postave NA 
  #ne znam kak izbaratati da se obojaju dvije zupanije osim ovog nacina
  hr <- hr %>% mutate(bojaj=ifelse(zupanije_id %in% c("Grad.Zagreb"),"Grad.Zagreb",ifelse(zupanije_id %in% c("Medimurska"),"Medimurska",NA)))

  #Paleta za kartu
  Mypal <- c('#313695','#fee090','#d73027','#72001a')
  
  #col = stupac u hr-u koji boja zupanije
  #pallete = paleta boja... to cemo lako stilki namjestiti
  #showNA = F znaci da se u legendi prikazuju samo odabrane zupanije
  
  output$map <- renderTmap({
    tm_shape(hr) + tm_fill(col="bojaj",popup.vars=c("Sjediste"="SjedisteZupanije","Broj stanovnika"="brojStanovnika"), palette = Mypal, title="Zupanija",showNA = F) + tm_borders() +tm_layout(title="Republika Hrvatska")
  })
  
  
  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    novi_id <- click$id
    
    print(click)
    
    filtriranaZupanija <-umrli_rodeni %>% filter(zupanije_id %in% c(novi_id))
    
    rodeni_barplot <- ggplot(filtriranaZupanija,aes(x=Godine,y=rodeni,color=Zupanija)) +
      geom_point() +
      geom_line(aes(group=1))  +
      theme(axis.text.x = element_text(angle=45)) 
    
    p2 <- ggplotly(rodeni_barplot) 

    output$plot <- renderPlotly(p2)
  })
  
  output$plot <- renderPlotly(p2)
  
}

shinyApp(ui = ui, server = server)
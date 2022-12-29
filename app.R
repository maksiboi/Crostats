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
doseljeni <- read_excel("dataset.xlsx", sheet = "Doseljeni")
odseljeni <- read_excel("dataset.xlsx", sheet = "Odseljeni")

zupanije_id <-
  c(
    "Grad.Zagreb",
    "Medimurska",
    "Krapinsko.zagorska",
    "Varazdinska",
    "Viroviticko.podravska",
    "Pozesko.slavonska",
    "Koprivnicko.krizevacka",
    "Bjelovarsko.bilogorska",
    "Vukovarsko.srijemska",
    "Brodsko.posavska",
    "Karlovacka",
    "Osjecko.baranjska",
    "Sisacko.moslavacka",
    "Licko.senjska",
    "Istarska",
    "Zagrebacka",
    "Sibensko.kninska",
    "Dubrovacko.neretvanska",
    "Splitsko.dalmatinska",
    "Primorsko.goranska",
    "Zadarska"
  )
stanovnistvo <- cbind(zupanije_id, stanovnistvo[-1,])

doseljeni <- cbind(zupanije_id, doseljeni)

odseljeni <- cbind(zupanije_id, odseljeni)

#narodnost <- cbind(zupanije_id, narodnost[-1,])
#spol <- cbind(zupanije_id, spol[-1,])
#starost <- cbind(zupanije_id, starost[-1,])
rodeni <- cbind(zupanije_id, rodeni[-1,])

umrli <- cbind(zupanije_id, umrli[-1,])

brakovi <- cbind(zupanije_id, brakovi[-1,])

razvodi <- cbind(zupanije_id, razvodi[-1,])


e_gradani<- e_gradani[-1,]

#rodeni1 <- rodeni[-1,]
rodeni1 <-
  pivot_longer(rodeni,
               "1998":"2021" ,
               names_to = "Godine",
               values_to = "rodeni")

#umrli1 <- umrli[-1,]
umrli1 <-
  pivot_longer(umrli,
               "1998":"2021" ,
               names_to = "Godine",
               values_to = "umrli")
brakovi1 <-
  pivot_longer(brakovi,
               "1998":"2021" ,
               names_to = "Godine",
               values_to = "brakovi")

razvodi1 <-
  pivot_longer(razvodi,
               "1998":"2021" ,
               names_to = "Godine",
               values_to = "razvodi")

### grupirano ovako zbog godina
umrli_rodeni_brakovi_razvodi <- cbind(rodeni1, "umrli" = umrli1$umrli, "brakovi" = brakovi1$brakovi, "razvodi" = razvodi1$razvodi)

doseljeni1 <-
  pivot_longer(doseljeni,
               "2011":"2021" ,
               names_to = "Godine",
               values_to = "doseljeni")

odseljeni1 <-
  pivot_longer(odseljeni,
               "2011":"2021" ,
               names_to = "Godine",
               values_to = "odseljeni")

odseljeni_doseljeni <-
  cbind(odseljeni1, "doseljeni" = doseljeni1$doseljeni)

stanovnistvo1 <- 
  pivot_longer(stanovnistvo,
               "2001":"2021",
               names_to = "Godine",
               values_to = "stanovnistvo")




changePlotType <- function(plotType, filtriranaZupanija) {
  if (plotType == 1) {
    rodeni_barplot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = rodeni, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      theme(axis.text.x = element_text(angle = 45),panel.background = element_rect(fill = "white", colour = "grey50"),panel.border = element_rect(color="black",fill=NA),legend.background = element_rect(size=0.5,linetype="solid",colour="black"))+
      ylab("Broj rođenih")+
      ggtitle("Broj stanovnika")
    
    rodeni_barplot %>% ggplotly
    
  } else if (plotType == 2) {
    umrli_barplot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = umrli, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      theme(axis.text.x = element_text(angle = 45),panel.background = element_rect(fill = "white", colour = "grey50"),panel.border = element_rect(color="black",fill=NA),legend.background = element_rect(size=0.5,linetype="solid",colour="black"))+
      ylab("Broj umrlih")+
      ggtitle("Broj stanovnika")
    
    umrli_barplot %>% ggplotly
  } else if (plotType == 3) {
    odseljeni_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = odseljeni, color = Zupanije)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      theme(axis.text.x = element_text(angle = 45),panel.background = element_rect(fill = "white", colour = "grey50"),panel.border = element_rect(color="black",fill=NA),legend.background = element_rect(size=0.5,linetype="solid",colour="black"))+
      ylab("Broj odseljenog stanovništva") +
      ggtitle("Broj stanovnika")
    
  } else if (plotType == 4) {
    doseljeni_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = doseljeni, color = Zupanije)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      ylab("Broj doseljenog stanovnistva") +
      theme(axis.text.x = element_text(angle = 45),panel.background = element_rect(fill = "white", colour = "grey50"),panel.border = element_rect(color="black",fill=NA),legend.background = element_rect(size=0.5,linetype="solid",colour="black"))+
      ggtitle("Broj stanovnika")
  } else if (plotType == 5) {
    stanovnistvo_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = stanovnistvo, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      ylab("Broj stanovnistva") +
      theme(axis.text.x = element_text(angle = 45),panel.background = element_rect(fill = "white", colour = "grey50"),panel.border = element_rect(color="black",fill=NA),legend.background = element_rect(size=0.5,linetype="solid",colour="black"))+
      ggtitle("Broj stanovnika")
  } else if (plotType == 6) {
    brakovi_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = brakovi, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      ylab("Broj sklopljenih brakova") +
      theme(axis.text.x = element_text(angle = 45),panel.background = element_rect(fill = "white", colour = "grey50"),panel.border = element_rect(color="black",fill=NA),legend.background = element_rect(size=0.5,linetype="solid",colour="black"))
  } else if (plotType == 7) {
    razvodi_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = razvodi, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      ylab("Broj razvoda ") +
      theme(axis.text.x = element_text(angle = 45),panel.background = element_rect(fill = "white", colour = "grey50"),panel.border = element_rect(color="black",fill=NA),legend.background = element_rect(size=0.5,linetype="solid",colour="black"))
  }
  
}

tmap_mode("view")
options(scipen = 999)
hr <- st_read("fileZaCrtanje2/zupanije.shp")

zupanije <-
  c(
    "Grad Zagreb",
    "Medimurska",
    "Krapinsko-zagorska",
    "Varazdinska",
    "Viroviticko-podravska",
    "Pozesko-slavonska",
    "Koprivnicko-krizevacka",
    "Bjelovarsko-bilogorska",
    "Vukovarsko-srijemska",
    "Brodsko-posavska",
    "Karlovacka",
    "Osjecko-baranjska",
    "Sisacko-moslavacka",
    "Licko-senjska",
    "Istarska",
    "Zagrebacka",
    "Sibensko-kninska",
    "Dubrovacko-neretvanska",
    "Splitsko-dalmatinska",
    "Primorsko-goranska",
    "Zadarska"
  )
hr$Zupanija <- zupanije

sjediste <-
  c(
    "Zagreb",
    "Cakovec",
    "Krapina",
    "Varazdin",
    "Virovitica",
    "Pozega",
    "Koprivnica",
    "Bjelovar",
    "Vukovar",
    "Slavonski Brod",
    "Karlovac",
    "Osijek",
    "Sisak",
    "Gospic",
    "Pazin",
    "Zagreb",
    "Sibenik",
    "Dubrovnik",
    "Split",
    "Rijeka",
    "Zadar"
  )
hr$SjedisteZupanije <- sjediste

brojstanovnika <-
  c(
    777183,
    107615,
    121934,
    161820,
    71432,
    65158,
    102564,
    103448,
    147022,
    134283,
    114254,
    262852,
    142613,
    43439,
    198155,
    304348,
    98460,
    117242,
    431213,
    269508,
    162481
  )
hr$brojStanovnika <- brojstanovnika

hr$zupanije_id <- zupanije_id
##### data scientist kaze nema na cemu
hr$rodeni_2021 <- rodeni %>% select("2021")
hr$umrli_2021 <- umrli %>% select("2021")
hr$odseljeni_2021 <- odseljeni %>% select("2021")
hr$doseljeni_2021 <- doseljeni %>% select("2021")
hr$stanovnistvo_2021 <- stanovnistvo %>% select("2021")
hr$brakovi_2021 <- brakovi %>% select("2021")
hr$razvodi_2021 <- razvodi %>% select("2021")

ui <- fluidPage(
  setBackgroundColor(
    color = c("#2171B5", "#F7FBFF", "#FF0000"),
    gradient = "linear",
    direction = "left"
  ),
  titlePanel(title = h1(
    "CroStats", align = "center", style = "font-size:60px"
  )),
  sidebarLayout(
    mainPanel(tmapOutput(outputId = "map", height = 900),
              width = 5),
    sidebarPanel(
      #h1("Prikaz podataka"),
      fluidRow(
        column(
          3, 
          selectInput(
            "selectPrikaz",
            label = h3("Kategorija:"),
            choices = list(
              "Rodeni" = 1,
              "Umrli" = 2,
              "Odseljeni" = 3,
              "Doseljeni" = 4,
              "Broj stanovnika" = 5,
              "Sklopljeni brakovi" = 6,
              "Razvodi" = 7
            )
          )
        ),
      
        column(
          8,
          numericRangeInput(
            "razdoblje",
            label = h3("Razdoblje:"),
            value = c(1998, 2021),
            separator = "-"
          )
        )
      ),
      
      
      plotlyOutput("plot"),
      
      h2("Fun Fact 1", style = "font-size:30px"),
      helpText("Serman je jedina vredela od svih", style = "font-size:20px"),
      h2("Fun Fact 2", style = "font-size:30px"),
      helpText("Facts prvi Fun Fact 1", style = "font-size:20px"),
      h2("Fun Fact 3", style = "font-size:30px"),
      helpText("https://youtu.be/SlWq9Nag5TQ", style = "font-size:20px"),
      width = 7
    )
  )
)

server <- function(input, output, session) {
  globalPlotType <- 1
  
  razdoblje <- c(1998,2021)
  
  globalOdabraneZupanije <- c("Grad.Zagreb")
  
  filtriranaZupanija <-
    umrli_rodeni_brakovi_razvodi %>% filter(zupanije_id %in% c("Grad.Zagreb")) %>% filter(Godine >= razdoblje[1] & Godine <= razdoblje[2])
  
  prvaFiltriranaZupanija <-
    umrli_rodeni_brakovi_razvodi %>% filter(zupanije_id %in% c("Grad.Zagreb")) %>% filter(Godine >= razdoblje[1] & Godine <= razdoblje[2])
  
  initial_plot <- changePlotType(1, prvaFiltriranaZupanija)
  
  output$plot <- renderPlotly(initial_plot)
  
  hr <-
    hr %>% mutate(bojaj = ifelse(zupanije_id %in% c("Grad.Zagreb"),
                                 Zupanija,
                                 NA))
  
  Mypal <- c('#313695', '#fee090', '#d73027', '#72001a')
  
  #col = stupac u hr-u koji boja zupanije
  #pallete = paleta boja... to cemo lako stilki namjestiti
  #showNA = F znaci da se u legendi prikazuju samo odabrane zupanije
  
  output$map <- renderTmap({
    tm_shape(hr) + tm_borders() + tm_layout(title = "Republika Hrvatska") + tm_polygons(
      col = "brojStanovnika",
      popup.vars = c("Sjediste" = "SjedisteZupanije", "Broj stanovnika" = "brojStanovnika"),
      title = "Broj Stanovnika",
      breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000,400000,450000,800000)
    )
  })
  
 
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    novi_id <- click$id
    
    if (novi_id %in% globalOdabraneZupanije &
        length(globalOdabraneZupanije) > 1) {
      globalOdabraneZupanije <<-
        globalOdabraneZupanije[!globalOdabraneZupanije == novi_id]
    } else if (novi_id %in% globalOdabraneZupanije &
               length(globalOdabraneZupanije) == 1) {
    } else {
      globalOdabraneZupanije <<- c(globalOdabraneZupanije, novi_id)
    }
    
    if (length(globalOdabraneZupanije) >= 5) {
      globalOdabraneZupanije <<- c(globalOdabraneZupanije[5])
    }
    
    
    hr <-
      hr %>% mutate(bojaj = ifelse(zupanije_id %in% globalOdabraneZupanije,
                                   Zupanija,
                                   NA))
    
    
    tmp <- NULL
    if (globalPlotType %in% c(1,2,6,7)) {
      tmp <- umrli_rodeni_brakovi_razvodi
    } else if (globalPlotType %in% c(3,4)) {
      tmp <- odseljeni_doseljeni
    } else if (globalPlotType == 5) {
      tmp <- stanovnistvo1
    } 
    
    filtriranaZupanija <<- 
      tmp %>% filter(zupanije_id %in% globalOdabraneZupanije) %>% filter(Godine >= razdoblje[1] & Godine <= razdoblje[2])
    
    newPlot <- changePlotType(globalPlotType, filtriranaZupanija)
    
    output$plot <- renderPlotly(newPlot)
  })
  
  #### KATEGORIJE
  observeEvent(input$selectPrikaz, {
    globalPlotType <<- as.numeric(input$selectPrikaz)
    
    tmp <- NULL
    if (globalPlotType %in% c(1,2,6,7)) {
      tmp <- umrli_rodeni_brakovi_razvodi
    } else if (globalPlotType %in% c(3,4)) {
      tmp <- odseljeni_doseljeni
    } else if (globalPlotType == 5) {
      tmp <- stanovnistvo1
    } 
    
    filtriranaZupanija <<- 
      tmp %>% filter(zupanije_id %in% globalOdabraneZupanije) %>% filter(Godine >= razdoblje[1] & Godine <= razdoblje[2])
    
    # KAD SE UPDATEA HR ONDA CE SE MIJENJATI MAPA
    # tmapProxy("map", session, {
    #   tm_shape(hr) + tm_borders() + tm_layout(title = "Republika Hrvatska") + tm_polygons(
    #     col = "brojStanovnika",
    #     popup.vars = c("Sjediste" = "SjedisteZupanije", "Broj stanovnika" = "brojStanovnika"),
    #     title = "Zupanija",
    #   )
    # })
    
    changePlotType(globalPlotType, filtriranaZupanija) -> newPlot
    
    output$plot <- renderPlotly(newPlot)
  })
  
  ### RAZDOBLJE
  observeEvent(input$razdoblje, {
    ### treba ispravnu validaciju odradit
    razdoblje[1] <<- ifelse(is.na(input$razdoblje[1]), 1998, ifelse(input$razdoblje[1] > 2021, 2021, input$razdoblje[1]))
    razdoblje[2] <<- ifelse(is.na(input$razdoblje[2]), 2021, ifelse(input$razdoblje[2] < 1998, 1998, input$razdoblje[2]))
    
    razdoblje <<- sort(razdoblje)
    
    ### bitno da je <- da se ne mijenja izvorna varijabla
    filtriranaZupanija <- filtriranaZupanija %>% filter(Godine >= razdoblje[1] & Godine <= razdoblje[2])
    
    changePlotType(globalPlotType, filtriranaZupanija) -> newPlot
    output$plot <- renderPlotly(newPlot)
  })
  
}

shinyApp(ui = ui, server = server)
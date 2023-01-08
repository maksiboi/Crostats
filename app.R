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
library(shinythemes)
library(webshot)

#ucitavanje podataka
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
fun_facts <- read_excel("dataset.xlsx", sheet = "FunFacts")
#konstante
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

#pridruzivanje podataka varijablama
stanovnistvo <- cbind(zupanije_id, stanovnistvo[-1,])

doseljeni <- cbind(zupanije_id, doseljeni)

odseljeni <- cbind(zupanije_id, odseljeni)

rodeni <- cbind(zupanije_id, rodeni[-1,])

umrli <- cbind(zupanije_id, umrli[-1,])

brakovi <- cbind(zupanije_id, brakovi[-1,])

razvodi <- cbind(zupanije_id, razvodi[-1,])

e_gradani <- e_gradani[-1,]

rodeni1 <-
  pivot_longer(rodeni,
               "1998":"2021" ,
               names_to = "Godine",
               values_to = "rodeni")

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

umrli_rodeni_brakovi_razvodi <-
  cbind(
    rodeni1,
    "umrli" = umrli1$umrli,
    "brakovi" = brakovi1$brakovi,
    "razvodi" = razvodi1$razvodi
  )

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

hr <- st_read("fileZaCrtanje2/zupanije.shp")


hr$Zupanija <- zupanije


hr$SjedisteZupanije <- sjediste


hr$brojStanovnika <- brojstanovnika

hr$zupanije_id <- zupanije_id
hr$rodeni_2021 <- rodeni %>% select("2021") %>% pull("2021")
hr$umrli_2021 <- umrli %>% select("2021") %>% pull("2021")
hr$odseljeni_2021 <- odseljeni %>% select("2021") %>% pull("2021")
hr$doseljeni_2021 <- doseljeni %>% select("2021") %>% pull("2021")
hr$stanovnistvo_2021 <-
  stanovnistvo %>% select("2021") %>% pull("2021")
hr$brakovi_2021 <- brakovi %>% select("2021") %>% pull("2021")
hr$razvodi_2021 <- razvodi %>% select("2021") %>% pull("2021")

#postavke za tmap
tmap_mode("view")
options(scipen = 999)
set.seed(101)


#custom funkcije
changePlotType <- function(plotType, filtriranaZupanija) {
  if (plotType == 1) {
    stanovnistvo_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = stanovnistvo, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      ylab("Broj stanovnistva") +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.background = element_rect(
          size = 0.5,
          linetype = "solid",
          colour = "black"
        )
      ) +
      ggtitle("Broj stanovnika")
    
    
  } else if (plotType == 2) {
    rodeni_barplot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = rodeni, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.background = element_rect(
          size = 0.5,
          linetype = "solid",
          colour = "black"
        )
      ) +
      ylab("Broj rođenih") +
      ggtitle("Broj stanovnika")
    
    rodeni_barplot %>% ggplotly
    
  } else if (plotType == 3) {
    umrli_barplot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = umrli, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.background = element_rect(
          size = 0.5,
          linetype = "solid",
          colour = "black"
        )
      ) +
      ylab("Broj umrlih") +
      ggtitle("Broj stanovnika")
    
    umrli_barplot %>% ggplotly
    
    
  } else if (plotType == 4) {
    doseljeni_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = doseljeni, color = Zupanije)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      ylab("Broj doseljenog stanovnistva") +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.background = element_rect(
          size = 0.5,
          linetype = "solid",
          colour = "black"
        )
      ) +
      ggtitle("Broj stanovnika")
    
  } else if (plotType == 5) {
    odseljeni_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = odseljeni, color = Zupanije)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.background = element_rect(
          size = 0.5,
          linetype = "solid",
          colour = "black"
        )
      ) +
      ylab("Broj odseljenog stanovništva") +
      ggtitle("Broj stanovnika")
    
  } else if (plotType == 6) {
    brakovi_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = brakovi, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      ylab("Broj sklopljenih brakova") +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.background = element_rect(
          size = 0.5,
          linetype = "solid",
          colour = "black"
        )
      )
  } else if (plotType == 7) {
    razvodi_plot <-
      ggplot(filtriranaZupanija,
             aes(x = Godine, y = razvodi, color = Zupanija)) +
      geom_point() +
      geom_line(aes(group = 1))  +
      ylab("Broj razvoda ") +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.background = element_rect(
          size = 0.5,
          linetype = "solid",
          colour = "black"
        )
      )
  }
  
}

changeMapCategory <- function(plotType) {
  if (plotType == 1) {
    "stanovnistvo_2021"
  } else if (plotType == 2) {
    "rodeni_2021"
  } else if (plotType == 3) {
    "umrli_2021"
  } else if (plotType == 4) {
    "doseljeni_2021"
  } else if (plotType == 5) {
    "odseljeni_2021"
  } else if (plotType == 6) {
    "brakovi_2021"
  } else if (plotType == 7) {
    "razvodi_2021"
  }
}

changeTitleName <- function(plotType) {
  if (plotType == 1) {
    "Stanovništvo"
  } else if (plotType == 2) {
    "Broj rođenih"
  } else if (plotType == 3) {
    "Broj umrlih"
  } else if (plotType == 4) {
    "Broj doseljenih"
  } else if (plotType == 5) {
    "Broj odseljenih"
  } else if (plotType == 6) {
    "Broj sklopljenih brakova"
  } else if (plotType == 7) {
    "Broj razvoda"
  }
}


changeData <- function(plotType) {
  if (plotType == 1) {
    stanovnistvo1
  } else if (plotType == 2) {
    rodeni1
  } else if (plotType == 3) {
    umrli1
  } else if (plotType == 4) {
    doseljeni1
  } else if (plotType == 5) {
    odseljeni1
  } else if (plotType == 6) {
    brakovi1
  } else if (plotType == 7) {
    razvodi1
  }
}

changeMapBreaks <- function(plotType) {
  if (plotType == 1) {
    c(0, 50000, 100000, 150000, 250000, 450000, 800000) # stanovnistvo
  } else if (plotType == 2) {
    c(0, 1000, 2000, 3000, 4000, 5000, 8500) # rodeni
  } else if (plotType == 3) {
    c(0, 2000, 4000, 6000, 8000, 10000, 15000) # umrli
  } else if (plotType == 4) {
    c(0, 3000, 6000, 9000, 12000, 15000, 20000) # doseljeni
  } else if (plotType == 5) {
    c(0, 2500, 4000, 8000, 12000, 16000, 20000) # odseljeni
  } else if (plotType == 6) {
    c(0, 400, 800, 1200, 1600, 2000, 4000) # brakovi
  } else if (plotType == 7) {
    c(0, 100, 200, 300, 400, 500, 1100) # razvodi
  }
}

#Shiny UI
ui <- fluidPage(
  title = "CroStats",
  theme = shinytheme("paper"),
  titlePanel(title = h1(
    "CroStats", align = "center", style = "font-size:60px"
  )),
  fluidRow(column(
    6,
    tags$head(tags$style(
      HTML(
        "
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300&display=swap');
      h1 {
        font-family: 'Yusei Magic', sans-serif;
        color: black;
        margin-bottom: 0;
      }
      h2 {
        font-family: 'Yusei Magic', sans-serif;
        color: black;
      }
      h3 {
        font-family: 'Yusei Magic', sans-serif;
        color: black;
      }
      h6 {
        font-family: 'Yusei Magic', sans-serif;
        color: black;
      }
      .shiny-text-output {
        font-family: 'Roboto', sans-serif;
            color: black;
      }
      .well {
        padding-top: 0px;
      }
      .selectize-input {
        background-color: #f5f5f5;
      }
        "
      )
    )),
    h6("Kliknite na županiju kako biste vidjeli podatke o njoj"),
  )),
  sidebarLayout(
    mainPanel(tmapOutput(outputId = "map", height = 700),
              width = 5),
    sidebarPanel(
      fluidRow(column(
        3,
        selectInput(
          "selectPrikaz",
          label = h3("Kategorija:"),
          choices = list(
            "Broj stanovnika" = 1,
            "Rodeni" = 2,
            "Umrli" = 3,
            "Doseljeni" = 4,
            "Odseljeni" = 5,
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
      )),
      fluidRow(column(
        6,
        h6("Kliknite na prvu ikonu u zaglavlju prikaza za preuzimanje"),
      )),
      
      plotlyOutput("plot"),
      br(),
      span(# downloadButton("downloadPlot", "Preuzmite prikaz"),
        downloadButton("downloadData", "Preuzmite podatke")),
      
      h2("Fun Facts", style = "font-size:30px"),
      span(textOutput("prva"), style = "font-size:20px"),
      span(textOutput("druga"), style = "font-size:20px"),
      span(textOutput("treca"), style = "font-size:20px"),
      span(textOutput("cetvrta"), style = "font-size:20px"),
      width = 7
    )
  )
)

server <- function(input, output, session) {
  globalPlotType <- 1
  
  razdoblje <- c(1998, 2021)
  
  globalOdabraneZupanije <- c("Grad.Zagreb")
  
  odabrani <- data.frame()
  
  output$prva <- renderText({
    set.seed(Sys.time())
    fun_facts %>% filter(zupanije_id %in% globalOdabraneZupanije) %>% sample_n(1) -> pom
    odabrani <<- rbind(odabrani, pom)
    paste("• ", odabrani[1, ]$fact)
  })
  
  filtriranaZupanija <-
    stanovnistvo1 %>% filter(zupanije_id %in% globalOdabraneZupanije) %>% filter(Godine >= razdoblje[1] &
                                                                                   Godine <= razdoblje[2])
  
  initial_plot <- changePlotType(1, filtriranaZupanija)
  
  output$plot <- renderPlotly(initial_plot)
  
  output$map <- renderTmap({
    tm_shape(hr) + tm_borders() + tm_layout(title = "Republika Hrvatska") + tm_polygons(
      col = "stanovnistvo_2021",
      popup.vars = c("Sjediste" = "SjedisteZupanije", "Broj stanovnika" = "brojStanovnika"),
      title = "Broj stanovnika",
      breaks = c(0, 50000, 100000, 150000, 250000, 450000, 800000)
    )
  })
  
  #sprjecava dvostruko renderiranje na pocetnom loadanju stranice
  freeToRender <- F
  
  
  ### KLIK NA DRZAVU
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    novi_id <- click$id
    
    set.seed(Sys.time())
    #osigurava da minimalno 1 zupanija bude odabrana i dodaje novu zupaniju
    if (novi_id %in% globalOdabraneZupanije &
        length(globalOdabraneZupanije) > 1) {
      globalOdabraneZupanije <<-
        globalOdabraneZupanije[!globalOdabraneZupanije == novi_id]
      ### ff implementacija
      odabrani <<- odabrani %>% filter(zupanije_id != novi_id)
    } else if (novi_id %in% globalOdabraneZupanije &
               length(globalOdabraneZupanije) == 1) {
      
    } else {
      globalOdabraneZupanije <<- c(globalOdabraneZupanije, novi_id)
      
      #### ff implementacija
      zupanija_visak <- globalOdabraneZupanije[1]
      set.seed(Sys.time())
      if (nrow(odabrani) == 4) {
        odabrani <<- odabrani %>% filter(zupanije_id != zupanija_visak)
        fun_facts %>% filter(novi_id == zupanije_id) %>% sample_n(1) -> pom
        odabrani <<- rbind(odabrani, pom)
      } else {
        fun_facts %>% filter(novi_id == zupanije_id) %>% sample_n(1) -> pom
        odabrani <<- rbind(odabrani, pom)
      }
    }
    
    
    #implementira da se zupanije ponasaju kao red
    if (length(globalOdabraneZupanije) >= 5) {
      globalOdabraneZupanije <<- c(globalOdabraneZupanije[-1])
    }
    
    if (!is.na(odabrani[1, ]$fact)) {
      output$prva <- renderText({
        paste("• ", odabrani[1, ]$fact)
      })
    } else {
      output$prva <- renderText({
        " "
      })
    }
    
    if (!is.na(odabrani[2, ]$fact)) {
      output$druga <- renderText({
        paste("• ", odabrani[2, ]$fact)
      })
    } else {
      output$druga <- renderText({
        " "
      })
    }
    
    
    if (!is.na(odabrani[3, ]$fact)) {
      output$treca <- renderText({
        paste("• ", odabrani[3, ]$fact)
      })
    } else {
      output$treca <- renderText({
        " "
      })
    }
    
    if (!is.na(odabrani[4, ]$fact)) {
      output$cetvrta <- renderText({
        paste("• ", odabrani[4, ]$fact)
      })
    } else {
      output$cetvrta <- renderText({
        " "
      })
    }
    
    hr <-
      hr %>% mutate(bojaj = ifelse(zupanije_id %in% globalOdabraneZupanije,
                                   Zupanija,
                                   NA))
  
    tmp <- NULL
    if (globalPlotType %in% c(2, 3, 6, 7)) {
      tmp <- umrli_rodeni_brakovi_razvodi
    } else if (globalPlotType %in% c(4, 5)) {
      tmp <- odseljeni_doseljeni
    } else if (globalPlotType == 1) {
      tmp <- stanovnistvo1
    }
    
    filtriranaZupanija <<-
      tmp %>% filter(zupanije_id %in% globalOdabraneZupanije) %>% filter(Godine >= razdoblje[1] &
                                                                           Godine <= razdoblje[2])
    
    newPlot <- changePlotType(globalPlotType, filtriranaZupanija)
    
    output$plot <- renderPlotly(newPlot)
  })
  
  ### KATEGORIJA
  observeEvent(input$selectPrikaz, {
    # kad se aplikacija prvi put loada, ovaj blok se triggera ali nije potrebno izvrsiti ga
    if (!freeToRender) {
      freeToRender <<- T
      return()
    }
    
    globalPlotType <<- as.numeric(input$selectPrikaz)
    
    tmp <- NULL
    if (globalPlotType %in% c(2, 3, 6, 7)) {
      tmp <- umrli_rodeni_brakovi_razvodi
    } else if (globalPlotType %in% c(4, 5)) {
      tmp <- odseljeni_doseljeni
    } else if (globalPlotType == 1) {
      tmp <- stanovnistvo1
    }
    
    filtriranaZupanija <<-
      tmp %>% filter(zupanije_id %in% globalOdabraneZupanije) %>% filter(Godine >= razdoblje[1] &
                                                                           Godine <= razdoblje[2])
    
    changePlotType(globalPlotType, filtriranaZupanija) -> newPlot
    
    output$plot <- renderPlotly(newPlot)
    
    #prikaz boja na mapi ovisno o kategoriji
    currentCol <- changeMapCategory(globalPlotType)
    currentTitle <- changeTitleName(globalPlotType)
    currentBreaks <- changeMapBreaks(globalPlotType)
    tmapProxy("map", session, {
      tm_shape(hr) + tm_borders() + tm_layout(title = "Republika Hrvatska") + tm_polygons(
        col = currentCol,
        popup.vars = c("Sjediste" = "SjedisteZupanije", "Broj stanovnika" = "brojStanovnika"),
        title = currentTitle,
        breaks = currentBreaks
      )
    })
  })
  
  ### RAZDOBLJE
  observeEvent(input$razdoblje, {
    razdoblje[1] <<-
      ifelse(
        is.na(input$razdoblje[1]),
        1998,
        ifelse(input$razdoblje[1] > 2021, 2021, input$razdoblje[1])
      )
    razdoblje[2] <<-
      ifelse(
        is.na(input$razdoblje[2]),
        2021,
        ifelse(input$razdoblje[2] < 1998, 1998, input$razdoblje[2])
      )
    
    razdoblje <<- sort(razdoblje)
    
    filtriranaZupanija <-
      filtriranaZupanija %>% filter(Godine >= razdoblje[1] &
                                      Godine <= razdoblje[2])
    
    changePlotType(globalPlotType, filtriranaZupanija) -> newPlot
    output$plot <- renderPlotly(newPlot)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(gsub(" ", "_", changeTitleName(globalPlotType)), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(changeData(globalPlotType), file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
###############################################################################
#Przygotowanie srodowiska
#install.packages('shiny')
remove(list = ls())
cat('\014')
library(pacman)
p_load(shiny, ggplot2)

##############################################################################
#Backend aplikacji - tworzenie wykresow

server <- function(input, output) {
  #Przygotowanie danych
  dane <- read.csv(file = 'ceny_samochodow.csv', header = T, sep = ',')
  dane$bezwypadkowy <- factor(dane$bezwypadkowy, labels = c("Nie", "TAK"))
  dane$wlasciciel <- factor(dane$wlasciciel, labels = c("Nie", "TAK"))
  dane$paliwo <- factor(dane$paliwo, labels = c("Nie", "TAK"))
  dane$skrzynia <- factor(dane$skrzynia, labels = c("Nie", "TAK"))
  dane$pojemnosc <- as.factor(round(dane$pojemnosc, digits = -2))
  dane$produkcja <- as.factor(dane$produkcja)
  
  #Funkcje do tworzenia wykresow
  ciagla <- function(width, x, title){
    return(ggplot(dane, aes(x)) + 
             geom_histogram(binwidth = width) + 
             scale_x_continuous(name = title) +
             scale_y_continuous(name = 'Liczba') +
             theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
                   axis.text.x = element_text(colour = 'black'), axis.text.y = element_text(colour = 'black')))
  }
  
  dyskretna <- function(x, title){
    return(ggplot(dane, aes(x)) + 
             geom_bar(aes(x, fill = x)) + 
             scale_x_discrete(name = title) +
             scale_y_continuous(name = 'Liczba')+
             scale_fill_manual(values=gray(length(levels(x)):1/(length(levels(x)) + 1))) +
             guides(fill=guide_legend(title="")) +
             theme(panel.background = element_rect(fill = 'white', colour = 'black'),
                   axis.text.x = element_text(colour = 'black'), axis.text.y = element_text(colour = 'black')))
  }
  
  zaleznosc_dyskretna <- function(x, title){
    ggplot(dane, aes(x, cena, fill = x)) +
      geom_boxplot(outlier.colour = "black", outlier.size = 3) +
      scale_y_continuous(name="Cena") +
      scale_x_discrete(name= title, labels = c('0' = 'Tak', '1' = 'Nie')) + 
      scale_fill_manual(values=gray(length(levels(x)):1/(length(levels(x)) + 1))) +
      guides(fill=guide_legend(title="")) +
      theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
            axis.text.x = element_text(colour = 'black'), axis.text.y = element_text(colour = 'black')) 
  }
  
  zaleznosc_ciagla <- function(x, title){
    ggplot(dane, aes(x, cena)) +
      geom_point(aes(x, cena)) + 
      scale_x_continuous(name = title) +
      scale_y_continuous(name = "Cena") +
      theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
            axis.text.x = element_text(colour = 'black'), axis.text.y = element_text(colour = 'black'))  
  }
  
  skaluj <- function(x){
    if(x < .2){
      return(1)
    } else if (x < .4){
      return(2)
    } else if(x < .6){
      return(3)
    } else {
      return(4)
    }
  }
  kolor <- sapply(X = (dane$cena - min(dane$cena))/max(dane$cena), FUN = skaluj)
  kolor <- factor(gray((4 - kolor) / 4))
  
  babelkowy <- function(kategoryczna, ciagla, tytul_k, tytul_c){
    return(
      ggplot(dane, aes(x=kategoryczna, y=ciagla, size=(cena - min(cena))/max(cena), fill = kolor,label = ""), guide=F) +
        geom_point(shape = 21) +
        scale_size_area(max_size = 10) +
        scale_x_discrete(name=tytul_k) +
        scale_y_continuous(name=tytul_c) +
        scale_fill_manual(values = levels(kolor)) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              axis.text.x = element_text(colour = 'black'), axis.text.y = element_text(colour = 'black'),
              legend.position = 'none') +
        geom_jitter()
    )
  }
  
  #Odpowiedzi na interakcje
  output$Zmienna <- renderPlot({
    switch(input$variable,
           cena = ciagla(10000, dane$cena, "Cena"),
           przebieg = ciagla(10000, dane$przebieg, "Przebieg"),
           moc = ciagla(20, dane$moc, "Moc"),
           pojemnosc = dyskretna(dane$pojemnosc, "Pojemnosc"),
           produkcja = dyskretna(dane$produkcja, "Produkcja"),
           bezwypadkowy = dyskretna(dane$bezwypadkowy, "Czy bezwypadkowy?"),
           wlasciciel = dyskretna(dane$wlasciciel, "Czy sprzedaje pierwszy wlasciciel?"),
           skrzynia = dyskretna(dane$skrzynia, "Czy auto ma automatyczna skrzynie biegow?"),
           paliwo = dyskretna(dane$paliwo, "Czy auto jest na Diesel?")
    )
  })
  output$Zmienna2 <- renderPlot({
    switch(input$variable2,
           moc = zaleznosc_ciagla(dane$moc, "Moc"),
           przebieg = zaleznosc_ciagla(dane$przebieg, "Przebieg"),
           pojemnosc = zaleznosc_dyskretna(dane$pojemnosc, "Pojemnosc silnika"),
           produkcja = zaleznosc_dyskretna(dane$produkcja, "Rok produkcji"),
           bezwypadkowy = zaleznosc_dyskretna(dane$bezwypadkowy, "Czy bezwypadkowy?"),
           wlasciciel = zaleznosc_dyskretna(dane$wlasciciel, "Czy sprzedaje pierwszy wlasciciej?"),
           skrzynia = zaleznosc_dyskretna(dane$skrzynia, "Czy auto ma automatyczna skrzynie biegow?"),
           paliwo = zaleznosc_dyskretna(dane$paliwo, "Czy auto ma Diesla?")
    )
  })
  output$Zmienna3 <- renderPlot({
    switch(paste(input$variable3, input$variable4, sep = ""),
           "11" = babelkowy(dane$produkcja, dane$moc, 'Produkcja', 'Moc'),
           "12" = babelkowy(dane$produkcja, dane$przebieg, 'Produkcja', 'Przebieg'),
           "21" = babelkowy(dane$pojemnosc, dane$moc, 'Pojemnosc', 'Moc'),
           "22" = babelkowy(dane$pojemnosc, dane$przebieg, 'Pojemnosc', 'Przebieg'),
           "31" = babelkowy(dane$bezwypadkowy, dane$moc, 'Czy bezwypadkowy?', 'Moc'),
           "32" = babelkowy(dane$bezwypadkowy, dane$przebieg, 'Czy bezwypadkowy?', 'Przebieg'),
           "41" = babelkowy(dane$wlasciciel, dane$moc, 'Czy sprzedaje pierwszy wlasciciel?', 'Moc'),
           "42" = babelkowy(dane$wlasciciel, dane$przebieg, 'Czy sprzedaje pierwszy wlasciciel?', 'Przebieg'),
           "51" = babelkowy(dane$paliwo, dane$moc, 'Czy auto ma Diesla?', 'Moc'),
           "52" = babelkowy(dane$paliwo, dane$przebieg, 'Czy auto ma Diesla?', 'Przebieg'),
           "61" = babelkowy(dane$skrzynia, dane$moc, 'Czy auto ma automatyczna skrzynie biegow?', 'Moc'),
           "62" = babelkowy(dane$skrzynia, dane$przebieg, 'Czy auto ma automatyczna skrzynie biegow?', 'Przebieg')
    )
  })
}

###############################################################################
#Wyglad aplikacji

ui <- fluidPage(
  titlePanel("Wizualizacja zbioru ceny_samochodow"),
  fluidRow(
    p('Autor: Mateusz Dolinski')
  ),
  sidebarLayout(position = "left",
                sidebarPanel( 
                  p("Wykresy pojedynczych cech ze zbioru danych"),
                  selectInput(inputId = "variable", label = "Wybierz nazwe zmiennej", 
                              choices = c("cena", "produkcja", "pojemnosc", "moc", "przebieg",
                                          "bezwypadkowy", "wlasciciel", "paliwo", "skrzynia"))
                ),
                mainPanel(
                  plotOutput('Zmienna')
                )
  ),
  sidebarLayout(position = "left",
                sidebarPanel( 
                  p("Wykresy zaleznosci pomiedzy cena samochodu a pozostalymi zmiennymi:"),
                  selectInput(inputId = "variable2", label = "Wybierz nazwe zmiennej objasniajacej", 
                              choices = c("produkcja", "pojemnosc", "moc", "przebieg",
                                          "bezwypadkowy", "wlasciciel", "paliwo", "skrzynia"))
                ),
                mainPanel(
                  plotOutput('Zmienna2')
                )
  ),
  sidebarLayout(position = 'left',
                sidebarPanel(
                  p("Wykres babelkowy (im wiekszy babelek, tym wyzsza cena samochodu):"),
                  radioButtons("variable3", label = h3("Zmienna dyskretna"),
                               choices = list("Produkcja" = 1, "Pojemnosc" = 2,"Bezwypadkowy" = 3,
                                              "Wlasciciel" =4, "Paliwo" = 5, "Skrzynia" = 6),selected = 1),
                  radioButtons("variable4", label = h3("Zmienna ciagla"),
                               choices = list("Moc" = 1, "Przebieg" = 2),selected = 1)
                ),
                mainPanel(
                  plotOutput('Zmienna3')
                )
  )
)

#############################################################################
#Wlaczenie aplikacji
shinyApp(ui = ui, server = server)
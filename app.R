library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)
library(stringi)
library(plotly)
library(shinydashboardPlus)
library(corrr)
library(ggcorrplot)

ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Środki ochrony roślin"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stan obecny", tabName = "obecnie", icon = icon("seedling")),
      menuItem("Zmiana w czasie", tabName = "zmiany", icon = icon("chart-line")),
      menuItem("Podobieństwo zmian", tabName = "podobienstwo", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "obecnie",
            fluidRow(
                     box(plotlyOutput(outputId = "stan_obecny_liczebnosc_sor") , footer = em("Źródło: Portal dane.gov.pl, \"rejestr podstawowy - 03.08.2020 r.\"."), width = 12)
            )
      ),
      tabItem(tabName = "zmiany",
            fluidRow(
                     box(plotlyOutput(outputId = "zmiany_w_czasie"), footer = em("Źródło: Portal dane.gov.pl, \"Środki ochrony roślin usunięte z rejestru - lipiec 2020 r.\" oraz \"Wykaz zezwoleń: lipiec 2020 r.\"."), width = 10),
                     box(title = NULL, checkboxInput(inputId = "wszystko_zmiana_w_czasie", label = "Pokaż całość"), width = 2, background = "yellow")
            ),
            fluidRow(
              box(plotlyOutput(outputId = "zmiany_w_czasie_skumulowane"), footer = em("Źródło: Portal dane.gov.pl, \"Środki ochrony roślin usunięte z rejestru - lipiec 2020 r.\" oraz \"Wykaz zezwoleń: lipiec 2020 r.\"."), width = 10),
              box(title = NULL, checkboxInput(inputId = "wszystko_zmiana_w_czasie_skumulowane", label = "Pokaż całość"), width = 2, background = "yellow")
            ),
            fluidRow(boxComment(textOutput(outputId = "zmiany_wyjasnienie"), title = em("Wyjaśnienie: wartości nie sumują się do stanu obecnego, ponieważ monitorowanie zmian nie przebiega od początku pojawienia się ŚOR na rynku.", style = "color:grey;")))
      ),
      tabItem(tabName = "podobienstwo",
              fluidRow(
                box(plotlyOutput(outputId = "podobienstwo_zmian"), width = 8, footer = "Przekreślono korelacje nieistotne statystycznie"),
                boxComment(title = p("Do analizy wybrano rodzaje ŚOR, które, biorąc pod uwagę ich zmianę co miesiąc (przyrost/spadek), miały mniej niż 2/3 zer (oznaczających brak zmian w liczebności w danym miesiącu).
                           Współczynnik korelacji pokazuje, na ile zmienność jednego rodzaju ŚOR współwystępuje ze zmiennością innego rodzaju ŚOR, tj. na ile zmiany (wzrost lub spadek) jednego rodzaju ŚOR biegną w tym samym kierunku (siła: 1) lub przeciwnym (siła: -1), co zmiany drugiego rodzaju ŚOR. Siła korelacji okazała się dodatnia, słaba lub co najwyżej słaba-umiarkowana (w przypadku pary Fungicyd-Chwastobójczy).
                           Oznacza to, że nie da się powiedzieć, iż zmiany jednego rodzaju ŚOR są w jakiś sposób powiązane ze zmianami innego rodzaju ŚOR.", style = "color:grey"))
              )
              )
    )
  ),
  tags$head(tags$style(HTML('
      * {
        font-family: Lucida;
        font-size: 18px;
      }
    ')))
)

server <- function(input, output, session) {
  
  dane <- reactive({
    obecnie <- read_csv2("Rejestr_podstawowe.csv", locale = locale(encoding = stri_enc_get()))
    Rodzaj <- unlist(stri_split_fixed(obecnie$Rodzaj, ", "))
    obecnie <- tibble(Rodzaj = Rodzaj)
    
    miesiace <- c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec", "lipiec", "sierpień", "wrzesień", "październik", "listopad", "grudzień")
    
    zmiany_usuniete <- read_excel("sor_usuniete.xlsx", col_types = "text")
    zmiany_usuniete <- zmiany_usuniete %>%
      mutate(Poz = ifelse(stri_detect_regex(Poz, "^\\d+$"), NA, Poz)) %>%
      select(Data = Poz, Rodzaj) %>%
      fill(Data, .direction = "down") %>%
      filter(!is.na(Rodzaj)) %>%
      separate(Rodzaj, sep = ", ", into = as.character(1:max(stri_count_boundaries(zmiany_usuniete$Rodzaj), na.rm = TRUE))) %>%
      gather("usun", "Rodzaj", -Data) %>%
      select(-usun) %>%
      filter(!is.na(Rodzaj)) %>%
      separate(Data, into = c("Miesiąc", "Rok"), sep = " ") %>%
      mutate(Miesiąc = str_to_lower(Miesiąc),
             Miesiąc = match(Miesiąc, miesiace)) %>%
      unite("Data", c(Miesiąc, Rok), sep = "-15-") %>%
      mutate(Data = mdy(Data)) %>%
      arrange(Data) %>%
      filter(Data >= ymd("2013-06-01"))
    
    zmiany_zezwolenia <- read_excel("sor_zezwolenia.xlsx", col_types = "text")
    zmiany_zezwolenia <- zmiany_zezwolenia %>%
      rename(Poz = Lp, Rodzaj = `rodzaj środka ochrony roslin`) %>%
      mutate(Poz = ifelse(stri_detect_regex(Poz, "^\\d+$"), NA, Poz)) %>%
      select(Data = Poz, Rodzaj)
    
    zmiany_zezwolenia <- zmiany_zezwolenia %>%
      fill(Data, .direction = "down") %>%
      filter(!is.na(Rodzaj)) %>%
      separate(Rodzaj, sep = ", ", into = as.character(1:max(stri_count_boundaries(zmiany_zezwolenia$Rodzaj), na.rm = TRUE))) %>%
      gather("usun", "Rodzaj", -Data) %>%
      select(-usun) %>%
      filter(!is.na(Rodzaj)) %>%
      separate(Data, into = c("Miesiąc", "Rok"), sep = " ") %>%
      mutate(Miesiąc = str_to_lower(Miesiąc),
             Miesiąc = if_else(Miesiąc == "lipec", "lipiec", Miesiąc),
             Miesiąc = match(Miesiąc, miesiace)) %>%
      unite("Data", c(Miesiąc, Rok), sep = "-15-") %>%
      mutate(Data = mdy(Data)) %>%
      arrange(Data)
    
    liczebnosc_usuniete <- zmiany_usuniete %>%
      group_by(Rodzaj, Data) %>%
      summarise(Liczebność_usuniete = n())
    
    liczebnosc_zezwolenia <- zmiany_zezwolenia %>%
      group_by(Rodzaj, Data) %>%
      summarise(Liczebność_zezwolenia = n())
    
    wszystkie_daty <- sort(unique(c(zmiany_usuniete$Data, zmiany_zezwolenia$Data)))
    wszystkie_rodzaje <- sort(unique(c(zmiany_usuniete$Rodzaj, zmiany_zezwolenia$Rodzaj)))
    
    kombinacje <- CJ(wszystkie_daty, wszystkie_rodzaje)
    kombinacje <- kombinacje %>%
      select(Rodzaj = wszystkie_rodzaje, Data = wszystkie_daty)
    
    liczebnosc_laczona <- full_join(liczebnosc_usuniete, liczebnosc_zezwolenia)
    wszystko <- full_join(kombinacje, liczebnosc_laczona)
    zmiany_w_czasie <- wszystko %>%
      mutate(Liczebność_usuniete = if_else(is.na(Liczebność_usuniete), 0, as.numeric(Liczebność_usuniete)),
             Liczebność_zezwolenia = if_else(is.na(Liczebność_zezwolenia), 0, as.numeric(Liczebność_zezwolenia)),
             Roznica = Liczebność_zezwolenia - Liczebność_usuniete) %>%
      arrange(Rodzaj, Data) %>%
      group_by(Rodzaj) %>%
      mutate(Zmiany_skumulowane = cumsum(Roznica)) %>%
      ungroup(Rodzaj)
    
    obecnie_wykres <- obecnie %>%
      mutate(Rodzaj = fct_infreq(Rodzaj)) %>%
      group_by(Rodzaj) %>%
      summarise(Liczebność = n()) %>%
      ggplot(aes(Rodzaj, Liczebność)) +
      geom_col(fill = "#31795F") +
      theme_minimal() +
      labs(y = "Liczebność") +
      ggtitle("Środki ochrony roślin w podziale na rodzaje") +
      theme(axis.line = element_line(color = "#EDBD1D"),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.title.x = element_blank(),
            text = element_text(family = "Georgia", size = 11),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(hjust = 0.5))
    obecnie_wykres <- ggplotly(obecnie_wykres)
    
    wykres_zmiany_calosc <- zmiany_w_czasie %>%
      rename(Zmiana = Roznica) %>%
      mutate(Rodzaj = fct_relevel(Rodzaj, "Pozostały", after = Inf)) %>%
      group_by(Data) %>%
      summarise(Zmiana = sum(Zmiana)) %>%
      ggplot(aes(Data, Zmiana)) +
      geom_line(color = "#31795F") +
      geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
      theme_minimal() +
      labs(y = "Zmiana (zezwolenia - usunięcia)") +
      ggtitle("Środki ochrony roślin - zmiana w czasie w danym miesiącu") +
      theme(axis.line = element_line(color = "#EDBD1D"),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.title.x = element_blank(),
            text = element_text(family = "Georgia", size = 11),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%m-%Y", breaks = "6 months")
    wykres_zmiany_calosc <- ggplotly(wykres_zmiany_calosc)
    
    wykres_zmiany <- zmiany_w_czasie %>%
      rename(Zmiana = Roznica) %>%
      mutate(Rodzaj = fct_relevel(Rodzaj, "Pozostały", after = Inf)) %>%
      ggplot(aes(Data, Zmiana, color = Rodzaj)) +
      geom_line() +
      geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
      theme_minimal() +
      labs(y = "Zmiana (zezwolenia - usunięcia)") +
      ggtitle("Środki ochrony roślin - zmiana w czasie w danym miesiącu") +
      theme(axis.line = element_line(color = "#EDBD1D"),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.title.x = element_blank(),
            text = element_text(family = "Georgia", size = 11),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%m-%Y", breaks = "6 months")
    wykres_zmiany <- ggplotly(wykres_zmiany)
    
    wykres_zmiany_skumulowane_calosc <- zmiany_w_czasie %>%
      rename(Zmiana_skumulowana = Zmiany_skumulowane) %>%
      mutate(Rodzaj = fct_relevel(Rodzaj, "Pozostały", after = Inf)) %>%
      group_by(Data) %>%
      summarise(Zmiana_skumulowana = sum(Zmiana_skumulowana)) %>%
      ggplot(aes(Data, Zmiana_skumulowana)) +
      geom_line(color = "#31795F") +
      geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
      theme_minimal() +
      labs(y = "Liczebność") +
      ggtitle("Środki ochrony roślin - skumulowana zmiana w czasie") +
      theme(axis.line = element_line(color = "#EDBD1D"),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.title.x = element_blank(),
            text = element_text(family = "Georgia", size = 11),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%m-%Y", breaks = "6 months")
    wykres_zmiany_skumulowane_calosc <- ggplotly(wykres_zmiany_skumulowane_calosc)
    
    wykres_zmiany_skumulowane <- zmiany_w_czasie %>%
      rename(Zmiana_skumulowana = Zmiany_skumulowane) %>%
      mutate(Rodzaj = fct_relevel(Rodzaj, "Pozostały", after = Inf)) %>%
      ggplot(aes(Data, Zmiana_skumulowana, color = Rodzaj)) +
      geom_line() +
      geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
      theme_minimal() +
      labs(y = "Liczebność") +
      ggtitle("Środki ochrony roślin - skumulowana zmiana w czasie") +
      theme(axis.line = element_line(color = "#EDBD1D"),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.title.x = element_blank(),
            text = element_text(family = "Georgia", size = 11),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%m-%Y", breaks = "6 months")
    wykres_zmiany_skumulowane <- ggplotly(wykres_zmiany_skumulowane)
    
    zmiany_w_czasie_do_korelacji <- zmiany_w_czasie %>%
      select(Data, Rodzaj, Roznica) %>%
      group_by(Rodzaj) %>%
      mutate(usunac = mean(Roznica == 0) >= 0.66) %>%
      filter(usunac == FALSE) %>%
      select(-usunac) %>%
      spread(Rodzaj, Roznica) %>%
      select(-Data)
    
    korelacje <- corr <- round(cor(zmiany_w_czasie_do_korelacji), 2)
    istotnosc <- round(cor_pmat(zmiany_w_czasie_do_korelacji), 3)
    
    wykres_korelacje <- korelacje %>%
      ggcorrplot(type = "upper", title = "Korelacje między wybranymi rodzajami ŚOR", hc.order = TRUE, p.mat = istotnosc, colors = c("#6D9EC1", "white", "#E46726"), digits = 3, legend.title = "Współczynnik korelacji", ggtheme = theme_minimal(base_family = "Georgia"))
    wykres_korelacje <- ggplotly(wykres_korelacje)
    
    list(obecnie_wykres = obecnie_wykres, wykres_zmiany_calosc = wykres_zmiany_calosc, wykres_zmiany = wykres_zmiany, wykres_zmiany_skumulowane_calosc = wykres_zmiany_skumulowane_calosc, wykres_zmiany_skumulowane = wykres_zmiany_skumulowane, wykres_korelacje = wykres_korelacje)
  })
  
  output$stan_obecny_liczebnosc_sor <- renderPlotly({
    wykres <- dane()
    wykres <- wykres$obecnie_wykres
    wykres
  })
  
  output$zmiany_w_czasie <- renderPlotly({
    wykres <- dane()
    if (input$wszystko_zmiana_w_czasie) {
      wykres <- wykres$wykres_zmiany_calosc
      wykres
    } else {
      wykres <- wykres$wykres_zmiany
      wykres
    }
  })
  
  output$zmiany_w_czasie_skumulowane <- renderPlotly({
    wykres <- dane()
    if (input$wszystko_zmiana_w_czasie_skumulowane) {
      wykres <- wykres$wykres_zmiany_skumulowane_calosc
      wykres
    } else {
      wykres <- wykres$wykres_zmiany_skumulowane
      wykres
    }
  })
  
  output$podobienstwo_zmian <- renderPlotly({
    wykres <- dane()
    wykres <- wykres$wykres_korelacje
    wykres
  })
}

shinyApp(ui = ui, server = server)

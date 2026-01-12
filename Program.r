library(shiny)
library(dplyr)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
miesiace_pl <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")

dane <- read.csv("~/Desktop/projjekttwd/Program/goodreads_library_export .csv",sep = ";",header = TRUE)

przeczytane <- dane %>%
  filter(Date.Read != "")%>%
  mutate(Date.Read = as.Date(Date.Read, format = "%Y/%m/%d"))%>%
  mutate(year = as.integer(format(Date.Read, "%Y")),
         month = as.integer(format(Date.Read, "%m")))
oceny <- przeczytane %>%
  mutate(my_rating = as.numeric(My.Rating),
         avg_rating = as.numeric(Average.Rating)) %>%
  filter(!is.na(my_rating),
         !is.na(avg_rating))

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Analiza hobby"),
  dashboardSidebar(
    selectInput("wybranaOsoba", "Wybierz osobę:", choices = c("", "Gabriela", "Jędrzej","Karolina"), selected = ""),
    sidebarMenu(
      menuItem("Panel główny",tabName = "info", icon = icon("info-circle")),
      menuItem("Analizy", tabName = "analizy", icon = icon("chart-bar")))
  ),
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .skin-black .main-header .navbar,
        .skin-black .main-header .logo {
          background-color: #0f1113 !important;
          color: #ffffff !important;
        }
        .skin-black .main-sidebar {
          background-color: #14181c;
        }
        .skin-black .sidebar-menu > li > a {
          color: #cfd8dc;
        }
        .skin-black .sidebar-menu > li.active > a {
          background-color: #1f252b;
          border-left: 3px solid #4fc3f7;
        }
        .skin-black .main-header {
        position: fixed;
        width: 100%;
        }
        .skin-black .main-sidebar {
        position: fixed;
        }
        .content-wrapper{
        margin-top: 50px;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "info",
        h1("Informacje o projekcie"),
        p("Aplikacja prezentuje analizy danych dotyczących hobby osób w grupie projektowej."),
        p("Wybierz osobę i przejdź do zakładki „Analizy”."),
        hr(),
      ),
      
      tabItem(
        tabName = "analizy",
        
        # brak osoby
        conditionalPanel(
          condition = "input.wybranaOsoba == ''",
          h1("Wybierz osobę"),
          p("Aby zobaczyć analizy, wybierz osobę z panelu bocznego.")
        ),
        
        # Karolina
        conditionalPanel(
          condition = "input.wybranaOsoba == 'Karolina'",
          h2(""),
          
          fluidRow(
            column(
              4,
              pickerInput(
                "rok",
                "Wybierz rok:",
                choices = sort(unique(przeczytane$year)),
                selected = sort(unique(przeczytane$year)),
                multiple = TRUE
              ),
              
              pickerInput(
                "miesiac",
                "Wybierz miesiące:",
                choices = setNames(1:12, miesiace_pl),
                selected = 1:12,
                multiple = TRUE
              )
            ),
            
            column(
              8,
              fluidRow(
                column(
                  6,
                  uiOutput("autorzy_title"),
                  uiOutput("autorzy")
                ),
                column(
                  6,
                  uiOutput("title_wydawnictwa"),
                  uiOutput("wydawnictwa")
                )
              )
            )
          ),
          
          hr(),
          fluidRow(
              column(6, plotlyOutput("wykres1", height = 400)),
              column(6, plotlyOutput("wykres2", height = 400))
          ),
          
          tags$hr(style = "
            border-top: 1px solid #808080;
            margin: 25px 0;
          "),
          fluidRow(
              column(
                6,
                pickerInput(
                    "autor_rating",
                    "Wybierz autora: ",
                    choices = sort(unique(oceny$Author)),
                    selected = "",
                    options = list(`live-search` = TRUE)
                )
            ),
          fluidRow(
            column(
              12,
              plotlyOutput("wykres3", height = 400)
            )
          )
            )
        ),

        
        # inne osoby
        conditionalPanel(
          condition = "input.wybranaOsoba != '' && input.wybranaOsoba != 'Karolina'",
          uiOutput("naglowekOsoby"),
          p("Dla tej osoby analizy nie są jeszcze dostępne.")
        )
      )
    )
  )
)


server <- function(input, output, session){
  output$naglowekOsoby <- renderText({
    paste("Analizy –", input$wybranaOsoba)
  })
  
  dane_filtrowane <- reactive({
    req(input$rok, input$miesiac)
    przeczytane %>%
      filter(
        year %in% as.integer(input$rok),
        month %in% as.integer(input$miesiac)
      )
  })
  
  output$autorzy_title <- renderUI({
    req(input$rok)
    lata <- sort(as.integer(input$rok))
    opis <- if (length(lata) == 1) paste("w roku", lata)
    else paste("w latach", paste(lata, collapse = ", "))
    h5(paste("Pięć najczęściej czytanych autorów", opis))
  })
  
  output$autorzy <- renderUI({
    req(input$rok)
    top_autorzy <- przeczytane %>%
      filter(year %in% as.integer(input$rok)) %>%
      group_by(Author) %>%
      summarise(ksiazki = n(), .groups = "drop") %>%
      arrange(desc(ksiazki)) %>%
      slice_head(n = 5)
    
    tags$ul(
      lapply(seq_len(nrow(top_autorzy)), function(i) {
        tags$li(
          paste0(top_autorzy$Author[i], " (", top_autorzy$ksiazki[i], " książek)")
        )
      })
    )
  })
  
  output$title_wydawnictwa <- renderUI({
    req(input$rok)
    lata <- sort(as.integer(input$rok))
    opis <- if (length(lata) == 1) paste("w roku", lata)
    else paste("w latach", paste(lata, collapse = ", "))
    h5(paste("Pięć najczęściej czytanych wydawnictw", opis))
  })
  
  output$wydawnictwa <- renderUI({
    req(input$rok)
    top_wydawnictwa <- przeczytane %>%
      filter(year %in% as.integer(input$rok),
             !is.na(Publisher),
             Publisher != "") %>%
      group_by(Publisher) %>%
      summarise(ile = n(), .groups = "drop") %>%
      arrange(desc(ile)) %>%
      slice_head(n = 5)
    
    tags$ul(
      lapply(seq_len(nrow(top_wydawnictwa)), function(i) {
        tags$li(
          paste0(top_wydawnictwa$Publisher[i], " (", top_wydawnictwa$ile[i], " książek)")
        )
      })
    )
  })
  
  output$wykres1 <- renderPlotly({
    df <- dane_filtrowane() %>%
      group_by(year, month) %>%
      summarise(nr_of_book = n(), .groups = "drop") %>%
      mutate(
        miesiac = factor(month, levels = 1:12, labels = miesiace_pl),
        tooltip = paste0(
          "<b>",miesiac, "</b><br>",
          "Rok: ",year, "<br>",
          "Liczba książek: ",nr_of_book
        )
      )
    
    
    p <- ggplot(df,
                aes(
                  x = miesiac,
                  y = nr_of_book,
                  fill = factor(year),
                  text = tooltip
                )
    ) +
      geom_col(position = "dodge") +
      labs(
        title = "Liczba przeczytanych książek w poszczególnych miesiącach",
        x = "Miesiąc",
        y = "Liczba książek",
        fill = "Rok"
      ) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
  })
  
  output$wykres2 <- renderPlotly({
    
    df <- dane_filtrowane() %>%
      mutate(
        Time = as.numeric(Time),
        pages = as.numeric(Number.of.Pages),
        tempo = pages / Time
      ) %>%
      filter(is.finite(tempo))
    
    p <- ggplot(
      df,
      aes(
        x = pages,
        y = tempo,
        color = factor(year),
        text = paste(
          "<b>", Title, "</b><br>",
          "Tempo:", round(tempo, 1), " str./dzień<br>",
          "Strony:", pages
        )
      )
    ) +
      geom_point(alpha = 0.7, size = 3) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(
        title = "Tempo czytania a objętość książek",
        x = "Liczba stron",
        y = "Tempo (strony / dzień)",
        color = "Rok"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      config(displaylogo = FALSE)
  })
  
  output$wykres3 <- renderPlotly({
    df <- oceny
    if(!is.null(input$autor_rating)&& input$autor_rating !=""){
      df <- df%>% filter(Author == input$autor_rating)
    }
    req(nrow(df)>0)
    
    df_long <- df%>%
      select(Title, Author, my_rating, avg_rating) %>%
      tidyr:: pivot_longer(
        cols = c(my_rating, avg_rating),
        names_to = "typ_oceny",
        values_to = "ocena"
      ) %>%
      mutate(
        typ_oceny = recode(
          typ_oceny,
          my_rating ="Moja ocena",
          avg_rating = "Średnia ocena"
        )
      )
    
    p <- ggplot(
      df_long,
      aes(
        x = Title,
        y = ocena,
        fill = typ_oceny,
        text = paste0(
          "<b>", Title, "</b><br>",
          "Autor: ", Author, "<br>",
          typ_oceny, ": ",ocena
        )
      )
    ) +
      geom_col(position = position_dodge(width = 0.7))+
      labs(
        title = "Porównanie ocen książek",
        x = "Książki",
        y = "Ocena",
        fill = ""
      )+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")%>%
      config(displaylogo =FALSE)
  })
}


shinyApp(ui = ui, server = server)


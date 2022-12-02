library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggthemes)
library(reactable)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(sf)
library(fuzzyjoin)

source('data.R')

# Dataset
nf_titles = as_tibble(read_csv("./data/netflix_titles.csv"))

# Separa coluna de atores/atrizes
nf_cast = nf_titles %>%
  separate_rows(cast, sep = ",") %>%
  mutate(cast = str_trim(cast, side = "both")) %>%
  filter(cast != "")

# Separa coluna de paises
nf_countries = nf_titles %>%
  separate_rows(country, sep = ",") %>%
  mutate(country = str_trim(country, side = "both")) %>%
  filter(country != "")

# GeoJSON dos países
world_country <- sf::st_as_sf(readOGR("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"))

# Define o tema padrão do ggplot2
theme_set(theme_gdocs())

# Define UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Netflix Movies and TV Shows", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Explorador", tabName = "explorer", icon = icon("table")),
      menuItem("Mapa", tabName = "map", icon = icon("map")),
      
      uiOutput("mapControls")
    )
  ),
  dashboardBody(
    # META TAGS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(HTML('
        $(document).ready(function(){
          $("a[data-toggle=tab]").on("show.bs.tab", function(e){
            Shiny.setInputValue("activeTab", $(this).attr("data-value"));
          });
        });
      '))
    ),
    
    tabItems(
      # Dashboard ----
      tabItem(
        tabName = "dashboard",
        
        # VALUE BOXES
        fluidRow(
          valueBoxOutput("totalCount"),
          valueBoxOutput("movieCount"),
          valueBoxOutput("tvShowCount"),
          valueBoxOutput("castCount"),
          valueBoxOutput("countriesCount"),
          valueBoxOutput("directorCount"),
        ),
        
        # CHARTS
        fluidRow(
          box(
            width = 12,
            title = "Filmes vs Programas de TV",
            plotlyOutput("moviesVsTvShowPlot")
          ),
          box(
            width = 12,
            title = "Top 10 atores/atrizes",
            plotOutput("top10CastPlot")
          )
        )
      ),
      
      # Explorer ----
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            width = 12,
            reactableOutput("explorerTable")
          )
        )
      ),
      
      # Map ----
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "Países com maiores quantidades de títulos produzidos",
            width = 12,
            leafletOutput("map", height = 'calc(100vh - 200px)')
          )
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  # Dashboard ----
  # Value boxes
  # Contagem de títulos
  output$totalCount <- renderValueBox({
    valueBox(
      width = 3,
      value = nf_titles %>%
        summarize(n = n_distinct(show_id)) %>%
        pull(n) %>%
        format(big.mark = ".", decimal.mark = ","),
      subtitle = "Títulos",
      icon = icon("table"),
      color = "black"
    )
  })
  # Contagem de filmes
  output$movieCount <- renderValueBox({
    valueBox(
      width = 3,
      value = nf_titles %>%
        filter(type == "Movie") %>%
        summarize(n = n_distinct(show_id)) %>%
        pull(n) %>%
        format(big.mark = ".", decimal.mark = ","),
      subtitle = "Filmes",
      icon = icon("video-camera"),
      color = "blue"
    )
  })
  # Contagem de Shows de TV
  output$tvShowCount <- renderValueBox({
    valueBox(
      width = 3,
      value = nf_titles %>%
        filter(type == "TV Show") %>%
        summarize(n = n_distinct(show_id)) %>%
        pull(n) %>%
        format(big.mark = ".", decimal.mark = ","),
      subtitle = "Shows de TV",
      icon = icon("film"),
      color = "yellow"
    )
  })
  # Contagem de atores
  output$castCount <- renderValueBox({
    valueBox(
      width = 3,
      value = nf_cast %>%
        summarise(n = n_distinct(cast)) %>%
        pull(n) %>%
        format(big.mark = ".", decimal.mark = ","),
      subtitle = "Atores/Atrizes",
      icon = icon("users"),
      color = "light-blue"
    )
  })
  # Contagem de países
  output$countriesCount <- renderValueBox({
    valueBox(
      width = 3,
      value = nf_countries %>%
        summarise(n = n_distinct(country)) %>%
        pull(n) %>%
        format(big.mark = ".", decimal.mark = ","),
      subtitle = "Países",
      icon = icon("globe"),
      color = "green"
    )
  })
  # Contagem de diretores
  output$directorCount <- renderValueBox({
    valueBox(
      width = 3,
      value = nf_titles %>%
        separate_rows(director, sep = ",") %>%
        mutate(director = str_trim(director, side = "both")) %>%
        filter(director != "") %>%
        summarise(n = n_distinct(director)) %>%
        pull(n) %>%
        format(big.mark = ".", decimal.mark = ","),
      subtitle = "Diretores",
      icon = icon("user"),
      color = "navy"
    )
  })
  
  # Charts
  # Filmes vs Porgramas de TV
  output$moviesVsTvShowPlot <- renderPlotly({
    ggplotly(
      tooltip = c("text"),
      nf_titles %>%
        group_by(release_year, type) %>%
        summarize(n = n_distinct(show_id)) %>%
        arrange(release_year) %>%
        ggplot(aes(
          x = release_year,
          y = n,
          text = paste(
            "<b>", release_year, "</b>",
            "<br>Tipo: ", type,
            "<br>Títulos lançados: ", n
          )
        )) +
        geom_line(
          aes(
            group = 1,
            color = type,
            linetype = type,
          ),
          size = 1
        ) +
        scale_color_manual(values = c("darkred", "steelblue")) +
        labs(color = "Tipo", linetype = NULL, x = "Ano de lançamento", y = "Quantidade de lançamentos")
    )
  })
  
  # Contagem de atores mais populares
  output$top10CastPlot <- renderPlot({
    nf_cast %>%
      group_by(cast) %>%
      summarise(n = n_distinct(show_id)) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      ggplot(aes(
        x = reorder(cast, n),
        y = n
      )) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Atores/Atrizes", y = "Títulos")
  })
  
  # Explorer ----
  output$explorerTable <- renderReactable({
    reactable(nf_titles, defaultSorted = c("date_added", "title"), defaultSortOrder = "desc", filterable = TRUE, searchable = TRUE)
  })
  
  # Map ----
  output$map = renderLeaflet(({
    nf_countries_count <- nf_countries %>%
      group_by(country) %>%
      summarise(n = n_distinct(show_id))
    
    # É preciso usar um join com regex pois pode haver diferenças entre os
    # nomes do GeoJSON e do DataSet, como por exemplo: United States e United
    # States of America
    countries_intersection <- st_as_sf(
      world_country %>%
        regex_inner_join(nf_countries_count, by = c(name = "country"), ignore_case = TRUE)
    )
    
    pal <- colorBin("Reds", nf_countries_count$n, bins = c(0, 10, 100, 200, 300, 400, 600, 800, 1000, 2000, 3000, Inf))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%d títulos</sup>",
      countries_intersection$name, countries_intersection$n
    ) %>% lapply(htmltools::HTML)
    
    opacity <- input[["mapOpcaitySlider"]] / 100
    
    countries_intersection %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(n),
        weight = 2,
        opacity = opacity,
        fillOpacity = opacity,
        color = "white",
        dashArray = "3",
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#333",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = ~n, opacity = 0.7, title = NULL,
                position = "bottomright")
  }))
  
  # Map controls
  output$mapControls = renderUI({
    if (!is.null(input$activeTab) && input$activeTab == "map") {
      fluidRow(
        column(
          width = 12,
          sliderInput(
            inputId = "mapOpcaitySlider",
            label = h3("Opacidade"),
            min = 0,
            max = 100,
            value = 75,
            post = '%'
          )
        ),
      )
    }
  })
}

# Run the app ----
shinyApp(ui, server)
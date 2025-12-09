# installer les packages
#install.packages("shiny")
#install.packages("knitr")
#install.packages("shinytitle")
#install.packages("sf")
#install.packages("dplyr")
#install.packages("cartography")
#install.packages("leaflet")
#install.packages("rsconnect")

# charger les librairies 
library(shiny)
library(knitr) 
library(shinytitle)
library(sf)
library(dplyr)
library(cartography)
library(leaflet)
library(rsconnect)

# définir le répertoire de travail
#setwd("D:/webmapping")

# charger les hôtels
hotels = st_read("hotels.gpkg")

# charger les arrondissements
arrondissements = st_read("arrondissements.gpkg")

# charger les zones touristiques internationales 
zones_tour <- st_read("zones_touristiques.geojson")

# palette de couleurs pour les étoiles
etoiles <- sort(unique(hotels$classement))
pal_etoiles <- colorFactor(
  palette = c("#FF6B6B",
              "#FFA94D", 
              "#FFD93D", 
              "#6BCB77",  
              "#4D96FF"),
  levels = etoiles)




# --------------------------
# UI
# --------------------------
ui <- fluidPage(
  titlePanel("Visualisation des hôtels classés à Paris"),
  
  tabsetPanel(
    
    # onglet d'accueil
    tabPanel(
      "Carte",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "filtre_etoiles",
            "Sélectionner les étoiles à afficher :",
            choices = etoiles,
            selected = etoiles
          ),
          selectInput(
            "select_arr",
            "Sélectionner un arrondissement :",
            # tri par ordre croissant
            choices = c("Tout Paris", sort(arrondissements$nom_arr)), 
            selected = "Tout Paris"
          ),
          plotOutput("diag_hotels", height = "300px")
        ),
        mainPanel(
          # ajuster la taille de la carte
          div(
            style = "height: calc(95vh - 80px);",
            leafletOutput("map", width = "100%", height = "100%")
          )
        )
      )
    ),
    
    # onglet relatif aux informations complémentaires
    tabPanel(
      "Informations",
      fluidRow(
        column(
          width = 12,
          h4("Crédits"),
          tags$p("Antoine VICENTE et Jean Luc Patrick MABIALA, 2024, Université de Rouen", 
                 style="font-size:small;"),
          hr(), # ligne de séparation
          h4("Description du projet"),
          p("Cette application web permet de visualiser les hôtels classés dans Paris, 
          ainsi que leur répartition par arrondissement et par catégorie (nombre d’étoiles). 
          Elle comprend un diagramme circulaire pour analyser synthétiquement la distribution des hôtels 
          par classement en fonction de l'arrondissement sélectionné. 
          L’objectif est de faciliter la compréhension de la répartition hôtelière et des zones touristiques de la ville, 
          et ainsi montrer un lien entre ces deux variables."),
          hr(),
          h4("Données utilisées"),
          tags$ul(
          tags$li(tags$a(href="https://www.data.gouv.fr/datasets/la-carte-des-hotels-classes-en-ile-de-france-idf/",
                         "La carte des hôtels classés en Île-de-France")),
          tags$li(tags$a(href="https://www.data.gouv.fr/datasets/contours-des-arrondissements-francais-issus-d-openstreetmap/", 
                         "Contours des arrondissements français issus d'OpenStreetMap")),
          tags$li(tags$a(href="https://www.data.gouv.fr/datasets/zones-touristiques-internationales-a-paris/", 
                         "Zones touristiques internationales à Paris"))
          ),
          hr(),
          h4("Ressources"),
          tags$ul(
            tags$li(tags$a(href="https://rstudio.github.io/leaflet/articles/shiny.html?utm", 
                           "Using Leaflet with Shiny")),
            tags$li(tags$a(href="https://lrouviere.github.io/TUTO_VISU/shiny.html", 
                           "Applications web avec Shiny")),
            tags$li(tags$a(href="https://blog.crea-troyes.fr/3752/tutoriel-r-et-shiny-le-package-shiny/", 
                           "Tutoriel R et Shiny : Le package Shiny")),
            tags$li(tags$a(href="https://www.simplehtmlguide.com/cheatsheet.php", 
                           "A Simple Guide to HTML")),
            tags$li(tags$a(href="https://docs.kanaries.net/fr/charts/how-to-make-pie-chart-r", 
                           "Créer facilement un diagramme circulaire avec R"))
          )
        )
      )
    ) 
  )
)

  
# --------------------------
# SERVER
# --------------------------
server <- function(input, output, session) {
  
  # carte des hôtels
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      # arrondissements
      addPolygons(
        data = arrondissements,
        layerId = ~code_postal,
        color = "#444444",
        weight = 2,
        opacity = 1,
        fillColor = "#FFFFFF",
        fillOpacity = 0,
        dashArray = "3",
        highlight = highlightOptions(
          weight = 3,
          color = "blue",
          bringToFront = FALSE
        ),
        label = ~nom_arr
      ) %>%
      
      # zones touristiques internationales 
      addPolygons(
        data = zones_tour,
        fillColor = "red", 
        fillOpacity = 0.05,   
        color = "red",        
        weight = 2,          
        opacity = 0.75,            
        popup = ~paste0("<b>Zone touristique : </b>", name), 
        group = "Zones touristiques"
      ) %>%
      
      # contrôle pour les zones touristiques
      addLayersControl(
        overlayGroups = c("Zones touristiques internationales"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # filtrer et afficher les hôtels
  observe({
    hotels_filtered <- hotels %>% filter(classement %in% input$filtre_etoiles)
    
    leafletProxy("map") %>%
      clearGroup("Hôtels") %>%
      {if(nrow(hotels_filtered) > 0) addCircleMarkers(
        .,
        data = hotels_filtered,
        lng = ~lon,
        lat = ~lat,
        radius = 5,
        color = ~pal_etoiles(classement),
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste0(
          "<b>", nom, "</b><br>",
          "Classement : ", classement, "<br>",
          "Adresse : ", adresse),
        group = "Hôtels"
      ) else .} %>%
      clearControls() %>%
      addLegend(
        "bottomright",
        pal = pal_etoiles,
        values = hotels$classement,
        title = "Classement (étoiles)",
        opacity = 1
      )
  })
  
  # zoom sur l'arrondissement sélectionné
  observeEvent(input$select_arr, {
    req(input$select_arr)
    
    if(input$select_arr == "Tout Paris"){
      bbox <- st_bbox(arrondissements)
    } else {
      poly <- arrondissements %>% filter(nom_arr == input$select_arr)
      if(nrow(poly) == 0) return()
      bbox <- st_bbox(poly)
    }
    
    leafletProxy("map") %>%
      fitBounds(
        lng1 = as.numeric(bbox["xmin"]),
        lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]),
        lat2 = as.numeric(bbox["ymax"])
      )
  })
  
  # diagramme circulaire des hôtels par classement
  output$diag_hotels <- renderPlot({
    
    # harmoniser le système de coordonnées 
    hotels_crs <- st_transform(hotels, st_crs(arrondissements))
    
    if(input$select_arr == "Tout Paris"){
      hotels_arr <- hotels_crs
    } else {
      poly <- arrondissements %>% filter(nom_arr == input$select_arr)
      hotels_arr <- hotels_crs[st_within(hotels_crs, poly, sparse = FALSE)[,1], ]
    }
    
    df <- hotels_arr %>%
      st_drop_geometry() %>%
      group_by(classement) %>%
      summarise(nb = n()) %>%
      arrange(classement)
    
    if(nrow(df) == 0){
      plot.new()
      text(0.5, 0.5, "Aucun hôtel dans cet arrondissement", cex = 1.2)
      return()
    }
    
    cols <- pal_etoiles(df$classement)
    # calcul des pourcentages
    pct <- round(100 * df$nb / sum(df$nb), 1)
    # labels : seulement nombre + pourcentage
    labels <- paste0(df$nb, " (", pct, "%)")
    
    pie(df$nb, labels = labels, col = cols,
        main = paste("Répartition des hôtels :", input$select_arr))
  })
}

# --------------------------
# LANCER L'APPLICATION
# --------------------------
shinyApp(ui, server)



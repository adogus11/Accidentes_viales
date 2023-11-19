#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# librer?a----
Sys.setlocale("LC_ALL", "Spanish")
library(shiny)
library(leaflet)
library(treemapify)
library(tidyverse)
# Funciones viuzz---
pal = wesanderson::wes_palette("Zissou1",
                                5, 
                                type = "continuous")

# Datos ----
# Datos del c5
# puntos = read_csv("01_Bases_de_datos/c5.csv") %>% 
#   mutate(Year=lubridate::year(fecha_creacion))

puntos = dtplyr::lazy_dt(
  data.table::fread("01_Bases_de_datos/c5.csv")
  ) %>% 
  mutate(Year=lubridate::year(fecha_creacion)) %>% 
  as_tibble()

# H3 bases 7,8,9

h9 = sf::st_read("01_Bases_de_datos/h9.gpkg")
h8 = sf::st_read("01_Bases_de_datos/h8.gpkg")
h7 = sf::st_read("01_Bases_de_datos/h7.gpkg")


# CDMX geometr?a
cdmx = sf::st_read("01_Bases_de_datos/cdmx_gen.gpkg")

# Relación folio h3

relacion = read.csv("01_Bases_de_datos/folio_h3.csv") %>% 
  select(-1) %>% 
  as_tibble()


# setwd("H:/Mi unidad/Proyecto_SIG_Accidentes_Viales_18042023/04_entregables/app")
# ui-----

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mapa", width = "100%", height = "100%"),
  absolutePanel(
    h3(style="color:range;text-align:center;","CentroGeo"),
    h3(style="color:orange;text-align:center;","Geomática"),
    # tags$img(src = "www/cg.png", height = 35, width = 35),
    h3(style="text-align:center;","Zonas de análisis",br()," agregadas por H3"),
    draggable = TRUE,
    top = 10,
    left=10,
    #right = 10,
    selectInput("id_h3",
                "Selecciona un nivel de agregación",
                c("H3~7 (5.16km^2)"="h7",
                  "H3~8 (0.73km^2)"="h8",
                  "H3~9 (0.10km^2)"="h9")
                ),
    selectInput(inputId = "id_year",
                label = "Selecciona un año",
                choices = c(2017,2018,2019,2020,2021,2022),
                selected = 2022,
                multiple = F,
                width = "100px"),
    echarts4r::echarts4rOutput(
      outputId = "prc_incidentes",
      width = "300px",
      height = "200px")
    ),
  absolutePanel(
    draggable = T,
    top = 10,
    right = 10,
    echarts4r::echarts4rOutput(
      outputId = "histograma",
      width = "300px",
      height = "200px"),
    # verbatimTextOutput("id_hex"),
    h2(style="color:black;text-align:center;","Autores:"),
    tags$ul(
      tags$li("Paola Hernández ", tags$a(href = "al.phernandez@centrogeo.edu.mx ",icon("envelope"),"Mi correo")),
      tags$li("Gustavo Islas ",tags$a(href = "al.gislas@centrogeo.edu.mx ",icon("envelope"), "Mi correo")),
      tags$li("Noe Osorio", tags$a(href = "al.nosorio@centrogeo.edu.mx",icon("envelope"),"Mi correo"))
    ),
    h3(style="color:black;text-align:center;","Asesores:"),
    tags$ul(
      tags$li("Mtra. Karime González Zuccolotto ", tags$a(href = "https://www.centrogeo.org.mx/areas-profile/karime.gonzalez.html",icon("envelope"),"contacto")),
      tags$li("Dr. Hugo Carlos Martínez ",tags$a(href = "https://www.centrogeo.org.mx/areas-profile/hcarlos ",icon("envelope"), "contacto"))
    )
  )
)
 


# server <- function(input, output) {}

server <- function(input, output) {
  
  geometria_ok =reactive({
      seleccionar = input$id_h3
           
           folioh3 = relacion %>%
             select(folio,seleccionar)#input$id_h3
           
           nombre = colnames(folioh3 %>% select(-1))
           
           geometria_ok = puntos %>% 
             filter(Year==as.numeric(input$id_year)) %>% 
             select(folio) %>% 
             left_join(folioh3) %>% 
             group_by(get(nombre)) %>% 
             summarise(total=n(),.groups = "drop")
           
           colnames(geometria_ok) <- c("h3_7","total")
           
           geometria_ok = geometria_ok %>% 
             filter(total>2) %>%
             mutate(rangos=cut(total,5)) %>% 
             left_join(get(seleccionar) %>% select(1)) %>% 
             sf::st_as_sf()
           })
  
  
  output$prc_incidentes <- echarts4r::renderEcharts4r({
  
  base_1 = puntos %>%
    filter(Year==as.numeric(input$id_year)) %>% 
    group_by(incidente_c4) %>%
    summarise(total=n(),.groups = "drop") %>%
    mutate(prc=total/sum(total))

  base_1 |>
    # select(1,2) %>% 
    mutate(name=paste0(incidente_c4,"\n",scales::percent(prc))) %>%
    select(name,value=prc) %>% 
    echarts4r::e_charts() |>
    echarts4r::e_title(
      text = paste0("% Accidentes: ",paste0(input$id_year))) %>% 
    echarts4r::e_treemap() %>% 
    # echarts4r::e_tooltip(trigger = "item") %>%
    echarts4r::e_color(color = pal)
  })
  
  
  output$mapa <- leaflet::renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB,group="Carto") %>% 
      addProviderTiles(providers$OpenStreetMap,group="OSM") %>% 
      setView(lng =-99.144813,lat =  19.387171,zoom = 10) %>% 
      addLayersControl(baseGroups = c("Carto","OSM"),
                       overlayGroups = c("CDMX","H3"),
                       options = layersControlOptions(collapsed = F)
                       )
  })
  
  observe({
    
    paleta = colorFactor(c("#2b83ba",
                           "#abdda4",
                           "#ffffbf",
                           "#fdae61",
                           "#d7191c"),
                         domain = geometria_ok()$rangos,
                         na.color = NA)
    
    proxy <- leafletProxy("mapa", data  =geometria_ok())  
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>%  
      clearMarkers() %>%
      clearShapes() %>% 
      addPolygons(
                  weight = 1,
                  color = ~paleta(rangos),
                  opacity = 1,
                  fillOpacity = .7,
                  label = ~total,
                  group = "H3"
      ) %>% 
      addPolygons(data=cdmx,
                  weight = .5,
                  color = "black",
                  fill = NA,
                  opacity = 1,group = "CDMX")
       
  })
  


  output$histograma <- echarts4r::renderEcharts4r({
      echarts4r::e_chart(geometria_ok()) %>% 
      echarts4r::e_histogram(total) %>% 
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_title(
        text = paste0("Distribución por ",input$id_h3)
      )
  })
  
  # output$id_hex <- renderPrint({
  #   click <- input$mapa_click
  #   if (!is.null(click) && click$origen == "mapa") {
  #     id_hex <- as.character(click$id)
  #     id_hex <- strsplit(id_hex)
  #     return(geometria_ok()$hexagonos$id[geometria_ok()$hexagonos$h3_7 == id_hex])
  #   } else {
  #     "Ning?n hex?gono seleccionado"
  #   }
  # })

  }

# Run the application ----
shinyApp(ui =htmlTemplate("www/index.html"), 
         server = server)
  

require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(tidyverse)
require(lubridate)
require(magrittr)
require(ggalluvial)
require(highcharter)
require(tmap)
require(sf)
require(tidyterra)
require(terra)

tmap_mode(mode = "view")
options(scipen = 999)

##Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

## bootswatch theme ("cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", "litera", "lumen", "lux", "materia", "minty", "morph", "pulse", "quartz", "sandstone", "simplex", "sketchy", "slate", "solar", "spacelab", "superhero", "united", "vapor", "yeti", "zephyr")

agr.data = read_csv("agriculture_clean.csv")%>% 
  mutate(crop = str_to_title(crop),
         region = if_else(region == "Coast", "Pwani", region),
         region = if_else(region == "Rukwa/Katavi", "Katavi", region),
         region = if_else(region == "Dar es Salaam", "Dar-es-salaam", region)) %>% 
  mutate(zones = case_when(region %in%  c("Mbeya", "Iringa","Rukwa" ,"Katavi" ,"Songwe", "Njombe") ~ "Southern Hi",
                           region %in% c("Shinyanga", "Simiyu", "Kigoma", "Tabora") ~ "Western",
                           region %in% c("Dodoma", "Singida") ~ "Central",
                           region %in% c("Dar-es-salaam", "Morogoro", "Pwani") ~ "Eastern",
                           region %in% c("Arusha", "Kilimanjaro", "Tanga", "Manyara") ~ "Northern",
                           region %in% c("Lindi", "Mtwara", "Ruvuma") ~ "Southern",
                           region %in% c("Mwanza", "Mara", "Kagera", "Geita") ~ "Lake")) %>% 
  drop_na(zones)

zones = agr.data %>% distinct(zones) %>% pull()

crop = agr.data %>% distinct(crop) %>% pull()

variable = agr.data %>% distinct(variable) %>% pull()

region = agr.data %>% distinct(region) %>% pull()


southernH = c("Mbeya", "Iringa","Rukwa" ,"Katavi" ,"Songwe", "Njombe")
western = c("Shinyanga", "Simiyu", "Kigoma", "Tabora")
central = c("Dodoma", "Singida")
eastern = c("Dar es Salaam", "Morogoro", "Pwani")
northern = c("Arusha", "Kilimanjaro", "Tanga", "Manyara")
southern = c("Lindi", "Mtwara", "Ruvuma")
lake = c("Mwanza", "Mara", "Kagera", "Geita")


region.sf = st_read("data/regions.gpkg") %>% 
  select(region = 4) %>% 
  st_make_valid()

africa = spData::world %>% 
  filter(continent == "Africa") %>% 
  st_as_sf()

region.agr = region.sf %>% 
  left_join(agr.data %>% filter(variable =="yield" ), by = "region") %>% 
  select(-variable)




ui = navbarPage(
  title = "",
  theme = bslib::bs_theme(version = 5, bootswatch = "cerulean"),
  useShinydashboard(),
  tabPanel(
    title = "Agriculture Production",
    fluidRow(
      column( width = 1),
      column(width = 2,
             tags$text("Choose years by dragging and view the flow of crop output"),
             sliderInput(inputId = "year_variable", label = "", min = 2001, max = 2019, value = c(2003,2019), step = 1, sep = ""),
             tags$hr(),
             tags$text("Select a crop output and view its variation at different zones"),
             pickerInput(inputId = "agr_variable", label = "", choices = variable, selected = "production", multiple = FALSE),
             tags$hr(),
             tags$text("Pick a zone and view crop output at different regions within a zone"),
             pickerInput(inputId = "agr_zones", label = "", choices = zones, selected = "Northern", multiple = FALSE)),
      column(width = 4,
             plotOutput(outputId = "variable_zone") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
             ),
      column(width = 4,
             plotOutput(outputId = "region_zone") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
             )
    ),
    
    tags$hr(),
    fluidRow(
      column(width = 1),
      column(width = 2,
             tags$text("Choose years by dragging and view the trend of crop output"),
             sliderInput(inputId = "trend_year", label = "", min = 2001, max = 2019, value = c(2007,2015), step = 1, sep = ""),
             tags$hr(),
             tags$text("Select a crop output to view its trend"),
             pickerInput(inputId = "trend_variable", label = "", choices = variable, selected = "yield"),
             tags$hr(),
             tags$text("Pick a crop whose trend will be displayed"),
             pickerInput(inputId = "trend_crop", label = "", choices = crop, selected = "Rice"),
             tags$hr(),
             tags$text("Choose regions/zones whose trend to be displayed"),
             virtualSelectInput(
               inputId = "trend_region", label = "", 
               choices = list( 
                 "Lake zone" = lake,
                 "Eastern zone" = eastern,
                 "Western zone" = western,
                 "Southern Highland" = southernH,
                 "Southern zone" = southern,
                 "Central zone" = central,
                 "Northern zone" = northern
                 ),
               selected = lake,
               showValueAsTags = TRUE,
               search = TRUE,
               multiple = TRUE
               )
             ),
      column(width = 4,
             plotOutput(outputId = "trend_plot") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
             ),
      column(width = 2,
             valueBoxOutput(outputId = "z_id", width = 100),
             fluidRow(
               valueBoxOutput(outputId = "slope_id", width = 100)
             ),
             fluidRow(
               valueBoxOutput(outputId = "p_id", width = 100)
               
             )
        
      )
      
        
      ),
    tags$hr(),
    fluidRow(
      column(width = 1),
      column(width = 2,
             tags$text("Choose a year by dragging and view its yield on a map"),
             sliderInput(inputId = "map_year", label = "", min = 2001, max = 2019, value = 2005, step = 1, sep = ""),
             tags$hr(),
             tags$text("Select a transformation method for a better display of the yield on a map"),
             radioButtons(inputId = "style_id",label = "",choices = c("quantile", "kmeans", "jenks", "cont") ),
             tags$hr(),
             tags$text("Pick a crop of interest and view its yield on a map"),
             pickerInput(inputId = "map_crop", label = "", choices = crop, selected = "Maize")
        
      ),
      column(width = 4,
             tmapOutput(outputId = "tmap_plot") %>% shinycustomloader::withLoader(type = "html", loader = "loader1")
        
      )
    ),
    tags$br(),
    tags$br()
      
             
    )
  )







server = function(input, output, session){
  
  zonesreactive = reactive({
    agr.data %>% 
      filter(variable %in% input$agr_variable,
             year >= input$year_variable[1]  & year <= input$year_variable[2]) %>% 
      group_by(region,zones,year, crop) %>% 
      summarise(
        n = n(),.groups = "drop",
        bar = sum(data, na.rm = T)
      ) %>% 
      arrange(-bar) 
    
  })
  
  output$variable_zone = renderPlot({
  
  zonesreactive() %>% 
    ggplot(aes(axis1 = zones, axis2 = crop, y = bar)) +
    geom_alluvium(aes(fill = crop), curve_type = "sigmoid", alpha = 0.6, width = 1/5)+
    geom_stratum(aes(fill = crop), width = 1/5)+
    geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3, alpha = 0.6)+
    theme_void()+
    theme(legend.position = "none")+
    ggsci::scale_fill_futurama()+
    scale_fill_brewer(palette = "Dark2")
  })
  
  output$region_zone = renderPlot({
    zonesreactive() %>% 
      filter(zones %in% input$agr_zones) %>% 
      ggplot(aes(axis1 = region,   axis2 = crop, y = bar)) +
      geom_alluvium(aes(fill = crop), curve_type = "sigmoid", alpha = 0.6, width = 1/5)+
      geom_stratum(aes(fill = crop), width = 1/5)+
      geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 3, alpha = 0.6)+
      theme_void()+
      theme(legend.position = "none")+
      ggsci::scale_fill_futurama()+
      scale_fill_brewer(palette = "Dark2")
  })
  
  reactivetrend = reactive({
    agr.data %>% 
    filter(variable %in% input$trend_variable & 
             crop %in% input$trend_crop & 
             year >= input$trend_year[1] & year <= input$trend_year[2] &
             region %in% input$trend_region) %>%
      group_by(year) %>%
      summarise(
        n = n(),
        bar = mean(data, na.rm = T), 
        error = sd(data, na.rm = T),
        error = error/sqrt(n),
        .groups = "drop") %>%
      pull(bar) %>% 
      ts(start = input$trend_year[1], frequency = 1)%>% 
      imputeTS::na_kalman() 
    
  })
  
  output$trend_plot = renderPlot({
    
  reactivetrend() %>% 
      forecast::autoplot() +
      theme_bw()
 
  })
  
 bb = reactive({agr.data %>% 
     filter(variable %in% input$trend_variable & 
              crop %in% input$trend_crop & 
              year >= input$trend_year[1] & year <= input$trend_year[2] &
              region %in% input$trend_region) %>%
     group_by(year) %>%
     summarise(
       n = n(),
       bar = mean(data, na.rm = T), 
       error = sd(data, na.rm = T),
       error = error/sqrt(n),
       .groups = "drop") %>%
     pull(bar) %>% 
     ts(start = input$trend_year[1], frequency = 1)%>% 
     imputeTS::na_kalman() %>% 
    trend::sens.slope()
 })
 
  output$z_id = renderValueBox({
    bb()[2] %>% as.numeric()%>% round(digits = 4) %>% 
      valueBox(subtitle = if_else(bb()[2] %>% as.numeric()%>% round(digits = 4) > 0, "Increasing trend", "Decreasing trend"), 
               icon = icon("water"),
               color = if_else(bb()[2] %>% as.numeric()%>% round(digits = 4) > 0, "green", "red"))
  })
  
  output$slope_id = renderValueBox({
    bb()[1] %>% as.numeric()%>% round(digits = 4) %>% 
      valueBox(subtitle = "Rate", icon = icon("water"), color = "purple")
  })
  
  output$p_id = renderValueBox({
    bb()[3] %>% as.numeric()%>% round(digits = 4) %>% 
      valueBox(subtitle = if_else(bb()[3] %>% as.numeric()%>% round(digits = 4) > 0.05, "Not significant", "Significant"), 
               icon = icon("water"),
               color = if_else(bb()[3] %>% as.numeric()%>% round(digits = 4) > 0.05, "red", "green"))
  })
  
  
     aaa = reactive(
       region.agr %>% 
        rename(yield = data) %>%  
        filter(year %in% input$map_year & crop %in% input$map_crop)
     )
  
  output$tmap_plot = renderTmap(
    
  tm_shape(shp = aaa()) +
    tm_fill(col = "yield", style = input$style_id, title = "Yield <br> (Tons/acre)") +
    tm_borders(col = "ivory") +
    tm_text(text = "region",size = 0.5)+
    tm_layout(legend.position = c(0.1,0.1),
              legend.bg.color = "white",
              legend.frame = TRUE, 
              legend.frame.lwd = 0.2)
  )
}





shinyApp(ui = ui, server = server)

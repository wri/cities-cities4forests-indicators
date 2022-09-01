library(shiny)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(plyr)
library(dplyr)
library(rgdal)
library(shinyWidgets)
library(rnaturalearth)
library(tidyverse)
library(sf)
library(rgeos)
library(httr)
library(jsonlite)
library(raster)
library(data.table)
library(DT)
library(leafem)
library(RColorBrewer)
library(shinydisconnect)

# define aws s3 path

aws_s3_path = "https://cities-cities4forests.s3.eu-west-3.amazonaws.com/"

############### Load data

# read indicator definition ------------


indicators_definitions = read.csv(paste(aws_s3_path,
                                        "data/indicators/indicators_definition.csv",
                                        sep = ""))

# get list of themes
indicators_themes = unique(indicators_definitions$theme)

# get list of indicators

indicators_list = unique(indicators_definitions$indicator_label)


# read boundaries georef -----

boundary_georef = read.csv(paste(aws_s3_path,
                                 "data/boundaries/v_0/geo_ref.csv",
                                 sep = ""),
                           fileEncoding="UTF-8-BOM")

boundary_georef = read.csv(paste(aws_s3_path,
                                 "data/boundaries/v_0/boundary_georef.csv",
                                 sep = ""),
                           fileEncoding="UTF-8-BOM")

cities = unique(boundary_georef$geo_name)


# read indicator ------------


# indicators = read.csv(paste(aws_s3_path,
#                             "data/indicators/cities_indicators.csv",
#                             sep = ""),
#                       encoding="UTF-8")
# 
# 
# 
# indicators_test = read.csv(paste(aws_s3_path,
#                                  "data/indicators/cities_indicators_erictest.csv",
#                                  sep = ""),
#                            encoding="UTF-8")
# 
# indicators = indicators %>% 
#   left_join(indicators_test[,c("geo_id",
#                                "GRE_3_2_percentPopwOpenSpaceAccess",
#                                "GRE_3_3_percentPopwTreeCoverAcess",
#                                "GRE_1_3_percentBuiltwLowAlbedo")],
#             by = "geo_id") 
# 
# indicators = indicators %>% 
#   mutate(GRE_3_1_percentOpenSpaceinBuiltup = 100 * GRE_3_1_percentOpenSpaceinBuiltup,
#          GRE_3_2_percentPopwOpenSpaceAccess = 100 * GRE_3_2_percentPopwOpenSpaceAccess,
#          GRE_3_3_percentPopwTreeCoverAcess = 100 * GRE_3_3_percentPopwTreeCoverAcess,
#          GRE_1_3_percentBuiltwLowAlbedo = 100 * GRE_1_3_percentBuiltwLowAlbedo) 


# Read v2 indicators

indicators = read.csv(paste(aws_s3_path,
                            "data/indicators/cities_indicators_v2.csv",
                            sep = ""),
                      encoding="UTF-8")

indicators = indicators %>% 
  mutate(GRE_3_1_percentOpenSpaceinBuiltup = 100 * GRE_3_1_percentOpenSpaceinBuiltup,
         GRE_3_2_percentPopwOpenSpaceAccess = 100 * GRE_3_2_percentPopwOpenSpaceAccess,
         GRE_3_3_percentPopwTreeCoverAcess = 100 * GRE_3_3_percentPopwTreeCoverAcess) 

# GRE_1_4 ----
indicators_GRE_1_4 = read.csv(paste(aws_s3_path,
                                    "data/indicators/dev/cities_indicators_GRE_1_4.csv",
                                    sep = ""),
                              encoding="UTF-8")

indicators = indicators %>% 
  left_join(indicators_GRE_1_4,
            by = "geo_id")

# GRE_1_3 ----
indicators_GRE_1_3 = read.csv(paste(aws_s3_path,
                                    "data/indicators/dev/cities_indicators_GRE_1_3.csv",
                                    sep = ""),
                              encoding="UTF-8")

indicators = indicators %>% 
  left_join(indicators_GRE_1_3[,c("geo_id","GRE_1_3_percentBuiltwLowAlbedo")],
            by = "geo_id") %>% 
  mutate(GRE_1_3_percentBuiltwLowAlbedo = 100 * GRE_1_3_percentBuiltwLowAlbedo)

# GRE_2_3 ----
indicators_GRE_2_3 = read.csv(paste(aws_s3_path,
                                    "data/indicators/dev/cities_indicators_GRE_2_3.csv",
                                    sep = ""),
                              encoding="UTF-8")

indicators = indicators %>% 
  left_join(indicators_GRE_2_3[,c("geo_id","GRE_2_3_population_exposure_pm25")],
            by = "geo_id") %>% 
  mutate(GRE_2_3_population_exposure_pm25 = 100 * GRE_2_3_population_exposure_pm25)




# keep distinct geo_id
indicators = indicators %>% 
  distinct(geo_id, .keep_all = TRUE)

# cities comparison ----


cities_comparison_list = c("BRA-Salvador_ADM4-union_1",
                           "COD-Bukavu_ADM3-union_1",
                           "COD-Uvira_ADM3-union_1",
                           "COG-Brazzaville_ADM4-union_1",
                           "COL-Barranquilla_ADM4-union_1",
                           "ETH-Addis_Ababa_ADM4-union_1",
                           "ETH-Dire_Dawa_ADM3-union_1",
                           "KEN-Nairobi_ADM3-union_1",
                           "MDG-Antananarivo_ADM4-union_1",
                           "MEX-Mexico_City_ADM2-union_1",
                           "MEX-Monterrey_ADM2-union_1",
                           "RWA-Musanze_ADM5-union_1")

indicators_comparison = indicators[indicators$geo_id %in% cities_comparison_list, ]
print(indicators_comparison)

############### App

ui = navbarPage("Cities4Forests-Dashboard",
                id = "active_tab",
                
                ### Indicators tab ----
                tabPanel("Indicators",
                         
                         ### Filters ----
                         fluidRow(
                           
                           
                           
                           column(3,
                                  
                                  ### Select city  ----
                                  selectInput(inputId = "city",
                                              label = "Select your city",
                                              choices = cities,
                                              selected = "COG-Brazzaville",
                                              width = '100%'),
                                  
                                  # select theme ----
                                  selectizeInput(inputId = "theme",
                                                 label = "Theme",
                                                 choices = indicators_themes,
                                                 selected = "Greenspace access", #"Greenspace access" "Health - Heat"
                                                 multiple = FALSE,
                                                 width = '100%'),
                                  
                                  # select indicator ----
                                  selectizeInput(inputId = "indicator",
                                                 label = "Select indicator",
                                                 choices = indicators_list,
                                                 selected = "Open space for public use", #  "Open space for public use" "Percent of Tree cover"
                                                 multiple = FALSE,
                                                 width = '100%'),
                                  
                                  # Main indicators
                                  
                                  h4("City wide level: "),
                                  htmlOutput("city_wide_indicator"),
                                  
                                  # plotlyOutput("cities_comparison_plot")
                                  
                           ),
                           ### Specify plots ----
                           column(8,
                                  div(style = "background-color: red; width: 100%; height: 100%;"),
                                  tabsetPanel(type = "tabs",
                                              ### Map plot
                                              tabPanel("Map", 
                                                       leafletOutput("indicator_map", 
                                                                     height = 500),
                                                       # disconnect message
                                                       disconnectMessage(
                                                         text = "An error occurred due to the data volumetry. Please refresh the page and try again with another city.",
                                                         refresh = "Refresh",
                                                         background = "#FFFFFF",
                                                         colour = "#077D29",
                                                         refreshColour = "#337AB7",
                                                         overlayColour = "#000000",
                                                         overlayOpacity = 0.6,
                                                         width = 450,
                                                         top = 50,
                                                         size = 22),
                                                       actionButton("disconnect", 
                                                                    "Disconnect the dashboard",
                                                                    width = '30%',
                                                                    # class = "btn-warning",
                                                                    # style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                                    style="color: #fff; background-color: #b3b3b3; border-color: #b3b3b3",
                                                       )),
                                              ### Table plot
                                              tabPanel("Table", DT::dataTableOutput("indicator_table",
                                                                                    height = 500),
                                                       downloadButton(outputId = "downloadData",
                                                                      label = "Download data")),
                                              ### timeseirs plot
                                              tabPanel("Chart", 
                                                       plotlyOutput("indicator_chart",
                                                                    height = 500)),
                                              ### Data description
                                              tabPanel("Definitions", htmlOutput("indicator_definition", 
                                                                                 height = 500)),
                                              
                                              ## Cities comparison
                                              tabPanel("Benchmark", plotlyOutput("cities_comparison_plot",
                                                                                 height = 500))
                                  )
                           )
                         )
                )
                
                
                
)

# Define server
server <- function(input, output, session) {
  
  # disconnect message
  observeEvent(input$disconnect, {
    session$close()
  })
  
  # Update indicators based on selected theme
  observeEvent(input$theme,{
    updateSelectInput(session,
                      'indicator',
                      choices=unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_label"]),
                      selected = unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_label"])[1],
    )
  })
  
  
  
  
  observe({
    
    # update panel data when the panel is selected
    input$active_tab
    
    geo_name = input$city
    print(geo_name)
    
    # read boundaries -----
    aoi_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "aoi_boundary_name"]
    units_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "units_boundary_name"]
    
    boundary_aoi = st_read(paste(aws_s3_path,
                                 "data/boundaries/v_0/boundary-",
                                 geo_name,
                                 "-",
                                 aoi_boundary_name,
                                 ".geojson",
                                 sep = "")
    )
    
    boundary_unit = st_read(paste(aws_s3_path,
                                  "data/boundaries/v_0/boundary-",
                                  geo_name,
                                  "-",
                                  units_boundary_name,
                                  ".geojson",
                                  sep = "")
    )
    
    # join ----------------
    
    aoi_indicators = boundary_aoi %>%
      dplyr::select(geo_id) %>%
      left_join(indicators, by = "geo_id")
    
    unit_indicators = boundary_unit %>%
      dplyr::select(geo_id) %>%
      left_join(indicators, by = "geo_id")
    
    # get selected indicator
    
    selected_indicator_label = input$indicator
    
    selected_indicator_name = indicators_definitions %>% 
      filter(indicator_label %in% selected_indicator_label) %>% 
      pull(indicator_name)
    
    # get indicator legend  -----
    selected_indicator_legend = indicators_definitions %>% 
      filter(indicator_label %in% selected_indicator_label) %>% 
      pull(indicator_legend)
    
    # get indicator values  -----
    
    selected_indicator_values = unit_indicators %>% 
      as.data.frame() %>% 
      pull(selected_indicator_name)
    
    # indicator color values ----
    
    pal_indicator<- colorNumeric(palette = "Greens", 
                                 domain = selected_indicator_values,
                                 na.color = "gray",
                                 revers = FALSE)
    
    # indicator labels for map ----
    
    labels_indicator <- sprintf("<strong>%s</strong><br/>%s: %s",
                                unit_indicators$geo_name,
                                selected_indicator_label,
                                round(selected_indicator_values, 2)) %>% 
      lapply(htmltools::HTML)
    
    
    if(input$indicator == "Open space for public use" | input$indicator == "Surface reflectivity" | input$indicator == "Tree cover in built-up areas" ){
      # layers: esa world cover  -----
      
      esa_worldcover_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                                       "data/land_use/esa_world_cover/v_0/",
                                       geo_name,
                                       "-",
                                       aoi_boundary_name,
                                       "-ESA-world_cover-2020_50m.tif",
                                       sep = "")
      
      
      # collect raster data
      esa_worldcover_data = raster(esa_worldcover_data_path)
      
      city_esa_worldcover = raster::mask(esa_worldcover_data,
                                         boundary_aoi)
      
      
      # define color palette for WOrld cover
      Trees_10_green = "#006400"
      Shrubland_20_orange = "#ffbb22"
      Grassland_30_yellow = "#ffff4c" 
      Cropland_40_mauve = "#f096ff"
      Built_up_50_red = "#fa0000"
      Barren_sparse_vegetation_60_gray = "#b4b4b4"
      Snow_ice_70_white = "#f0f0f0"
      Open_Water_80_blue = "#0064c8"
      Herbaceous_wetland_90_blue2 = "#0096a0"
      Mangroves_95_green2 = "#00cf75"
      Moss_lichen_100_beige = "#fae6a0"
      
      worldcover_col = c(Trees_10_green,
                         Shrubland_20_orange,
                         Grassland_30_yellow,
                         Cropland_40_mauve,
                         Built_up_50_red,
                         Barren_sparse_vegetation_60_gray,
                         Snow_ice_70_white,
                         Open_Water_80_blue,
                         Herbaceous_wetland_90_blue2,
                         Mangroves_95_green2,
                         Moss_lichen_100_beige)
      worldcover_labels = c('Trees','Shrubland','Grassland','Cropland','Built-up',
                            'Barren / sparse vegetation','Snow/ice','Open water','Herbaceous wetland',
                            'Mangroves','Moss/lichen')
      
      # define a color palette
      pal_worldcover <- colorFactor(palette = worldcover_col, 
                                    levels = c("10","20","30","40","50","60",
                                               "70","80","90","95","100"),
                                    na.color = "transparent")
      
    }
    
    if(input$indicator == "Open space for public use" | input$indicator == "Access to public open space"){
      
      # read OSM open space ----
      osm_open_space = st_read(paste(aws_s3_path,
                                     "data/open_space/openstreetmap/v_0/",
                                     geo_name,
                                     "-",
                                     aoi_boundary_name,
                                     "-OSM-open_space-2022.geojson",
                                     sep = "")
      )
      
    }
    
    if(input$indicator == "Access to public open space"){
      
      # layers: worldpop  -----
      
      
      pop_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                            "data/population/worldpop/v_0/",
                            geo_name,
                            "-",
                            aoi_boundary_name,
                            "-WorldPop-population.tif",
                            sep = "")
      
      print(pop_data_path)
      
      # collect raster data
      city_pop = raster(pop_data_path)
      
      city_pop_boundary = raster::mask(city_pop,
                                       boundary_aoi)
      
      # color pop
      pop_values = values(city_pop_boundary)[!is.na(values(city_pop_boundary))]
      
      pal_pop <- colorNumeric("RdYlBu",
                              pop_values,
                              na.color = "transparent",
                              reverse = TRUE)
      
      
      # layers: worldpop with open space  -----
      
      
      pop_open_space_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                                       "data/population/worldpop/v_0/",
                                       geo_name,
                                       "-",
                                       aoi_boundary_name,
                                       "-population-wOpenSpace-2020.tif",
                                       sep = "")
      
      # collect raster data
      pop_open_space_data = raster(pop_open_space_data_path)
      
      city_pop_open_space = raster::mask(pop_open_space_data,
                                         boundary_aoi)
      
      # color pop 
      pop_open_space_values = values(city_pop_open_space)[!is.na(values(city_pop_open_space))]
      
      pal_pop_open_space <- colorNumeric("RdYlBu", 
                                         pop_open_space_values,
                                         na.color = "transparent",
                                         reverse = TRUE)
    }
    
    if(input$indicator == "Percent of Tree cover" | input$indicator == "Access to tree cover"){
      
      # layers: worldpop  -----
      
      
      pop_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                            "data/population/worldpop/v_0/",
                            geo_name,
                            "-",
                            aoi_boundary_name,
                            "-WorldPop-population.tif",
                            sep = "")
      
      # collect raster data
      city_pop = raster(pop_data_path)
      
      city_pop_boundary = raster::mask(city_pop,
                                       boundary_aoi)
      
      # color pop 
      pop_values = values(city_pop_boundary)[!is.na(values(city_pop_boundary))]
      
      pal_pop <- colorNumeric("RdYlBu", 
                              pop_values,
                              na.color = "transparent",
                              reverse = TRUE)
      
      # Layers: tml ----
      
      tml_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/data/tree_cover/tree_mosaic_land/v_0/",
                            geo_name,
                            "-",
                            aoi_boundary_name,
                            "-TML-tree_cover-2020_50m.tif",
                            sep = "")
      
      
      # collect raster data
      city_tml = raster(tml_data_path)
      
      city_tml_boundary = raster::mask(city_tml,
                                       boundary_aoi)
      
      city_tml_boundary[city_tml_boundary<10] = NA
      
      # define color for tree cover
      pal_tml <- colorNumeric(palette = "Greens",
                              domain = values(city_tml_boundary), 
                              na.color = "transparent")
      
      # layers: worldpop with access to tree cover  -----
      
      
      pop_tree_cover_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                                       "data/population/worldpop/v_0/",
                                       geo_name,
                                       "-",
                                       aoi_boundary_name,
                                       "-population-wTreeCover-2020.tif",
                                       sep = "")
      
      # collect raster data
      pop_tree_cover_data = raster(pop_tree_cover_data_path)
      
      city_pop_tree_cover = raster::mask(pop_tree_cover_data,
                                         boundary_aoi)
      
      # color pop 
      pop_tree_cover_values = values(city_pop_tree_cover)[!is.na(values(city_pop_tree_cover))]
      
      pal_pop_tree_cover <- colorNumeric("RdYlBu", 
                                         pop_tree_cover_values,
                                         na.color = "transparent",
                                         reverse = TRUE)
      
      
    }
    
    if(input$indicator == "Tree cover in built-up areas" ){
      
      # Layers: tml ----
      
      tml_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/data/tree_cover/tree_mosaic_land/v_0/",
                            geo_name,
                            "-",
                            aoi_boundary_name,
                            "-TML-tree_cover-2020_50m.tif",
                            sep = "")
      
      
      # collect raster data
      city_tml = raster(tml_data_path)
      
      city_tml_boundary = raster::mask(city_tml,
                                       boundary_aoi)
      
      city_tml_boundary[city_tml_boundary<10] = NA
      
      # define color for tree cover
      pal_tml <- colorNumeric(palette = "Greens",
                              domain = values(city_tml_boundary), 
                              na.color = "transparent")
      
      
    }
    
    if(input$indicator == "Population exposure to PM 2.5"){
      
      # layers: worldpop  -----
      
      
      pop_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                            "data/population/worldpop/v_0/",
                            geo_name,
                            "-",
                            aoi_boundary_name,
                            "-WorldPop-population.tif",
                            sep = "")
      
      print(pop_data_path)
      
      # collect raster data
      city_pop = raster(pop_data_path)
      
      city_pop_boundary = raster::mask(city_pop,
                                       boundary_aoi)
      
      # color pop
      pop_values = values(city_pop_boundary)[!is.na(values(city_pop_boundary))]
      
      pal_pop <- colorNumeric("RdYlBu",
                              pop_values,
                              na.color = "transparent",
                              reverse = TRUE)
      
    }
    
    
    
    ########################
    # map indicator ----
    ########################
    
    
    m = leaflet(boundary_aoi) %>%
      addTiles() %>%
      fitBounds(~as.numeric(st_bbox(boundary_aoi)[1]),
                ~as.numeric(st_bbox(boundary_aoi)[2]),
                ~as.numeric(st_bbox(boundary_aoi)[3]),
                ~as.numeric(st_bbox(boundary_aoi)[4])) %>% 
      # boundaries ----
    addPolygons(data = boundary_aoi,
                group = "Administrative boundaries",
                stroke = TRUE, color = "black", weight = 3,dashArray = "3",
                smoothFactor = 0.5, fill = FALSE, fillOpacity = 0.5,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.3,
                  bringToFront = TRUE),
                label = boundary_aoi$geo_name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>% 
      # Map indicator ----
    addPolygons(data = unit_indicators,
                group = selected_indicator_label,
                fillColor = ~pal_indicator(selected_indicator_values),
                weight = 1,
                opacity = 1,
                color = "grey",
                fillOpacity = 0.8,
                label = labels_indicator,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = FALSE),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 6px"),
                  textsize = "15px",
                  direction = "auto")) %>%
      addLegend(pal = pal_indicator,
                values = selected_indicator_values,
                opacity = 0.9,
                title = selected_indicator_legend,
                group = selected_indicator_label,
                position = "topright",
                labFormat = labelFormat(suffix = "")) %>% 
      # Layers control
      addLayersControl(
        overlayGroups = c("Administrative boundaries",
                          # "Open Space Areas",
                          selected_indicator_label),
        options = layersControlOptions(collapsed = FALSE)
      ) 
    
    # Open space for public use - Add layers ----
    
    if(input$indicator == "Open space for public use"){
      m = m %>% 
        # plot layer: OSM
        addPolygons(data = osm_open_space,
                    group = "Open Space Areas",
                    stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                    smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.3,
                      bringToFront = TRUE)) %>% 
        # plot layer: ESA world cover ----
      addRasterImage(city_esa_worldcover,
                     colors = pal_worldcover,
                     opacity = 1,
                     maxBytes = 100 * 1024 * 1024,
                     project=FALSE,
                     group = "Land cover types") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "World Cover",
                  group = "Land cover types",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Layers control
        addLayersControl(
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Open Space Areas",
                            "Land cover types"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(c("Open Space Areas",
                    "Land cover types")) 
    }
    
    # Access to public open space - Add layers ----
    if(input$indicator == "Access to public open space"){
      m = m %>% 
        # plot layer: OSM ----
      addPolygons(data = osm_open_space,
                  group = "Open Space Areas",
                  stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                  smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.3,
                    bringToFront = TRUE)) %>% 
        # plot layer: POP ----
      addRasterImage(city_pop_boundary,
                     colors = pal_pop ,
                     opacity = 0.9,
                     group = "Population",
                     project=FALSE,
                     maxBytes = 8 * 1024 * 1024,
                     layerId = "Population") %>% 
        # Legend for population 
        addLegend(pal = pal_pop ,
                  values = pop_values,
                  opacity = 0.9,
                  title = "Population count </br> (persons per 100m)",
                  group = "Population",
                  position = "bottomleft") %>%
        # plot layer: POP with open space ----
      addRasterImage(city_pop_open_space,
                     colors = pal_pop_open_space ,
                     opacity = 0.9,
                     group = "Population - open space",
                     project=FALSE,
                     maxBytes = 8 * 1024 * 1024) %>%
        # Legend
        addLegend(pal = pal_pop_open_space ,
                  values = pop_open_space_values,
                  opacity = 0.9,
                  title = "Population with access to </br> open space  (persons per 100m)",
                  group = "Population - open space",
                  position = "bottomleft") %>%
        # Layers control ----
      addLayersControl(
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_label,
                          "Open Space Areas",
                          "Population",
                          "Population - open space"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
        hideGroup(c("Population",
                    "Open Space Areas",
                    "Population - open space")) 
    }
    
    # Access to tree cover - Add layers ----
    if(input$indicator == "Access to tree cover"){
      m = m %>% 
        # plot layer: POP
        addRasterImage(city_pop_boundary,
                       colors = pal_pop ,
                       opacity = 0.9,
                       group = "Population",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Population") %>% 
        # Legend for population 
        addLegend(pal = pal_pop ,
                  values = pop_values,
                  opacity = 0.9,
                  title = "Population count </br> (persons per 100m)",
                  group = "Population",
                  position = "bottomleft") %>%
        # plot layer: POP with access to tree cover ----
      addRasterImage(city_pop_tree_cover,
                     colors = pal_pop_tree_cover,
                     opacity = 0.9,
                     group = "Population - Tree cover",
                     project=FALSE,
                     maxBytes = 8 * 1024 * 1024) %>%
        # Raster of tree cover
        addRasterImage(city_tml_boundary, #city_tml_aggregate
                       colors = pal_tml,
                       opacity = 0.9,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Tree cover") %>%
        addLegend(pal = pal_tml,
                  values = values(city_tml_boundary), #values(city_tml_aggregate),
                  title = "Tree cover percent",
                  group = "Tree cover",
                  position = "bottomleft") %>%
        # Legend
        addLegend(pal = pal_pop_tree_cover ,
                  values = pop_tree_cover_values,
                  opacity = 0.9,
                  title = "Population with access to </br> Tree Cover  (persons per 100m)",
                  group = "Population - Tree cover",
                  position = "bottomleft") %>%
        # Layers control
        addLayersControl(
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Population",
                            "Population - Tree cover",
                            "Tree cover"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(c("Population",
                    "Population - Tree cover",
                    "Tree cover")) 
    } 
    
    # Percent of tree cover - Add layers ----
    if(input$indicator == "Tree cover in built-up areas"){
      m = m %>% 
        # Raster of tree cover ----
      addRasterImage(city_tml_boundary, 
                     colors = pal_tml,
                     opacity = 0.9,
                     maxBytes = 20 * 1024 * 1024,
                     project=FALSE,
                     group = "Tree cover") %>%
        addLegend(pal = pal_tml,
                  values = values(city_tml_boundary), 
                  title = "Tree cover percent",
                  group = "Tree cover",
                  position = "bottomleft") %>%
        # plot layer: ESA world cover ----
      addRasterImage(city_esa_worldcover,
                     colors = pal_worldcover,
                     opacity = 1,
                     maxBytes = 100 * 1024 * 1024,
                     project=FALSE,
                     group = "Land cover types") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "World Cover",
                  group = "Land cover types",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Layers control
        addLayersControl(
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Tree cover",
                            "Land cover types"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(c("Tree cover","Land cover types")) 
    }
    
    
    # Population exposure to pm2.5 - Add layers ----
    if(input$indicator == "Population exposure to PM 2.5"){
      m = m %>% 
        # plot layer: POP ----
      addRasterImage(city_pop_boundary,
                     colors = pal_pop ,
                     opacity = 0.9,
                     group = "Population",
                     project=FALSE,
                     maxBytes = 8 * 1024 * 1024,
                     layerId = "Population") %>% 
        # Legend for population 
        addLegend(pal = pal_pop ,
                  values = pop_values,
                  opacity = 0.9,
                  title = "Population count </br> (persons per 100m)",
                  group = "Population",
                  position = "bottomleft") %>%
        # Layers control ----
      addLayersControl(
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_label,
                          "Population"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
        hideGroup(c("Population")) 
    }
    
    # center map  
    output$indicator_map <- renderLeaflet({
      m
    })
    
    if(input$indicator == "Surface reflectivity"){
      m = m %>% 
        # plot layer: ESA world cover ----
      addRasterImage(city_esa_worldcover,
                     colors = pal_worldcover,
                     opacity = 1,
                     maxBytes = 100 * 1024 * 1024,
                     project=FALSE,
                     group = "Land cover types") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "World Cover",
                  group = "Land cover types",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Layers control
        addLayersControl(
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Land cover types"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(c("Land cover types")) 
    }
    
    
    
    #########################################
    ### Main indicators ----
    
    # city wide  ----
    city_wide_indicator_value = aoi_indicators %>%
      as.data.frame() %>%
      pull(selected_indicator_name) %>% 
      round(2)
    
    output$city_wide_indicator <- renderText({
      paste("<center>","<font size=5px; weight=500; color=\"#168A06\"><b>", city_wide_indicator_value, "%")
    })
    
    #########################################
    ### Table ----
    
    # Table plot
    
    table_plot = unit_indicators %>% 
      drop_na(selected_indicator_name, geo_name) %>% 
      as.data.frame() %>%
      dplyr::select(-geometry) %>% 
      dplyr::select(geo_name,selected_indicator_name) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      arrange(desc(selected_indicator_name)) 
    
    # remove empty city name
    table_plot$geo_name[table_plot$geo_name==""]<-NA
    table_plot = table_plot %>% 
      drop_na(geo_name)
    
    names(table_plot) = c("City name",selected_indicator_label)
    
    output$indicator_table <- DT::renderDataTable(
      DT::datatable(table_plot, 
                    options = list(pageLength = 10,order = list(list(2, 'desc')))) %>% formatStyle(
                      selected_indicator_label,
                      backgroundColor = styleInterval(seq(from = min(table_plot[,selected_indicator_label]),
                                                          to = max(table_plot[,selected_indicator_label]),
                                                          length.out = 8), 
                                                      brewer.pal(9, "Greens")
                      ),
                      fontWeight = 'bold')
    )
    
    
    # output data to download
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$city,"-", input$indicator,"-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(table_plot, file)
      }
    )
    
    #########################################
    ### Chart ----
    
    # keep distinct city_name
    table_plot = table_plot %>% 
      distinct(`City name`, .keep_all = TRUE)
    
    output$indicator_chart <- renderPlotly({
      fig = plot_ly(x = table_plot$`City name`,
                    y = table_plot[[colnames(table_plot)[2]]],
                    type = "bar",
                    orientation = "v",
                    name = names(table_plot)[2],
                    color = I("green4")) %>% 
        layout(yaxis = list(title = names(table_plot)[2]),
               xaxis = list(title = 'Cities',categoryorder = "total descending"))
      
      fig
      
    })
    
    #########################################
    ### Cities comparison  ----
    
    # cities comparison ----
    
    print(indicators_comparison)
    
    indicators_comparison = indicators_comparison %>% 
      as.data.frame() %>%
      dplyr::select(geo_name,selected_indicator_name) %>% 
      drop_na(selected_indicator_name, geo_name) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      arrange(desc(selected_indicator_name)) 
    
    print(indicators_comparison)
    
    # change names
    names(indicators_comparison) = c("City name",selected_indicator_label)
    
    city_num = which(indicators_comparison$`City name` == geo_name)
    city_color = rep("grey",nrow(indicators_comparison))
    city_color[city_num] = "green"
    
    
    
    cities_indicator_avg = round(mean(indicators_comparison[[colnames(indicators_comparison)[2]]]),2)
    
    output$cities_comparison_plot <- renderPlotly({
      fig = plot_ly(x = indicators_comparison$`City name`,
                    y = indicators_comparison[[colnames(indicators_comparison)[2]]],
                    type = "bar",
                    orientation = "v",
                    name = names(indicators_comparison)[2],
                    marker = list(color = city_color)) %>% 
        layout(yaxis = list(title = names(indicators_comparison)[2]),
               xaxis = list(title = 'Cities',categoryorder = "total descending"),
               annotations = list(
                 x = 10,
                 y = cities_indicator_avg, 
                 text = paste("Cities' averrage: ", cities_indicator_avg, " %", sep = ""),
                 showarrow = FALSE,
                 xanchor = "right"
               ))
      
      add_trace(fig,
                y = cities_indicator_avg, 
                type='scatter',
                mode = 'lines',
                # mode = 'lines+markers',
                name = 'Average', 
                showlegend = F,
                line = list(color = 'black', 
                            dash = 'dot',
                            width = 1)) 
      
    })
    
    
    
    #########################################
    ### Indicator definition text  ----
    
    indicator_def_text = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>% 
      pull(indicator_definition)
    
    indicator_data_sources = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>%  
      pull(data_sources)
    
    indicator_importance = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>%  
      pull(importance)
    
    indicator_methods = indicators_definitions %>% 
      filter(indicator_label == selected_indicator_label) %>%  
      pull(methods)
    
    # plot text 
    output$indicator_definition <- renderText({
      paste("<right>","<font size=3px; weight=100; color=\"#168A06\"><b>",
            "<font color=\"#168A06\"><b>", " ", "<br>",
            "<font color=\"#168A06\"><b>","Definition: ",
            "<font color=\"#454545\"><b>", indicator_def_text,
            "<br/>",
            "<font color=\"#168A06\"><b>", " ", "<br>",
            "<font color=\"#168A06\"><b>","Data sources: ",
            "<font color=\"#454545\"><b>", indicator_data_sources,
            "<br/>",
            "<font color=\"#168A06\"><b>", " ", "<br>",
            "<font color=\"#168A06\"><b>","Importance: ",
            "<font color=\"#454545\"><b>", indicator_importance,
            "<br/>",
            "<font color=\"#168A06\"><b>", " ", "<br>",
            "<font color=\"#168A06\"><b>","Methods: ",
            "<font weight=50; color=\"#454545\"><b>", indicator_methods
      )
    })
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
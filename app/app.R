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
library(shinyjs)
library(leaflet.multiopacity)

# define aws s3 path

aws_s3_path = "https://cities-cities4forests.s3.eu-west-3.amazonaws.com/"

############### Load data

# read indicator definition ------------



indicators_definitions = read.csv(paste(aws_s3_path,
                                        "data/indicators/indicators_definition_v2.csv",
                                        sep = ""))

# indicators_definitions = indicators_definitions %>% 
#   add_row(theme = "Health - Air Quality",
#           indicator_label = "Air pollutant emissions",
#           indicator_code = "2_1",
#           indicator_name = "GRE_2_1_air_pollution",
#           indicator_legend = "Air pollutant emissions <br> (Tonnes)",
#           indicator_definition = " ",
#           data_sources = " ",
#           importance = " ",
#           methods = " ")


# indicators_definitions_labels = indicators_definitions %>% 
#   filter(indicator_label != "High pollution days (carbon monoxide)")

indicators_definitions_labels = indicators_definitions %>% 
  filter(!indicator_label %in% c("High pollution days (carbon monoxide)",
                                 "Air pollution (by pollutant)",
                                 "Air pollution (by sector)"))



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




##########################################################


# read indicators v2 -----

indicators_v2 = read.csv(paste(aws_s3_path,
                               "data/indicators/cities_indicators_v2test.csv",
                               sep = ""),
                         encoding="UTF-8")


indicators = indicators_v2 %>% 
  dplyr::select(geo_id,
                geo_level,
                geo_name,
                geo_parent_name,
                # GRE_1_1_percentChangeinDaysAbove35C2020to2050,
                GRE_1_2_percentBuiltupwHighLST.2013to2022meanofmonthwhottestday,
                GRE_1_3_percentBuiltwLowAlbedo,
                #GRE_1_4_percentBuiltupWithoutTreeCover,
                GRE_3_1_percentOpenSpaceinBuiltup,
                GRE_3_2_percentPopwOpenSpaceAccess,
                GRE_3_3_percentPopwTreeCoverAccess,
                GRE_4_1_percentFloodProneinBuiltup2050,
                # GRE_4_2_percentChangeinMaxDailyPrecip2020to2050,
                GRE_4_3_percentBuiltupWithin1mAboveDrainage,
                GRE_4_4_percentImperviousinBuiltup2018,
                GRE_4_5_percentBuiltupWOvegetationcover2020,
                GRE_4_6_percentRiparianZonewoVegorWatercover2020,
                GRE_4_7_percentSteepSlopesWOvegetationcover2020,
                GRE_5_2_meanannualTreeCarbonFluxMgcO2eperHA) %>% 
  mutate_at(vars(matches("GRE")), ~ .* 100) %>% 
  mutate(GRE_5_2_meanannualTreeCarbonFluxMgcO2eperHA = 0.01 * GRE_5_2_meanannualTreeCarbonFluxMgcO2eperHA)

# process GRE_1_1 -----

indicators_v2_GRE_1_1 = indicators_v2 %>% 
  drop_na(GRE_1_1_percentChangeinDaysAbove35C2020to2050) %>% 
  dplyr::select(geo_parent_name,
                GRE_1_1_percentChangeinDaysAbove35C2020to2050) %>% 
  mutate_at(vars(matches("GRE")), ~ .* 100)

indicators = indicators %>% 
  right_join(indicators_v2_GRE_1_1,
             by = "geo_parent_name") 



# GRE_1_4 ----
indicators_GRE_1_4 = read.csv(paste(aws_s3_path,
                                    "data/indicators/dev/cities_indicators_GRE_1_4.csv",
                                    sep = ""),
                              encoding="UTF-8")

indicators_GRE_1_4 = indicators_GRE_1_4 %>% 
  dplyr::select(geo_id,
                GRE_1_4_percent_tree_cover_builtup_areas) %>% 
  mutate(GRE_1_4_percent_builtup_areas_without_tree_cover = 100 - GRE_1_4_percent_tree_cover_builtup_areas)


indicators = indicators %>% 
  left_join(indicators_GRE_1_4,
            by = "geo_id")


# GRE_2_1 ----
# indicators_GRE_2_1 = read.csv(paste(aws_s3_path,
#                                     "data/indicators/dev/cities_indicators_GRE_2_1.csv",
#                                     sep = ""),
#                               encoding="UTF-8")
# 
# 
# indicators = indicators %>% 
#   left_join(indicators_GRE_2_1,
#             by = "geo_id")


# GRE_2_1 v2 ----
indicators_GRE_2_1_v2 = read.csv(paste(aws_s3_path,
                                       "data/indicators/GRE-2.1.csv",
                                       sep = ""),
                                 encoding="UTF-8")

indicators_GRE_2_1_aoi = indicators_GRE_2_1_v2 %>%
  add_column(geo_parent_name = c("BRA-Salvador",
                                 "COD-Bukavu",
                                 "COD-Uvira" ,
                                 "COG-Brazzaville",
                                 "COL-Barranquilla",
                                 "ETH-Addis_Ababa",
                                 "ETH-Dire_Dawa",
                                 "KEN-Nairobi",
                                 "MDG-Antananarivo",
                                 "MEX-Mexico_City",
                                 "MEX-Monterrey",
                                 "RWA-Musanze")) %>%
  dplyr::select(geo_parent_name,
                GRE_2_1_air_pollution =  total_tonnes_2020) %>%
  mutate_if(is.numeric, round, 0)

indicators = indicators %>% 
  right_join(indicators_GRE_2_1_aoi,
             by = "geo_parent_name") 


indicators_GRE_2_1_unit = indicators_GRE_2_1_v2 %>% 
  dplyr::select(c(1:97),115) %>% 
  rename(geo_id = X)

indicators = indicators %>% 
  left_join(indicators_GRE_2_1_unit,
            by = "geo_id")


# GRE_2_2 ----
indicators_GRE_2_2 = read.csv(paste(aws_s3_path,
                                    "data/indicators/dev/cities_indicators_GRE_2_2.csv",
                                    sep = ""),
                              encoding="UTF-8")

indicators = indicators %>% 
  left_join(indicators_GRE_2_2[,c("geo_id",
                                  "GRE_2_2_nb_exceedance_days_nitroge_dioxide",
                                  "GRE_2_2_nb_exceedance_days_sulfur_dioxide",
                                  "GRE_2_2_nb_exceedance_days_ozone",
                                  "GRE_2_2_nb_exceedance_days_fine_particulate_matter",
                                  "GRE_2_2_nb_exceedance_days_coarse_particulate_matter",
                                  "GRE_2_2_nb_exceedance_days_coarse_carbon_monoxide")],
            by = "geo_id") 

# new version

indicators_GRE_2_2_new = read.csv(paste(aws_s3_path,
                                        "data/indicators/GRE-2.2.csv",
                                        sep = ""),
                                  encoding="UTF-8")

indicators_GRE_2_2_new = indicators_GRE_2_2_new %>% 
  dplyr::select("geo_parent_name",
                "GRE_2_2_2_exceedancedays_nitrogen.dioxide" = "exceedancedays_nitrogen.dioxide",
                "GRE_2_2_2_exceedancedays_sulfur.dioxide" = "exceedancedays_sulfur.dioxide",
                "GRE_2_2_2_exceedancedays_ozone" = "exceedancedays_ozone",
                "GRE_2_2_2_exceedancedays_fine.particulate.matter" = "exceedancedays_fine.particulate.matter",
                "GRE_2_2_2_exceedancedays_coarse.particulate.matter" = "exceedancedays_coarse.particulate.matter",
                "GRE_2_2_2_exceedancedays_carbon.monoxide" = "exceedancedays_carbon.monoxide",
                "GRE_2_2_2_exceedancedays_additive" = "exceedancedays_additive",
                "GRE_2_2_exceedancedays_atleastone" = "exceedancedays_atleastone")

# join with parent_name to replicate values at the subcity level 
indicators = indicators %>% 
  right_join(indicators_GRE_2_2_new,
             by = "geo_parent_name") 



# GRE_2_3 ----
# indicators_GRE_2_3 = read.csv(paste(aws_s3_path,
#                                     "data/indicators/dev/cities_indicators_GRE_2_3.csv",
#                                     sep = ""),
#                               encoding="UTF-8")
# 
# indicators = indicators %>% 
#   left_join(indicators_GRE_2_3[,c("geo_id","GRE_2_3_population_exposure_pm25")],
#             by = "geo_id") %>% 
#   mutate(GRE_2_3_population_exposure_pm25 = 100 * GRE_2_3_population_exposure_pm25)




indicators_GRE_2_3 = read.csv(paste(aws_s3_path,
                                    "data/indicators/GRE-2.3.csv",
                                    sep = ""),
                              encoding="UTF-8")

indicators = indicators %>% 
  left_join(indicators_GRE_2_3[,c("geo_id","exceedance_popfraction_2020")],
            by = "geo_id") %>% 
  mutate(GRE_2_3_population_exposure_pm25 = na_if(exceedance_popfraction_2020, -9999)) %>% 
  mutate(GRE_2_3_population_exposure_pm25 = 100 * GRE_2_3_population_exposure_pm25)

# GRE_4_2 ----
indicators_GRE_4_2 = read.csv(paste(aws_s3_path,
                                    "data/indicators/dev/cities_indicators_GRE_4_2.csv",
                                    sep = ""),
                              encoding="UTF-8")

indicators = indicators %>% 
  left_join(indicators_GRE_4_2[,c("geo_id",
                                  "GRE_4_2_percentChangeinMaxDailyPrecip2020to2050")],
            by = "geo_id") %>% 
  mutate(GRE_4_2_percentChangeinMaxDailyPrecip2020to2050 = 100 * GRE_4_2_percentChangeinMaxDailyPrecip2020to2050)

# GRE_5_1 ----
indicators_GRE_5_1 = read.csv(paste(aws_s3_path,
                                    "data/indicators/GRE-5.1.csv",
                                    sep = ""),
                              encoding="UTF-8")

# indicators_GRE_5_1 = indicators_GRE_5_1 %>%
#   add_column(geo_parent_name = c("BRA-Salvador",
#                                  "COD-Bukavu",
#                                  "COD-Uvira" ,
#                                  "COG-Brazzaville",
#                                  "COL-Barranquilla",
#                                  "ETH-Addis_Ababa",
#                                  "ETH-Dire_Dawa",
#                                  "KEN-Nairobi",
#                                  "MDG-Antananarivo",
#                                  "MEX-Mexico_City",
#                                  "MEX-Monterrey",
#                                  "RWA-Musanze")) %>%
#   dplyr::select(geo_parent_name,
#                 GRE_5_1_ghg_emissions =  total_CO2e) %>%
#   mutate_if(is.numeric, round, 0)
# 
# # join with parent_name to replicate values at the subcity level
# indicators = indicators %>% 
#   right_join(indicators_GRE_5_1,
#              by = "geo_parent_name") 


indicators_GRE_5_1_aoi = indicators_GRE_5_1 %>%
  add_column(geo_parent_name = c("BRA-Salvador",
                                 "COD-Bukavu",
                                 "COD-Uvira" ,
                                 "COG-Brazzaville",
                                 "COL-Barranquilla",
                                 "ETH-Addis_Ababa",
                                 "ETH-Dire_Dawa",
                                 "KEN-Nairobi",
                                 "MDG-Antananarivo",
                                 "MEX-Mexico_City",
                                 "MEX-Monterrey",
                                 "RWA-Musanze")) %>%
  dplyr::select(geo_parent_name,
                GRE_5_1_ghg_emissions =  total_CO2e) %>%
  mutate_if(is.numeric, round, 0)

indicators = indicators %>% 
  right_join(indicators_GRE_5_1_aoi,
             by = "geo_parent_name") 


indicators_GRE_5_1_unit = indicators_GRE_5_1 %>% 
  dplyr::select(c(1:85),105) %>% 
  rename(geo_id = X)

indicators = indicators %>% 
  left_join(indicators_GRE_5_1_unit,
            by = "geo_id")


# keep distinct geo_id ----
indicators = indicators %>% 
  distinct(geo_id, .keep_all = TRUE)



##########################################################

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


############### App


ui <- tagList(
  useShinyjs(),
  navbarPage("Cities4Forests-Dashboard",
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
                               
                               # # select air polluant ----
                               # hidden(selectizeInput(inputId = "indicator_air_polluant",
                               #                       label = "Select air polluant",
                               #                       choices = c("GRE_2_2_nb_exceedance_days_nitroge_dioxide",
                               #                                   "GRE_2_2_nb_exceedance_days_sulfur_dioxide"),
                               #                       selected = "GRE_2_2_nb_exceedance_days_nitroge_dioxide", 
                               #                       multiple = FALSE,
                               #                       width = '100%')),
                               
                               # Main indicators
                               
                               h4("City wide level: "),
                               htmlOutput("city_wide_indicator"),
                               
                               # plotlyOutput("cities_comparison_plot")
                               
                        ),
                        ### Specify plots ----
                        column(8,
                               div(style = "background-color: red; width: 100%; height: 100%;"),
                               tabsetPanel(type = "tabs",
                                           id = "tabs",
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
                                           
                                           ### timeseirs plot
                                           tabPanel("Chart", 
                                                    plotlyOutput("indicator_chart",
                                                                 height = 500)),
                                           
                                           ### Table plot
                                           tabPanel("Table", DT::dataTableOutput("indicator_table",
                                                                                 height = 500),
                                                    downloadButton(outputId = "downloadData",
                                                                   label = "Download data")),
                                           
                                           
                                           
                                           ## Cities comparison
                                           tabPanel("Benchmark", plotlyOutput("cities_comparison_plot",
                                                                              height = 500)),
                                           
                                           ### Data description
                                           tabPanel("Definitions", htmlOutput("indicator_definition", 
                                                                              height = 500)),
                               )
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
  
  # # Update indicators based on selected theme
  observeEvent(input$theme,{
    updateSelectInput(session,
                      'indicator',
                      choices=unique(indicators_definitions_labels[indicators_definitions_labels$theme==input$theme, "indicator_label"]),
                      selected = unique(indicators_definitions_labels[indicators_definitions_labels$theme==input$theme, "indicator_label"])[1],
    )
  })
  
  
  
  # hide tab based on selected indicator
  observeEvent(input$indicator, {
    if(input$indicator %in% c("High pollution days",
                              "Greenhouse gas emissions",
                              "Air pollutant emissions")){
      showTab(inputId = "tabs", target = "Chart")
      showTab(inputId = "tabs", target = "Definitions")
      showTab(inputId = "tabs", target = "Table")
      hideTab(inputId = "tabs", target = "Map")
      showTab(inputId = "tabs", target = "Benchmark")
      show("city_wide_indicator") 
    } else if(input$indicator == "Air pollution (by pollutant)"){
      hideTab(inputId = "tabs", target = "Table")
      hideTab(inputId = "tabs", target = "Map")
      hideTab(inputId = "tabs", target = "Benchmark") 
      hide("city_wide_indicator") 
    } else if(input$indicator == "Air pollution (by sector)"){
      hideTab(inputId = "tabs", target = "Table")
      hideTab(inputId = "tabs", target = "Map")
      hideTab(inputId = "tabs", target = "Benchmark") 
      hide("city_wide_indicator") 
    } else if(input$indicator == "Population exposure to PM 2.5"){
      showTab(inputId = "tabs", target = "Benchmark") 
      showTab(inputId = "tabs", target = "Definitions") 
      showTab(inputId = "tabs", target = "Table")
      showTab(inputId = "tabs", target = "Chart")
      showTab(inputId = "tabs", target = "Map")
      show("city_wide_indicator") 
    } else if(input$indicator  %in% c("Extreme heat hazard",
                                      "Extreme precipitation hazard")){
      showTab(inputId = "tabs", target = "Benchmark") 
      showTab(inputId = "tabs", target = "Definitions") 
      hideTab(inputId = "tabs", target = "Table")
      hideTab(inputId = "tabs", target = "Chart")
      hideTab(inputId = "tabs", target = "Map")
      show("city_wide_indicator") 
    } else if(!input$indicator %in% c("Population exposure to PM 2.5",
                                      "High pollution days",
                                      "Air pollution (by pollutant)",
                                      "Air pollution (by sector)")){
      showTab(inputId = "tabs", target = "Table")
      showTab(inputId = "tabs", target = "Chart")
      showTab(inputId = "tabs", target = "Map")
      showTab(inputId = "tabs", target = "Benchmark") 
      show("city_wide_indicator") 
    } 
    
  })
  
  
  observe({
    
    # update panel data when the panel is selected
    input$active_tab
    
    geo_name = input$city
    
    
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
    
    if(input$indicator %in% c("Air pollution (by pollutant)")){
      # since the label is used to extract data and create the maps we need to provide a field
      # for which we have data to generate the maps even if we will not show these maps. We selected by defualt the
      # (carbon monoxide) but it could be any other pollutant. In the future we need to find a a more appropriate way to
      # avoid generating a map when this indicator is selected
      selected_indicator_label = "High pollution days (carbon monoxide)"
    } else{
      selected_indicator_label = input$indicator
    }
    
    # selected_indicator_label = input$indicator
    
    
    
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
    
    # if(input$indicator %in% c("Exposure to coastal and river flooding",
    #                           "Extreme precipitation hazard")){
    #   pal_indicator<- colorNumeric(palette = "Blues", 
    #                                domain = selected_indicator_values,
    #                                na.color = "gray",
    #                                revers = FALSE)
    #   table_color = "Blues"
    # } else {
    #   pal_indicator<- colorNumeric(palette = "Greens", 
    #                                domain = selected_indicator_values,
    #                                na.color = "gray",
    #                                revers = FALSE)
    #   
    #   table_color = "Greens"
    # }
    
    pal_indicator<- colorNumeric(palette = "Greens", 
                                 domain = selected_indicator_values,
                                 na.color = "gray",
                                 revers = FALSE)
    
    table_color = "Greens"
    
    
    
    # indicator labels for map ----
    
    labels_indicator <- sprintf("<strong>%s</strong><br/>%s: %s",
                                unit_indicators$geo_name,
                                selected_indicator_label,
                                round(selected_indicator_values, 2)) %>% 
      lapply(htmltools::HTML)
    
    
    # layers: esa world cover ----
    
    if(input$indicator %in% c("Open space for public use",
                              "Surface reflectivity",
                              "Built land without tree cover",
                              "Exposure to coastal and river flooding",
                              "Land near natural drainage",
                              "Built areas without vegetation cover",
                              "Impervious surfaces",
                              "High land surface temperature")){
      
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
    
    # layers: OSM open space ----
    if(input$indicator == "Open space for public use" | input$indicator == "Access to public open space"){
      osm_open_space = st_read(paste(aws_s3_path,
                                     "data/open_space/openstreetmap/v_0/",
                                     geo_name,
                                     "-",
                                     aoi_boundary_name,
                                     "-OSM-open_space-2022.geojson",
                                     sep = "")
      )
      
    }
    
    # layers: worldpop  -----
    if(input$indicator == "Access to public open space"){
      
      if(geo_name == "MEX-Mexico_City"){
        pop_data_path = paste("https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                              "data/population/worldpop/v_0/",
                              geo_name,
                              "-",
                              aoi_boundary_name,
                              "-WorldPop-population.tif",
                              sep = "")
      } else {
        pop_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                              "data/population/worldpop/v_0/",
                              geo_name,
                              "-",
                              aoi_boundary_name,
                              "-WorldPop-population.tif",
                              sep = "")
        
      }
      
      
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
      
      if(geo_name == "MEX-Mexico_City"){
        pop_open_space_data_path = paste("https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                                         "data/population/worldpop/v_0/",
                                         geo_name,
                                         "-",
                                         aoi_boundary_name,
                                         "-WorldPop-population.tif",
                                         sep = "")
      } else {
        pop_open_space_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                                         "data/population/worldpop/v_0/",
                                         geo_name,
                                         "-",
                                         aoi_boundary_name,
                                         "-population-wOpenSpace-2020.tif",
                                         sep = "")
        
      }
      
      
      
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
    
    # layers: worldpop tre cover ---- -----
    if(input$indicator %in% c("Percent of Tree cover", "Access to tree cover")){
      
      
      # layers: worldpop
      
      if(geo_name == "MEX-Mexico_City"){
        pop_data_path = paste("https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                              "data/population/worldpop/v_0/",
                              geo_name,
                              "-",
                              aoi_boundary_name,
                              "-WorldPop-population.tif",
                              sep = "")
      } else {
        pop_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                              "data/population/worldpop/v_0/",
                              geo_name,
                              "-",
                              aoi_boundary_name,
                              "-WorldPop-population.tif",
                              sep = "")
        
      }
      
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
    
    # Layers: population with tree cover ----
    if(input$indicator == "Access to tree cover" & !input$city %in% c("BRA-Salvador",
                                                                      "MEX-Monterrey") ){
      
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
    
    # Layers: tml ----
    if(input$indicator %in% c("Built land without tree cover",
                              "Percent of Tree cover",
                              "Access to tree cover") & !input$city %in% c("BRA-Salvador",
                                                                           "MEX-Monterrey")){
      
      
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
      
      city_tml_boundary[city_tml_boundary==0] = NA
      # city_tml_boundary[city_tml_boundary<10] = NA
      
      # define color for tree cover
      pal_tml <- colorNumeric(palette = "Greens",
                              domain = values(city_tml_boundary), 
                              na.color = "transparent")
      
      
    }
    
    # layers: worldpop  -----
    if(input$indicator == "Population exposure to PM 2.5"){
      
      # layers: worldpop  ---
      
      
      if(geo_name == "MEX-Mexico_City"){
        pop_data_path = paste("https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                              "data/population/worldpop/v_0/",
                              geo_name,
                              "-",
                              aoi_boundary_name,
                              "-WorldPop-population.tif",
                              sep = "")
      } else {
        pop_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                              "data/population/worldpop/v_0/",
                              geo_name,
                              "-",
                              aoi_boundary_name,
                              "-WorldPop-population.tif",
                              sep = "")
        
      }
      
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
    
    # layers: Flooding  -----
    if(input$indicator == "Exposure to coastal and river flooding"){
      
      # layers: Flooding  --
      flood_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                              "data/flooding/aqueduct/v_0/",
                              geo_name,
                              "-",
                              aoi_boundary_name,
                              "-flood-innundation-2050-rp0100.tif",
                              sep = "")
      
      
      # collect raster data
      city_flood = raster(flood_data_path)
      
      city_flood_boundary = raster::mask(city_flood,
                                         boundary_aoi)
      
      # color flood 
      flood_values = values(city_flood_boundary)[!is.na(values(city_flood_boundary))]
      
      pal_flood <- colorNumeric("Blues", 
                                flood_values,
                                na.color = "transparent",
                                reverse = FALSE)
      
      
      
    }
    
    
    # layers: Flooding HAND  -----
    if(input$indicator == "Land near natural drainage"){
      
      hand_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                             "data/flooding/global30mHAND/v_0/",
                             geo_name,
                             "-",
                             aoi_boundary_name,
                             "-HAND-1m-1000km2FA.tif",
                             sep = "")
      
      
      # collect raster data
      city_flood_hand = raster(hand_data_path)
      
      city_flood_hand_boundary = raster::mask(city_flood_hand,
                                              boundary_aoi)
      
      city_flood_hand_boundary[city_flood_hand_boundary<1] = NA
      
      
      
    }
    
    # layers: Flooding NDVI  -----
    # if(input$indicator %in% c("Built areas without vegetation cover",
    #                           "Riparian zones without vegetation cover",
    #                           "Vulnerable steep slopes")){
    #   
    #   ndvi_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
    #                          "data/vegetation/sentinel-2/v_0/",
    #                          geo_name,
    #                          "-",
    #                          aoi_boundary_name,
    #                          "-vegetation-cover-2020-NDVItheshold0.4.tif",
    #                          sep = "")
    #   
    #   
    #   # collect raster data
    #   city_ndvi = raster(ndvi_data_path)
    # 
    #   city_ndvi_boundary = raster::mask(city_ndvi,
    #                                     boundary_aoi)
    #   city_ndvi_boundary[city_ndvi_boundary==0] = NA
    #   values(city_ndvi_boundary) = values(city_ndvi_boundary) *0.01
    #   
    #   # color
    #   ndvi_values = values(city_ndvi_boundary)[!is.na(values(city_ndvi_boundary))]
    #   
    #   pal_ndvi<- colorNumeric("Greens", 
    #                           ndvi_values,
    #                           na.color = "transparent",
    #                           reverse = FALSE)
    # 
    #   
    # }
    
    # layers: Flooding impervious surfaces  -----
    if(input$indicator == "Impervious surfaces"){
      
      impervious_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                                   "data/impervious/tsinghua/v_0/",
                                   geo_name,
                                   "-",
                                   aoi_boundary_name,
                                   "-impervious-areas-through-2018.tif",
                                   sep = "")
      
      # collect raster data
      city_impervious = raster(impervious_data_path)
      
      
      city_impervious_boundary = raster::mask(city_impervious,
                                              boundary_aoi)
      city_impervious_boundary[city_impervious_boundary==0] = NA
      
      # color  
      impervious_values = values(city_impervious_boundary)[!is.na(values(city_impervious_boundary))]
      
      pal_impervious <- colorNumeric("Greys", 
                                     impervious_values,
                                     na.color = "transparent",
                                     reverse = FALSE)
      
      
      
      
    }
    
    # layers: Riparian zones  -----
    if(input$indicator == "Riparian zones without vegetation cover"){
      
      riparian_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                                 "data/flooding/riparian/v_0/",
                                 geo_name,
                                 "-",
                                 aoi_boundary_name,
                                 "-RiparianBuffer.tif",
                                 sep = "")
      
      # collect raster data
      city_riparian = raster(riparian_data_path)
      
      
      city_riparian_boundary = raster::mask(city_riparian,
                                            boundary_aoi)
      
      city_riparian_boundary[city_riparian_boundary==0] = NA
      
      # color  
      riparian_values = values(city_riparian_boundary)[!is.na(values(city_riparian_boundary))]
      
      pal_riparian <- colorNumeric("YlOrBr", 
                                   riparian_values,
                                   na.color = "transparent",
                                   reverse = FALSE)
      
      
      
    }
    
    # layers: slopes  -----
    if(input$indicator == "Vulnerable steep slopes"){
      
      slopes_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                               "data/slope/nasa/v_0/",
                               geo_name,
                               "-",
                               aoi_boundary_name,
                               "-slopes-gte10degrees.tif",
                               sep = "")
      
      # collect raster data
      city_slopes = raster(slopes_data_path)
      
      
      city_slopes_boundary = raster::mask(city_slopes,
                                          boundary_aoi)
      city_slopes_boundary[city_slopes_boundary==0] = NA
      
      # color  
      slopes_values = values(city_slopes_boundary)[!is.na(values(city_slopes_boundary))]
      
      pal_slopes <- colorNumeric("Greys", 
                                 slopes_values,
                                 na.color = "transparent",
                                 reverse = TRUE)
      
      
      
    }
    
    # layers: lst  -----
    if(input$indicator == "High land surface temperature"){
      
      lst_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                            "data/land-surface-temperature/landsat/v_0/",
                            geo_name,
                            "-",
                            aoi_boundary_name,
                            "-land-surface-temperature-2013to2022meanofmonthwhottestday.tif",
                            sep = "")
      
      # collect raster data
      city_lst = raster(lst_data_path)
      
      
      city_lst_boundary = raster::mask(city_lst,
                                       boundary_aoi)
      city_lst_boundary[city_lst_boundary==0] = NA
      
      
      # color  
      lst_values = values(city_lst_boundary)[!is.na(values(city_lst_boundary))]
      
      pal_lst <- colorNumeric("RdYlBu", 
                              lst_values,
                              na.color = "transparent",
                              reverse = TRUE)
      
      
      
    }
    
    # layers: albedo  -----
    if(input$indicator == "Surface reflectivity"){
      
      albedo_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/data/albedo/sentinel-2/v_0/",
                               geo_name,
                               "-",
                               aoi_boundary_name,
                               "-S2-albedo-2021_50m.tif",
                               sep = "")
      
      
      
      # collect raster data
      city_albedo = raster(albedo_data_path)
      
      city_albedo_boundary = raster::mask(city_albedo,
                                          boundary_aoi)
      
      values(city_albedo_boundary) = values(city_albedo_boundary) *0.01
      
      # color albedo 
      albedo_values = values(city_albedo_boundary)[!is.na(values(city_albedo_boundary))]
      
      
      # define color for tree cover
      pal_albedo <- colorNumeric(palette = "Greys", 
                                 domain = albedo_values, 
                                 na.color = "transparent",
                                 reverse = TRUE)
      
      
      
    }
    
    # layers: pm 2.5  -----
    if(input$indicator == "Population exposure to PM 2.5"){
      
      pm25_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                             "data/air_pollution/acag/pm25/",
                             geo_name,
                             "-",
                             aoi_boundary_name,
                             "-ACAG-PM2.5-annual-2020.tif",
                             sep = "")
      
      # collect raster data
      city_pm25 = raster(pm25_data_path)
      
      
      city_pm25_boundary = raster::mask(city_pm25,
                                        boundary_aoi)
      
      # color  
      pm25_values = values(city_pm25_boundary)[!is.na(values(city_pm25_boundary))]
      
      pal_pm25 <- colorNumeric("YlOrRd", 
                               pm25_values,
                               na.color = "transparent",
                               reverse = FALSE)
      
      
      
    }
    
    # layers: carbon flux  -----
    if(input$indicator == "Average annual carbon flux from trees"){
      
      carbonflux_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                                   "data/tree_cover/wri-forest-carbon-fluxes/v_0/",
                                   geo_name,
                                   "-",
                                   aoi_boundary_name,
                                   "-WRI-ForestCarbonFluxes-MgCO2eperHA2001-2021.tif",
                                   sep = "")
      
      # collect raster data
      city_carbonflux = raster(carbonflux_data_path)
      
      
      city_carbonflux_boundary = raster::mask(city_carbonflux,
                                              boundary_aoi)
      
      # color  
      carbonflux_values = values(city_carbonflux_boundary)[!is.na(values(city_carbonflux_boundary))]
      
      pal_carbonflux <- colorNumeric("PRGn", 
                                     carbonflux_values,
                                     na.color = "transparent",
                                     reverse = TRUE)
      
      
      
    }
    
    ########################
    # map indicator ----
    ########################
    
    # main map -----
    m = leaflet(boundary_aoi) %>%
      # addTiles() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addScaleBar() %>%
      fitBounds(~as.numeric(st_bbox(boundary_aoi)[1]),
                ~as.numeric(st_bbox(boundary_aoi)[2]),
                ~as.numeric(st_bbox(boundary_aoi)[3]),
                ~as.numeric(st_bbox(boundary_aoi)[4])) %>% 
      # boundaries
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
      # Map indicator 
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
                          selected_indicator_label),
        options = layersControlOptions(collapsed = FALSE)
      ) 
    
    # GRE-1-2: High land surface temperature ----
    if(input$indicator == "High land surface temperature"){
      m = m %>% 
        # plot layer: ESA world cover ----
      addRasterImage(city_esa_worldcover,
                     colors = pal_worldcover,
                     opacity = 1,
                     maxBytes = 100 * 1024 * 1024,
                     project=FALSE,
                     group = "Land cover types",
                     layerId = "Land cover types") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "World Cover",
                  group = "Land cover types",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Layer: LST
        addRasterImage(city_lst_boundary,
                       colors = pal_lst,
                       opacity = 0.9,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Land surface temperature",
                       layerId = "Land surface temperature") %>%
        addLegend(pal = pal_lst,
                  values = lst_values,
                  title = "Land surface temperature <br> (C, daytime hot season mean)",
                  group = "Land surface temperature",
                  position = "bottomleft") %>% 
        # Layers control
        addLayersControl(
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Land surface temperature",
                            "Land cover types"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(c("Land cover types",
                    "Land surface temperature")) 
    }
    
    # GRE-1-3: Surface reflectivity ----
    if(input$indicator == "Surface reflectivity"){
      m = m %>% 
        # plot layer: Albedo ----
      addRasterImage(city_albedo_boundary,
                     colors = pal_albedo ,
                     opacity = 0.9,
                     group = "Surface albedo",
                     project=FALSE,
                     maxBytes = 8 * 1024 * 1024) %>%
        # Legend for population
        addLegend(pal = pal_albedo ,
                  values = albedo_values,
                  opacity = 0.9,
                  title = "Surface albedo",
                  group = "Surface albedo",
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
                            "Surface albedo",
                            "Land cover types"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(c("Surface albedo",
                    "Land cover types")) 
    }
    
    # GRE-3-1: Open space for public use - Add layers ----
    if(input$indicator == "Open space for public use" & !input$city %in% c("MEX-Mexico_City","MEX-Monterrey") ){
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
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Open Space Areas",
                            "Land cover types"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Open Space Areas",
                    "Land cover types")) 
    }
    
    # GRE-3-2: Access to public open space - Add layers ----
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
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_label,
                          "Open Space Areas",
                          "Population",
                          "Population - open space"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Population",
                    "Open Space Areas",
                    "Population - open space")) 
    }
    
    # GRE-3-3: Access to tree cover - Add layers ----
    if(input$indicator == "Access to tree cover" & !input$city %in% c("BRA-Salvador",
                                                                      "MEX-Monterrey")){
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
        addRasterImage(city_tml_boundary, 
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
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Population",
                            "Population - Tree cover",
                            "Tree cover"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Population",
                    "Population - Tree cover",
                    "Tree cover")) 
    } 
    
    # GRE-1-4: Built land without tree cover ----
    if(input$indicator == "Built land without tree cover"){
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
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Land cover types",
                            "Tree cover"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Tree cover",
                    "Land cover types")) 
    }
    
    # GRE-2-3: Population exposure to PM 2.5 ----
    if(input$indicator == "Population exposure to PM 2.5"){
      m = m %>% 
        addRasterImage(city_pm25_boundary,
                       colors = pal_pm25 ,
                       opacity = 0.9,
                       group = "PM2.5 concentration",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "PM2.5 concentration") %>%
        # Legend for population
        addLegend(pal = pal_pm25 ,
                  values = pm25_values,
                  opacity = 0.9,
                  title = "PM2.5 annual average <br> concentration (µg/m3)",
                  group = "PM2.5 concentration",
                  position = "bottomleft") %>% 
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
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_label,
                          "PM2.5 concentration",
                          "Population"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("PM2.5 concentration",
                    "Population")) 
    }
    
    # GRE-4-1: Exposure to coastal and river flooding -----
    if(input$indicator == "Exposure to coastal and river flooding"){
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
        # Raster of flooding --
        addRasterImage(city_flood,
                       colors = pal_flood,
                       opacity = 1,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Coastal/riverine flooding") %>%
        addLegend(pal = pal_flood,
                  values = flood_values, 
                  title = "Inundation depth (decimeters)",
                  group = "Coastal/riverine flooding",
                  position = "bottomleft") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Land cover types",
                            "Coastal/riverine flooding"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Coastal/riverine flooding",
                    "Land cover types")) 
    }
    
    # GRE-4-3: Land near natural drainage ----
    if(input$indicator == "Land near natural drainage"){
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
        # plot layer:HAND ----
      addRasterImage(city_flood_hand_boundary,
                     colors = "blue" ,
                     opacity = 0.9,
                     group = "Land within 1m height above nearest drainage",
                     project=FALSE,
                     maxBytes = 8 * 1024 * 1024) %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Land cover types",
                            "Land within 1m height above nearest drainage"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Land cover types",
                    "Land within 1m height above nearest drainage")) 
    }
    
    # GRE 4-4: 	Impervious surfaces ----
    if(input$indicator == "Impervious surfaces"){
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
        # plot layer:Impervious surfaces ----
      addRasterImage(city_impervious_boundary,
                     colors = pal_impervious,
                     opacity = 0.7,
                     maxBytes = 20 * 1024 * 1024,
                     project=FALSE,
                     group = "Impervious areas") %>%
        addLegend(pal = pal_impervious,
                  values = impervious_values,
                  title = "Impervious areas (%)",
                  group = "Impervious areas",
                  position = "bottomleft") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Land cover types",
                            "Impervious areas"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup(c("Land cover types",
                    "Impervious areas"))
    }
    
    # GRE 4-5: Built areas without vegetation cover ----
    if(input$indicator == "Built areas without vegetation cover"){
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
        #   # plot layer:NDVI ----
      # addRasterImage(city_ndvi_boundary,
      #                colors = pal_ndvi,
      #                opacity = 0.9,
      #                maxBytes = 20 * 1024 * 1024,
      #                project=FALSE,
      #                group = "Vegetation cover") %>%
      #   addLegend(pal = pal_ndvi,
      #             values = ndvi_values,
      #             title = "Vegetation cover",
      #             group = "Vegetation cover",
      #             position = "bottomleft") %>% 
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_label,
                          "Land cover types"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
        hideGroup(c("Land cover types"))
    }
    
    # GRE 4-6: Riparian zones without vegetation cover ----
    if(input$indicator == "Riparian zones without vegetation cover"){
      m = m %>%
        # Layer: riparian
        addRasterImage(city_riparian_boundary,
                       colors = pal_riparian,
                       opacity = 0.7,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Riparian buffer areas") %>%
        addLegend(pal = pal_riparian,
                  values = riparian_values,
                  title = "Riparian buffer areas <br> (meter from water)",
                  group = "Riparian buffer areas",
                  position = "bottomleft") %>% 
        #   # plot layer:NDVI ----
      # addRasterImage(city_ndvi_boundary,
      #                colors = pal_ndvi,
      #                opacity = 0.9,
      #                maxBytes = 20 * 1024 * 1024,
      #                project=FALSE,
      #                group = "Vegetation cover") %>%
      #   addLegend(pal = pal_ndvi,
      #             values = ndvi_values,
      #             title = "Vegetation cover",
      #             group = "Vegetation cover",
      #             position = "bottomleft") %>% 
      # Layers control
      addLayersControl(
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_label,
                          "Riparian buffer areas"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
        hideGroup(c("Riparian buffer areas"))
    }
    
    # GRE 4-7: Vulnerable steep slopes ----
    if(input$indicator == "Vulnerable steep slopes"){
      m = m %>%
        # Layer: slopes
        addRasterImage(city_slopes_boundary,
                       colors = pal_slopes,
                       opacity = 0.9,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Hillside slopes") %>%
        addLegend(pal = pal_slopes,
                  values = slopes_values,
                  title = "Hillside slopes ((degrees)",
                  group = "Hillside slopes",
                  position = "bottomleft") %>% 
        #   # plot layer:NDVI ----
      # addRasterImage(city_ndvi_boundary,
      #                colors = pal_ndvi,
      #                opacity = 0.9,
      #                maxBytes = 20 * 1024 * 1024,
      #                project=FALSE,
      #                group = "Vegetation cover") %>%
      #   addLegend(pal = pal_ndvi,
      #             values = ndvi_values,
      #             title = "Vegetation cover",
      #             group = "Vegetation cover",
      #             position = "bottomleft") %>% 
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_label,
                          "Hillside slopes"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
        hideGroup(c("Hillside slopes"))
    }
    
    # GRE 5-2: Average annual carbon flux from trees ----
    if(input$indicator == "Average annual carbon flux from trees"){
      m = m %>%
        # plot layer: carbon flux
        addRasterImage(city_carbonflux_boundary,
                       colors = pal_carbonflux ,
                       opacity = 0.9,
                       group = "Carbon flux from trees <br> (net, Mt CO2e/ha, 2001 to 2021)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Carbon flux from trees") %>%
        # Legend for population
        addLegend(pal = pal_carbonflux ,
                  values = carbonflux_values,
                  opacity = 0.9,
                  title = "Carbon flux from trees <br> (net, Mt CO2e/ha, 2001 to 2021)",
                  group = "Carbon flux from trees <br> (net, Mt CO2e/ha, 2001 to 2021)",
                  position = "bottomleft") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_label,
                            "Carbon flux from trees <br> (net, Mt CO2e/ha, 2001 to 2021)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup(c("Carbon flux from trees <br> (net, Mt CO2e/ha, 2001 to 2021)"))
    }
    
    # center map  
    output$indicator_map <- renderLeaflet({
      m 
      
      # addOpacityControls(collapsed = TRUE,
      #                    category = c("image"),
      #                    size = "s",
      #                    position = "bottomright")
    })
    
    #########################################
    ### Main indicators ----
    
    # city wide  ----
    city_wide_indicator_value = aoi_indicators %>%
      as.data.frame() %>%
      pull(selected_indicator_name) %>% 
      round(2)
    
    
    # unit
    city_wide_indicator_value_unit = "%"
    
    if(input$indicator %in% c('Nb days air pollution (carbon monoxide)',
                              "High pollution days")){
      city_wide_indicator_value_unit = "days"
    } else if(input$indicator %in% c("Greenhouse gas emissions")){
      city_wide_indicator_value_unit = "Million Tonnes CO2 eq"
      city_wide_indicator_value = round(city_wide_indicator_value/1000000,2)
    } else if(input$indicator %in% c("Air pollutant emissions")){
      city_wide_indicator_value_unit = "Tonnes"
    } else if(input$indicator %in% c("Average annual carbon flux from trees")){
      city_wide_indicator_value_unit = "Mt CO2 eq/ha"
    } 
    
    
    # output plot
    output$city_wide_indicator <- renderText({
      paste("<center>","<font size=5px; weight=500; color=\"#168A06\"><b>", 
            city_wide_indicator_value, 
            city_wide_indicator_value_unit)
    })
    
    #########################################
    ### Table ----
    
    
    # Table plot
    
    if(input$indicator == "High pollution days"){
      table_plot = aoi_indicators %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        dplyr::select("City" = geo_name,
                      "Nitrogen dioxide"= GRE_2_2_2_exceedancedays_nitrogen.dioxide,
                      "Sulfur dioxide" = GRE_2_2_2_exceedancedays_sulfur.dioxide,
                      "Ozone" = GRE_2_2_2_exceedancedays_ozone,
                      "Fine particulate matter" = GRE_2_2_2_exceedancedays_fine.particulate.matter,
                      "Coarse particulate matter" = GRE_2_2_2_exceedancedays_coarse.particulate.matter,
                      "Carbon monoxide" = GRE_2_2_2_exceedancedays_carbon.monoxide) %>% 
        mutate_if(is.numeric, round, 2) 
    } else if(input$indicator %in%  c("Air pollution (by pollutant)",
                                      "Air pollution (by sector)")){
      table_plot = aoi_indicators %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        pivot_longer(
          cols = bc_agl_2010:nmvoc_tro_2020,
          names_to = c("gas", "sector", "year"),
          names_pattern = "(.*)_(.*)_(.*)",
          values_to = "value") %>% 
        dplyr::select(geo_name,
                      gas,
                      sector, 
                      year, 
                      value) %>% 
        filter(year == 2020,
               sector != 'sum') %>% 
        mutate_at("gas", 
                  ~recode(.,
                          "bc"='Black carbon', 
                          'ch4'='Methane',
                          'co' = 'Carbon monoxide',
                          'co2' = 'Carbon dioxide',
                          'nox' = 'Nitrogen oxides',
                          'so2' = 'Sulfur dioxide',
                          'oc' = 'Organic carbon',
                          'nh3' = 'Ammonia',
                          'nmvoc' = 'Non-methane volatile organic compounds')) %>% 
        mutate_at("sector", 
                  ~recode(.,
                          "agl"='Agriculture livestock', 
                          'ags'='Agriculture soils',
                          'awb' = 'Agriculture waste burning',
                          'ene' = 'Power generation',
                          'fef' = 'Fugitives',
                          'ind' = 'Industry',
                          'res' = 'Residential, commercial, and other combustion',
                          'shp' = 'Ships',
                          'slv' = 'Solvents',
                          'sum' = 'All sources',
                          'swd' = 'Solid waste and wastewater',
                          'tnr' = 'Off-road transportation',
                          'tro' = 'Road transportation'))
    } else if(input$indicator %in%  c("Air pollutant emissions")){
      table_plot = aoi_indicators %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        pivot_longer(
          cols = bc_agl_2020:nmvoc_tro_2020,
          names_to = c("gas", "sector","year"),
          names_pattern = "(.*)_(.*)_(.*)",
          values_to = "value") %>% 
        dplyr::select(geo_name,
                      gas,
                      sector, 
                      value) %>% 
        arrange(desc(value)) %>% 
        mutate_at("gas", 
                  ~recode(.,
                          "bc"='Black carbon', 
                          'ch4'='Methane',
                          'co' = 'Carbon monoxide',
                          'co2' = 'Carbon dioxide',
                          'nox' = 'Nitrogen oxides',
                          'so2' = 'Sulfur dioxide',
                          'oc' = 'Organic carbon',
                          'nh3' = 'Ammonia',
                          'nmvoc' = 'Non-methane volatile organic compounds')) %>% 
        mutate_at("sector", 
                  ~recode(.,
                          "agl"='Agriculture livestock', 
                          'ags'='Agriculture soils',
                          'awb' = 'Agriculture waste burning',
                          'ene' = 'Power generation',
                          'fef' = 'Fugitives',
                          'ind' = 'Industry',
                          'res' = 'Residential, commercial, and other combustion',
                          'shp' = 'Ships',
                          'slv' = 'Solvents',
                          'sum' = 'All sources',
                          'swd' = 'Solid waste and wastewater',
                          'tnr' = 'Off-road transportation',
                          'tro' = 'Road transportation'))
    } else if(input$indicator %in%  c("Greenhouse gas emissions")){
      table_plot = aoi_indicators %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        pivot_longer(
          cols = bc_agl:nmvoc_tro,
          names_to = c("gas", "sector"),
          names_pattern = "(.*)_(.*)",
          values_to = "value") %>% 
        dplyr::select(geo_name,
                      gas,
                      sector, 
                      value) %>% 
        arrange(desc(value)) %>% 
        mutate_at("gas", 
                  ~recode(.,
                          "bc"='Black carbon', 
                          'ch4'='Methane',
                          'co' = 'Carbon monoxide',
                          'co2' = 'Carbon dioxide',
                          'nox' = 'Nitrogen oxides',
                          'so2' = 'Sulfur dioxide',
                          'oc' = 'Organic carbon',
                          'nh3' = 'Ammonia',
                          'nmvoc' = 'Non-methane volatile organic compounds')) %>% 
        mutate_at("sector", 
                  ~recode(.,
                          "agl"='Agriculture livestock', 
                          'ags'='Agriculture soils',
                          'awb' = 'Agriculture waste burning',
                          'ene' = 'Power generation',
                          'fef' = 'Fugitives',
                          'ind' = 'Industry',
                          'res' = 'Residential, commercial, and other combustion',
                          'shp' = 'Ships',
                          'slv' = 'Solvents',
                          'sum' = 'All sources',
                          'swd' = 'Solid waste and wastewater',
                          'tnr' = 'Off-road transportation',
                          'tro' = 'Road transportation'))
    } else{
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
      
      # keep distinct city_name
      table_plot = table_plot %>% 
        distinct(`City name`, .keep_all = TRUE)
    }
    
    
    
    output$indicator_table <- DT::renderDataTable(
      
      if(input$indicator %in% c("High pollution days",
                                "Greenhouse gas emissions",
                                "Air pollutant emissions")){
        DT::datatable(table_plot,
                      options = list(pageLength = 10,order = list(list(2, 'desc')))) 
      } else {
        DT::datatable(table_plot,
                      options = list(pageLength = 10,order = list(list(2, 'desc')))) %>% 
          formatStyle(selected_indicator_label,
                      backgroundColor = styleInterval(seq(from = min(table_plot[,selected_indicator_label]),
                                                          to = max(table_plot[,selected_indicator_label]),
                                                          length.out = 8),
                                                      brewer.pal(9, table_color)
                      ),
                      fontWeight = 'bold')
      }
      
    )
    
    
    
    
    
    #########################################
    # DOwnload tabular table ----
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
    
    
    
    if(input$indicator == "High pollution days"){
      fig = plot_ly(x = c("Nitrogen dioxide",
                          "Sulfur dioxide",
                          "Ozone",
                          "Fine particulate matter",
                          "Coarse particulate matter",
                          "Carbon monoxide"),
                    y = as.numeric(table_plot[1, c(2,3,4,5,6,7)]),
                    type = "bar",
                    orientation = "v",
                    # name = names(table_plot)[2],
                    color = I("green4")) %>% 
        layout(yaxis = list(title = "Annual number of days air pollutants <br> are above WHO air quality standards"),
               xaxis = list(title = 'Air pollutants',categoryorder = "total descending"))
      
      
    } else if(input$indicator == "Air pollution (by pollutant)"){
      fig <- plot_ly(table_plot, 
                     labels = ~gas, 
                     values = ~value, 
                     type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent')
      
      fig <- fig %>% layout(title = 'Air pollution by pollutant (2020)',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = "h",   # show entries horizontally
                                          xanchor = "center",  # use center of legend as anchor
                                          x = 0.5)
      )
      
      
    } else if(input$indicator == "Air pollution (by sector)"){
      fig <- plot_ly(table_plot, 
                     labels = ~sector, 
                     values = ~value, 
                     type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent')
      
      fig <- fig %>% layout(title = 'Sectors contribution to air pollution (2020)',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            legend = list(orientation = "h",
                                          xanchor = "center",  # use center of legend as anchor
                                          x = 0.5))
      
      
    } else if(input$indicator == "Air pollutant emissions"){
      fig <- plot_ly(table_plot, 
                     x = ~sector, 
                     y = ~value, 
                     type = 'bar',
                     name = ~gas,
                     color = ~gas) %>% 
        layout(yaxis = list(title = 'Air pollutant emissions (Tonnes)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      
    } else if(input$indicator == "Greenhouse gas emissions"){
      fig <- plot_ly(table_plot, 
                     x = ~sector, 
                     y = ~value, 
                     type = 'bar',
                     name = ~gas,
                     color = ~gas) %>% 
        layout(yaxis = list(title = 'Emissions (CO2 eq)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      
    } else {
      fig = plot_ly(x = table_plot$`City name`,
                    y = table_plot[[colnames(table_plot)[2]]],
                    # y = selected_indicator_legend, 
                    type = "bar",
                    orientation = "v",
                    name = names(table_plot)[2],
                    color = I("green4")) %>% 
        layout(yaxis = list(title = selected_indicator_legend ), #names(table_plot)[2]),
               xaxis = list(title = 'Cities',categoryorder = "total descending"))
      
    }
    
    output$indicator_chart <- renderPlotly({
      fig
      
    })
    
    #########################################
    ### Cities comparison  ----
    
    # cities comparison ----
    
    indicators_comparison = indicators_comparison %>% 
      as.data.frame() %>%
      dplyr::select(geo_name,selected_indicator_name) %>% 
      drop_na(selected_indicator_name, geo_name) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      arrange(desc(selected_indicator_name)) 
    
    # change names
    names(indicators_comparison) = c("City name",selected_indicator_label)
    
    city_num = which(indicators_comparison$`City name` == geo_name)
    city_color = rep("grey",nrow(indicators_comparison))
    city_color[city_num] = "green"
    
    
    
    cities_indicator_avg = round(mean(indicators_comparison[[colnames(indicators_comparison)[2]]]),2)
    
    # if(input$indicator == "Greenhouse gas emissions"){
    #   cities_indicator_avg = round(cities_indicator_avg /1000000, 2)
    # }
    
    output$cities_comparison_plot <- renderPlotly({
      fig = plot_ly(x = indicators_comparison$`City name`,
                    y = indicators_comparison[[colnames(indicators_comparison)[2]]],
                    type = "bar",
                    orientation = "v",
                    name = names(indicators_comparison)[2],
                    marker = list(color = city_color)) %>% 
        layout(yaxis = list(title = selected_indicator_legend), #names(indicators_comparison)[2]
               xaxis = list(title = 'Cities',categoryorder = "total descending"),
               annotations = list(
                 x = 10,
                 y = cities_indicator_avg+cities_indicator_avg*0.1, 
                 text = paste("Cities' averrage: ", cities_indicator_avg, city_wide_indicator_value_unit, sep = ""),
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
      filter(indicator_label == input$indicator) %>% 
      pull(indicator_definition)
    
    indicator_data_sources = indicators_definitions %>% 
      filter(indicator_label == input$indicator) %>%  
      pull(data_sources)
    
    indicator_importance = indicators_definitions %>% 
      filter(indicator_label == input$indicator) %>%  
      pull(importance)
    
    indicator_methods = indicators_definitions %>% 
      filter(indicator_label == input$indicator) %>%  
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
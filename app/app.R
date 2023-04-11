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


#library(shinycssloaders)

# library(leaflet.multiopacity)

#library(shinyscreenshot)
# library(webshot)
# library(mapview)

# define project

# selected_project = "urbanshift"
selected_project = "cities4forests"

if(selected_project == "urbanshift"){
  default_city = "BRA-Belem"
  logo_file = "logo_urbanshift.png"
  logo_height = "30px"
  default_theme = "Biodiversity"
  default_indicator = "Natural Areas"
} else if(selected_project == "cities4forests"){
  default_city = "COG-Brazzaville"
  logo_file = "logo.png"
  logo_height = "15px"
  default_theme = "Greenspace access"
  default_indicator = "Open space for public use"
}




# define aws s3 path

aws_s3_path = "https://cities-indicators.s3.eu-west-3.amazonaws.com/"


############### Load data: indicator definition

# read indicator definition all projects------------

indicators_definitions = read.csv(paste(aws_s3_path,
                                        "indicators/definitions.csv",
                                        sep = ""))

# read indicator definition ------------

# indicators_definitions = read.csv(paste(aws_s3_path,
#                                         "indicators/",
#                                         selected_project,
#                                         "/definitions.csv",
#                                         sep = ""))

# remove wrong labels still in the table
indicators_definitions = indicators_definitions %>% 
  filter(!indicator_label %in% c("High pollution days (carbon monoxide)",
                                 "Air pollution (by pollutant)",
                                 "Air pollution (by sector)"))

indicators_definitions = indicators_definitions %>% 
  add_row(X = nrow(indicators_definitions)+1, 
          theme = "Land protection and restoration",
          indicator_label = "Change in vegetation and water cover",
          project_name = "urbanshift",
          indicator_code = "LND-3",
          indicator_name = "LND_3_percentChangeinVegetation.WaterCover2019.2022",
          indicator_legend = "Change in vegetation and water cover (%)",
          indicator_definition = "Net increase/decrease in area of vegetation and water cover between 2016 and 2021 as percent of 2016 area with vegetation and water cover",
          data_sources = "10-meter Sentinel-2",
          importance = "Vegetation and water cover provide many ecosystem services to cities, including groundwater accumulation, flood management and temperature moderation. They are also critical prerequisites to habitat.",
          methods = "This indicator uses a trendline of the measurements of spectral indices from 2016-2021 to estimate the change in the presence of vegetation and water at points within each city. We use 10-meter Sentinel-2 data, as accessed on Earth Engine and cloudmasked, to calculate spectral indices associated with vegetation (Normalized Difference Vegetation Index, NDVI) and water (Normalized Difference Water Index, NDWI) and then create annual greenest/bluest pixel mosaics for each year. We calculate trendlines of greenness and blueness for each pixel over the six year period and flag pixels with an average annual change (slope) of at least (-)0.03 in its index value. We then mask these trend layers by layers with all pixels that meet the threshold for vegetation (NDVI of at least 0.4) or water (NDWI of at least 0.3) in at least one year so as to exclude pixels that are not likely vegetation or water in any year. Next we separate these change layers into gain and loss layers, combine the separate vegetation and water layers to produce one gain and one loss layer, and apply 30-meter resolution reductions to count the number of pixels of loss and gain for either vegetation or water and in each city and sub-city area. We subtract the count of loss pixels from the count of gain pixels to produce a count of net change in vegetation and water pixels. To normalize this value so as to represent a percent increase or decrease in vegetation and water area, we divide it by the total number of 30-meter pixels that met the index value threshold for vegetation or water in 2016 for the same area of interest."
  )

# add new indicator names
indicators_definitions = indicators_definitions %>% 
  filter(project_name == selected_project) %>% 
  mutate(indicator_name_generic = recode(indicator_name,
                                         # HEA
                                         "GRE_1_1_percentChangeinDaysAbove35C2020to2050" = "HEA_1_percentChangeinDaysAbove35C2020to2050",
                                         "GRE_1_2_percentBuiltupwHighLST.2013to2022meanofmonthwhottestday" = "HEA_2_percentBuiltupwHighLST.2013to2022meanofmonthwhottestday",
                                         "GRE_1_3_percentBuiltwLowAlbedo" = "HEA_3_percentBuiltwLowAlbedo",
                                         "GRE_1_4_percentBuiltupWithoutTreeCover" = "HEA_4_percentBuiltupWithoutTreeCover",
                                         "GRE_1_4_percent_builtup_areas_without_tree_cover" = "HEA_4_percentBuiltupWithoutTreeCover",
                                         
                                         # AQ
                                         "percent_of_WHO_2020" = "AQ_3_percentWHOlimitPM25exposure2020",
                                         "GRE_2_3_population_exposure_pm25" = "AQ_3_percentWHOlimitPM25exposure2020",
                                         
                                         # ACC
                                         "GRE.1" = "ACC_1_OpenSpaceHectaresper1000people2022",
                                         
                                         "GRE.2" = "ACC_2_percentOpenSpaceinBuiltup2022", 
                                         "GRE_3_1_percentOpenSpaceinBuiltup" = "ACC_2_percentOpenSpaceinBuiltup2022",
                                         
                                         "GRE.3" = "ACC_3_percentPopwOpenSpaceAccess2022",
                                         "GRE_3_2_percentPopwOpenSpaceAccess" = "ACC_3_percentPopwOpenSpaceAccess2022",
                                         
                                         "GRE.4" = "ACC_4_percentPopwTreeCoverAccess2022",
                                         "GRE_3_3_percentPopwTreeCoverAccess" = "ACC_4_percentPopwTreeCoverAccess2022",
                                         
                                         # FLD
                                         "GRE_4_1_percentFloodProneinBuiltup2050" = "FLD_1_percentFloodProneinBuiltup2050",
                                         "GRE_4_2_percentChangeinMaxDailyPrecip2020to2050" = "FLD_2_percentChangeinMaxDailyPrecip2020to2050",
                                         "GRE_4_3_percentBuiltupWithin1mAboveDrainage" = "FLD_3_percentBuiltupWithin1mAboveDrainage",
                                         "GRE_4_4_percentImperviousinBuiltup2018" = "FLD_4_percentImperviousinBuiltup2018",
                                         "GRE_4_5_percentBuiltupWOvegetationcover2020" = "FLD_5_percentBuiltupWOvegetationcover2020",
                                         "GRE_4_6_percentRiparianZonewoVegorWatercover2020" = "FLD_6_percentRiparianZonewoVegorWatercover2020",
                                         "GRE_4_7_percentSteepSlopesWOvegetationcover2020" = "FLD_7_percentSteepSlopesWOvegetationcover2020",
                                         
                                         # GHG
                                         "GRE_5_2_meanannualTreeCarbonFluxMgcO2eperHA" = "GHG_2_meanannualTreeCarbonFluxMgcO2eperHA",
                                         "GRE.5.2" = "GHG_2_meanannualTreeCarbonFluxMgcO2eperHA",
                                         # "GHG_2_meanannualTreeCarbonFluxMgcO2eperHA" = "GHG_2_meanannualTreeCarbonFluxMgcO2eperHA",
                                         
                                         # BIO
                                         "BIO.1" = "BIO_1_percentNaturalArea",
                                         "BIO.2" = "BIO_2_habitat_connectivity",
                                         "BIO.3" = "BIO_3_percentBirdsinBuiltupAreas",
                                         "BIO.4" = "BIO_4_numberPlantSpecies",
                                         "BIO.5" = "BIO_5_numberBirdSpecies",
                                         "BIO.6" = "BIO_6_numberArthropodSpecies",
                                         
                                         # LND
                                         "LND.1" = "LND_1_percentPermeableSurface",
                                         "LND.2" = "LND_2_percentTreeCover",
                                         "LND.4" = "LND_4_percentof2000HabitatAreaRestoredby2020",
                                         "LND.5" = "LND_5_numberofHabitatTypesRestoredby2020",
                                         "LND.6" = "LND_6_percentProtectedArea",
                                         "LND.7" = "LND_7_percentKBAsProtected",
                                         "LND.8" = "LND_8_percentKBAsBuiltup"
  )
  )


# get list of themes
indicators_themes = unique(indicators_definitions$theme)

# get list of indicators

indicators_list = unique(indicators_definitions$indicator_label)

############### Load data: boundary georef

# all projects
boundary_georef = read.csv(paste(aws_s3_path,
                                 "data/boundaries/boundary_georef.csv",
                                 sep = ""),
                           fileEncoding="UTF-8-BOM")

boundary_georef = boundary_georef %>% 
  filter(project_name == selected_project) 


# # by project
# boundary_georef = read.csv(paste(aws_s3_path,
#                                  "data/",
#                                  selected_project,
#                                  "/boundaries/boundary_georef.csv",
#                                  sep = ""),
#                            fileEncoding="UTF-8-BOM")

cities = boundary_georef %>% 
  # mutate(geo_name = recode(geo_name,
  #                          "SLE-Freetown" = "SLE-Freetown_city")) %>% 
  pull(geo_name) 




# list cities without available tml data and related indicators
cities_no_tml_data = c("BRA-Salvador","MEX-Monterrey",
                       "ARG-Mendoza", "ARG-Mer_del_plata", "ARG-Ushiaia", 
                       "ARG-Buenos_Aires", "BRA-Florianopolis", "CHN-Chengdu", 
                       "CHN-Chongqing", "CHN-Ningbo")

############### Load data: indicators

# read indicator ------------

# project based indicators
# indicators = read.csv(paste(aws_s3_path,
#                             "indicators/",
#                             selected_project,
#                             "/indicators.csv",
#                             sep = ""))

# generic indicator
indicators = read.csv(paste(aws_s3_path,
                            "indicators/indicators.csv",
                            sep = ""))

# read AQ1 indicator -----

data_path = "https://cities-indicators.s3.eu-west-3.amazonaws.com/indicators/AQ-1-emissionschange2000to2020.csv"


indicators_AQ1 = read.csv(data_path)

geo_name_c4f = boundary_georef %>% 
  filter(project_name == selected_project) %>% 
  pull(geo_name)

city_id_c4f = boundary_georef %>% 
  filter(project_name == selected_project) %>% 
  pull(city_id)

indicators_AQ1_aoi = indicators_AQ1 %>%
  rename_at(1,~"city_id") %>% 
  filter(city_id %in% city_id_c4f) %>% 
  add_column(geo_parent_name = geo_name_c4f) %>%
  mutate(geo_parent_name = recode(geo_parent_name,
                                  "SLE-Freetown" = "SLE-Freetown_city")) %>% 
  dplyr::select(geo_parent_name,
                GRE_2_1_air_pollution =  total_change_2020) %>%
  mutate(GRE_2_1_air_pollution = 100 * GRE_2_1_air_pollution)

indicators = indicators %>% 
  right_join(indicators_AQ1_aoi,
             by = "geo_parent_name") 


indicators_AQ1_unit = indicators_AQ1 %>% 
  dplyr::select(c(1, 194:385)) %>% 
  rename(geo_id = "Unnamed..0")

indicators = indicators %>% 
  left_join(indicators_AQ1_unit,
            by = "geo_id")

# read AQ2 indicator -----

data_path = "https://cities-indicators.s3.eu-west-3.amazonaws.com/indicators/AQ-2-highpollutiondays.csv"


indicators_AQ2 = read.csv(data_path)

indicators_AQ2 = indicators_AQ2 %>% 
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
  right_join(indicators_AQ2,
             by = "geo_parent_name")

# read GHG1 -----

data_path = "https://cities-indicators.s3.eu-west-3.amazonaws.com/indicators/GHG-1-ghg-emissions.csv"


indicators_GHG1 = read.csv(data_path)

indicators_GHG1$total_change = (indicators_GHG1$total_2020_co2e - indicators_GHG1$total_2000_co2e)/indicators_GHG1$total_2000_co2e * 100

indicators_GHG1_aoi = indicators_GHG1 %>%
  rename_at(1,~"city_id") %>% 
  filter(city_id %in% city_id_c4f) %>% 
  add_column(geo_parent_name = geo_name_c4f) %>%
  dplyr::select(geo_parent_name,
                GRE_5_1_ghg_emissions =  total_change) %>%
  mutate_if(is.numeric, round, 0)

indicators = indicators %>% 
  right_join(indicators_GHG1_aoi,
             by = "geo_parent_name") 


indicators_GHG1_unit = indicators_GHG1 %>% 
  dplyr::select(1,
                c(194:284),
                c(319:408)) %>% 
  dplyr::select(-c("bc_2000_co2e","co_2000_co2e","ch4_2000_co2e","co2_2000_co2e",
                   "oc_2000_co2e", "nmvoc_2000_co2e", "ch4_2020_co2e","co_2020_co2e" ,
                   "oc_2020_co2e" , "nmvoc_2020_co2e","co2_2020_co2e" ,"nox_2020_co2e","nox_2000_co2e")) %>% 
  rename(geo_id = "Unnamed..0")

indicators = indicators %>% 
  left_join(indicators_GHG1_unit,
            by = "geo_id")


# process indicators indicator ------------

indicators = indicators %>%
  # HEA
  mutate(HEA_1_percentChangeinDaysAbove35C2020to2050 = 100 * HEA_1_percentChangeinDaysAbove35C2020to2050) %>%
  mutate(HEA_2_percentBuiltupwHighLST.2013to2022meanofmonthwhottestday = 100 * HEA_2_percentBuiltupwHighLST.2013to2022meanofmonthwhottestday) %>%
  mutate(HEA_3_percentBuiltwLowAlbedo = 100 * HEA_3_percentBuiltwLowAlbedo) %>%
  mutate(HEA_4_percentBuiltupWithoutTreeCover = 100 * HEA_4_percentBuiltupWithoutTreeCover) %>%
  # BIO
  mutate(BIO_1_percentNaturalArea = 100 * BIO_1_percentNaturalArea) %>% 
  mutate(BIO_2_habitat_connectivity = 1 * BIO_2_habitat_connectivity) %>% 
  mutate(BIO_3_percentBirdsinBuiltupAreas = na_if(BIO_3_percentBirdsinBuiltupAreas, -9999)) %>% 
  mutate(BIO_4_numberPlantSpecies = na_if(BIO_4_numberPlantSpecies, -9999)) %>% 
  mutate(BIO_5_numberBirdSpecies = na_if(BIO_5_numberBirdSpecies, -9999)) %>% 
  mutate(BIO_6_numberArthropodSpecies = na_if(BIO_6_numberArthropodSpecies, -9999)) %>% 
  # LND
  mutate(LND_1_percentPermeableSurface = na_if(LND_1_percentPermeableSurface, -9999)) %>% 
  mutate(LND_1_percentPermeableSurface = 100 * LND_1_percentPermeableSurface) %>% 
  mutate(LND_2_percentTreeCover = 100 * LND_2_percentTreeCover) %>% 
  mutate(LND_3_percentChangeinVegetation.WaterCover2019.2022 = 100 * LND_3_percentChangeinVegetation.WaterCover2019.2022) %>% 
  mutate(LND_4_percentof2000HabitatAreaRestoredby2020 = 100 * LND_4_percentof2000HabitatAreaRestoredby2020) %>% 
  mutate(LND_5_numberofHabitatTypesRestoredby2020 = 100 * LND_5_numberofHabitatTypesRestoredby2020) %>% 
  mutate(LND_6_percentProtectedArea = 100 * LND_6_percentProtectedArea) %>% 
  mutate(LND_7_percentKBAsProtected = na_if(LND_7_percentKBAsProtected, -9999)) %>%
  mutate(LND_7_percentKBAsProtected = 100 * LND_7_percentKBAsProtected) %>% 
  mutate(LND_8_percentKBAsBuiltup = na_if(LND_8_percentKBAsBuiltup, -9999)) %>%
  mutate(LND_8_percentKBAsBuiltup = 100 * LND_8_percentKBAsBuiltup) %>% 
  # ACC
  mutate(ACC_2_percentOpenSpaceinBuiltup2022 = 100 * ACC_2_percentOpenSpaceinBuiltup2022) %>% 
  mutate(ACC_3_percentPopwOpenSpaceAccess2022 = 100 * ACC_3_percentPopwOpenSpaceAccess2022) %>% 
  mutate(ACC_4_percentPopwTreeCoverAccess2022 = 100 * ACC_4_percentPopwTreeCoverAccess2022) %>% 
  # FLD
  mutate(FLD_1_percentFloodProneinBuiltup2050 = 100 * FLD_1_percentFloodProneinBuiltup2050) %>% 
  mutate(FLD_2_percentChangeinMaxDailyPrecip2020to2050 = 100 * FLD_2_percentChangeinMaxDailyPrecip2020to2050) %>% 
  mutate(FLD_3_percentBuiltupWithin1mAboveDrainage = 100 * FLD_3_percentBuiltupWithin1mAboveDrainage) %>% 
  mutate(FLD_4_percentImperviousinBuiltup2018 = 100 * FLD_4_percentImperviousinBuiltup2018) %>% 
  mutate(FLD_5_percentBuiltupWOvegetationcover2020 = 100 * FLD_5_percentBuiltupWOvegetationcover2020) %>% 
  mutate(FLD_6_percentRiparianZonewoVegorWatercover2020 = 100 * FLD_6_percentRiparianZonewoVegorWatercover2020) %>% 
  mutate(FLD_7_percentSteepSlopesWOvegetationcover2020 = 100 * FLD_7_percentSteepSlopesWOvegetationcover2020)







############### get cities comparison list ----------------


indicators_comparison = indicators %>% 
  filter(geo_id %in% boundary_georef$city_id)


print(indicators_comparison)

# label indicator map function -----

pal.indicator.fun = function(selected_indicator_values){
  if(sum(is.na(selected_indicator_values)) == length(selected_indicator_values))
  {
    print("NOT available")
    selected_indicator_values = 0
    
    pal_indicator<- colorNumeric(palette = "gray",
                                 domain = selected_indicator_values,
                                 na.color = "gray",
                                 revers = FALSE)
  } else {
    pal_indicator<- colorNumeric(palette = "Greens",
                                 domain = selected_indicator_values,
                                 na.color = "gray",
                                 revers = FALSE)
  }
  return(pal_indicator)
}

############### App

ui = tagList(
  useShinyjs(),
  navbarPage(title = div("Indicators Dashboard",
                         img(src = logo_file,
                             height = logo_height,
                             style = "top: -3px;
                                    right: -900px;padding-right:100px;")),
             id = "active_tab",
             
             ### Indicators tab ----
             tabPanel("Indicators",
                      
                      ### Filters ----
                      fluidRow(
                        
                        column(3,
                               
                               # ### Select the project  ----
                               # selectInput(inputId = "project",
                               #             label = tags$span(style="color: #242456;","Select project"),
                               #             choices = c("urbanshift",'cities4forests'),
                               #             selected = "urbanshift",
                               #             width = '100%'),
                               
                               ### Select city  ----
                               selectInput(inputId = "city",
                                           label = tags$span(style="color: #242456;","Select your city"),
                                           # choices =  NULL,
                                           choices = cities,
                                           selected = default_city,
                                           width = '100%'),
                               
                               # select theme ----
                               selectizeInput(inputId = "theme",
                                              label = tags$span(style="color: #242456;","Theme"),
                                              # choices =  NULL,
                                              choices = indicators_themes,
                                              selected = default_theme,
                                              multiple = FALSE,
                                              width = '100%'),
                               
                               # select indicator ----
                               selectizeInput(inputId = "indicator",
                                              label = tags$span(style="color: #242456;","Select indicator"),
                                              # choices =  NULL,
                                              choices = indicators_list,
                                              selected = default_indicator,
                                              multiple = FALSE,
                                              width = '100%'),
                               
                               # Main indicators
                               
                               tags$span(h4("City wide level: "),
                                         style="color: #242456;"),
                               htmlOutput("city_wide_indicator"),
                               
                               
                        ),
                        ### Specify plots ----  
                        column(8,
                               div(style = "background-color: red; width: 100%; height: 100%;"),
                               tabsetPanel(type = "tabs",
                                           id = "tabs",
                                           ### Map plot
                                           tabPanel("Map", 
                                                    # withSpinner(leafletOutput("indicator_map", 
                                                    #               height = 500)),
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
                                                    # download geo data
                                                    downloadButton(outputId = "download_geo_data",
                                                                   label = "Download geospatial data"),
                                                    # # download geo data
                                                    # downloadButton(outputId = "download_map",
                                                    #                label = "Download map")
                                           ),
                                           ### Table plot
                                           tabPanel("Table", DT::dataTableOutput("indicator_table"),
                                                    downloadButton(outputId = "downloadData",
                                                                   label = "Download tabular data")),
                                           ### barchart 
                                           tabPanel("Chart", 
                                                    plotlyOutput("indicator_chart",
                                                                 height = 500)),
                                           
                                           ## Cities comparison
                                           tabPanel("Benchmark", plotlyOutput("cities_comparison_plot",
                                                                              height = 500),
                                                    downloadButton(outputId = "downloadDataBenchmark",
                                                                   label = "Download benchmark data")),
                                           ### Data description
                                           tabPanel("Definitions", htmlOutput("indicator_definition", 
                                                                              height = 500))
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
  
  # # Update cities based on selected project
  # observeEvent(input$project,{
  #   updateSelectInput(session,
  #                     'city',
  #                     choices=unique(boundary_georef[boundary_georef$project_name==input$project, "geo_name"]),
  #                     selected = unique(boundary_georef[boundary_georef$project_name==input$project, "geo_name"])[1],
  #   )
  # 
  # })
  
  # Update themes based on selected project
  observeEvent(input$project,{
    updateSelectInput(session,
                      'theme',
                      choices=unique(indicators_definitions[indicators_definitions$project_name==input$project, "theme"]),
                      selected = unique(indicators_definitions[indicators_definitions$project_name==input$project, "theme"])[1],
    )
  })
  
  
  
  # Update indicators based on selected theme
  observeEvent(input$theme,{
    updateSelectInput(session,
                      'indicator',
                      choices=unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_label"]),
                      selected = unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_label"])[1],
    )
  })
  
  # hide tab based on selected indicator
  observeEvent(input$indicator, {
    if(input$indicator %in% c("Change in greenhouse gas emissions")){
      showTab(inputId = "tabs", target = "Chart")
      showTab(inputId = "tabs", target = "Definitions")
      showTab(inputId = "tabs", target = "Table")
      hideTab(inputId = "tabs", target = "Map")
      show("city_wide_indicator") 
    } else if(input$indicator %in% c("High pollution days",
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
    } else if(input$indicator == "Exposure to PM 2.5"){
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
    } else if(!input$indicator %in% c("Exposure to PM 2.5",
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
    
    # selected_project = input$project
    # print(selected_project)
    
    selected_city = input$city
    print(selected_city)
    
    
    
    # read boundaries -----
    aoi_boundary_name = boundary_georef[boundary_georef$geo_name == input$city, "aoi_boundary_name"]
    units_boundary_name = boundary_georef[boundary_georef$geo_name == input$city, "units_boundary_name"]
    
    aoi_boundary_name = aoi_boundary_name[1]
    units_boundary_name = units_boundary_name[1]
    
    
    boundary_aoi = st_read(paste(aws_s3_path,
                                 "data/boundaries/boundary-",
                                 selected_city,
                                 "-",
                                 aoi_boundary_name,
                                 ".geojson",
                                 sep = "")
    )
    
    print(head(boundary_aoi))
    
    boundary_unit = st_read(paste(aws_s3_path,
                                  "data/boundaries/boundary-",
                                  selected_city,
                                  "-",
                                  units_boundary_name,
                                  ".geojson",
                                  sep = "")
    )
    
    # join boundaries with indicators ----------------
    
    aoi_indicators = boundary_aoi %>%
      dplyr::select(geo_id) %>%
      left_join(indicators, by = "geo_id")
    
    
    print(head(aoi_indicators))
    
    unit_indicators = boundary_unit %>%
      dplyr::select(geo_id) %>%
      left_join(indicators, by = "geo_id")
    
    print(head(unit_indicators))
    
    # get selected indicator
    
    selected_indicator_label = input$indicator
    
    selected_indicator_name = indicators_definitions %>%
      filter(indicator_label %in% selected_indicator_label) %>%
      pull(indicator_name_generic)
    # pull(indicator_name)
    
    print(selected_indicator_name)
    
    # get indicator legend  -----
    selected_indicator_legend = indicators_definitions %>%
      filter(indicator_label %in% selected_indicator_label) %>%
      pull(indicator_legend)
    
    # get indicator values  -----
    
    selected_indicator_values = unit_indicators %>%
      as.data.frame() %>%
      pull(selected_indicator_name)
    
    # indicator color values ----
    
    pal_indicator = pal.indicator.fun(selected_indicator_values)
    
    # indicator labels for map ----
    
    labels_indicator <- sprintf("<strong>%s</strong><br/>%s: %s",
                                unit_indicators$geo_name,
                                selected_indicator_label,
                                round(selected_indicator_values, 2)) %>%
      lapply(htmltools::HTML)
    
    
    ########################
    # collect layers ----
    ########################
    
    # layers: esa world cover
    if(input$indicator %in% c(
      # urbanshift indicators
      "Natural Areas",
      "Connectivity of ecological networks",
      "Biodiversity in built-up areas (birds)",
      "Built-up Key Biodiversity Areas",
      "Urban open space for public use",
      # c4f indicators
      "Open space for public use",
      "Surface reflectivity",
      "Built land without tree cover",
      "Exposure to coastal and river flooding",
      "Land near natural drainage",
      "Built areas without vegetation cover",
      "Impervious surfaces",
      "High land surface temperature")){
      
      if(selected_project == "urbanshift"){
        esa_worldcover_data_path = paste("/vsicurl/",
                                         aws_s3_path,
                                         "data/land_use/esa_world_cover/",
                                         selected_city,
                                         "-",
                                         aoi_boundary_name,
                                         "-ESA-world_cover-2020-50m.tif",
                                         sep = "")
      } else if(selected_project == "cities4forests"){
        esa_worldcover_data_path = paste("/vsicurl/",
                                         aws_s3_path,
                                         "data/land_use/esa_world_cover/",
                                         selected_city,
                                         "-",
                                         aoi_boundary_name,
                                         "-ESA-world_cover-2020-50m.tif",
                                         sep = "")
      }
      
      
      
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
      
      # ESA land cover - natural areas
      city_worldcover_natural_areas_mask = city_esa_worldcover
      
      city_worldcover_natural_areas_mask[!city_worldcover_natural_areas_mask%in% c("10","20","30","90","95","100")] <- NA
      
      city_worldcover_natural_areas = mask(x = city_esa_worldcover,
                                           mask = city_worldcover_natural_areas_mask)
      
    }
    
    
    # layers: GLAD land cover
    if(input$indicator %in% c(
      # urbanshift indicators
      "Proportion of natural areas restored",
      "Number of habitat types restored",
      "Change in vegetation and water cover")){
      
      
      # GLAD 2000 
      glad_2000_data_path = paste("/vsicurl/",
                                  aws_s3_path,
                                  "data/land_use/GLAD-ARD/",
                                  selected_city,
                                  "-",
                                  aoi_boundary_name,
                                  "-GLADlandcover-2000-100m.tif",
                                  sep = "")
      
      # collect raster data
      city_glad_2000 = raster(glad_2000_data_path)
      
      city_glad_2000 = raster::mask(city_glad_2000,
                                    boundary_aoi)
      
      # GLAD 2020 
      
      glad_2020_data_path = paste("/vsicurl/",
                                  aws_s3_path,
                                  "data/land_use/GLAD-ARD/",
                                  selected_city,
                                  "-",
                                  aoi_boundary_name,
                                  "-GLADlandcover-2020-100m.tif",
                                  sep = "")
      
      # collect raster data
      city_glad_2020 = raster(glad_2020_data_path)
      
      city_glad_2020 = raster::mask(city_glad_2020,
                                    boundary_aoi)
      
      
      # color ----
      bareGround_0 = "#FEFECC"
      shortVegetation_1 = "#B9B91E"
      forest_2 = "#347834"
      tallForest_3 = "#0D570D"
      wetlandShortVegetation_4 = "#88CAAD"
      wetlandForest_5 = "#589558"
      water_6 = "#6BAED6"
      snowIce_7 = "#ACD1E8"
      cropland_8 = "#FFF183"
      built_9 = "#E8765D"
      
      glad_col = c(bareGround_0,
                   shortVegetation_1,
                   forest_2,
                   tallForest_3,
                   wetlandShortVegetation_4,
                   wetlandForest_5,
                   water_6,
                   snowIce_7,
                   cropland_8,
                   built_9)
      glad_labels = c('bare ground',
                      'short vegetation',
                      'forest',
                      'tall forest',
                      'wetland - short vegetation',
                      'wetland - forest',
                      'water',
                      'snow/ice',
                      'cropland',
                      'built-up')
      
      
      # define a color palette
      pal_glad <- colorFactor(palette = glad_col,
                              levels = c("0","1","2","3","4","5",
                                         "6","7","8","9"),
                              na.color = "transparent")
      
      
      # GLAD changes ----
      
      glad_change_data_path = paste("/vsicurl/",
                                    aws_s3_path,
                                    "data/land_use/GLAD-ARD/",
                                    selected_city,
                                    "-",
                                    aoi_boundary_name,
                                    "-habitatchange-2000to2020-100m.tif",
                                    sep = "")
      
      
      # collect raster data
      city_glad_change = raster(glad_change_data_path)
      
      city_glad_change = raster::mask(city_glad_change,
                                      boundary_aoi)
      
      city_glad_change[city_glad_change==0] = NA
      
      # define color palette for WOrld cover
      habitat_loss_10 = "red"
      habitatr_gain_1 = "purple"
      
      
      glad_change_col = c(habitat_loss_10,
                          habitatr_gain_1)
      glad_change_labels = c('Habitat loss',
                             'Habitat gain')
      
      
      # define a color palette
      pal_glad_change <- colorFactor(palette = glad_change_col,
                                     levels = c("10","1"),
                                     na.color = "transparent")
      
      # GLAD changes loss ----
      
      city_glad_change_loss = city_glad_change
      
      city_glad_change_loss[!city_glad_change_loss==10] = NA
      
      # GLAD changes gain ----
      
      city_glad_change_gain = city_glad_change
      
      city_glad_change_gain[!city_glad_change_gain==1] = NA
      
    }
    
    # layers: OSM open space ----
    if(input$indicator %in% c(
      # urbanshift indicators
      "Recreational space per capita",
      "Urban open space for public use",
      "Proximity to public open space",
      # c4f indicators
      "Open space for public use",
      "Access to public open space"
    )){
      
      
      osm_open_space_path = paste(aws_s3_path,
                                  "data/open_space/openstreetmap/",
                                  selected_city,
                                  "-",
                                  aoi_boundary_name,
                                  "-OSM-open_space-2022.geojson",
                                  sep = "")
      
      osm_open_space = st_read(osm_open_space_path)
      
      
    }
    
    # layers: population ----
    if(input$indicator %in% c(
      # urbanshift indicators
      "Recreational space per capita",
      "Proximity to public open space",
      "Proximity to tree cover",
      # c4f indicators
      "Access to public open space",
      "Access to tree cover",
      "Exposure to PM 2.5"))
    {
      
      if(selected_project == "urbanshift"){
        pop_data_path = paste("/vsicurl/",
                              aws_s3_path,
                              "data/population/worldpop-16bit/",
                              selected_city,
                              "-",
                              aoi_boundary_name,
                              "-WorldPop-population-2020.tif",
                              sep = "")
      } else if(selected_project == "cities4forests"){
        pop_data_path = paste("/vsicurl/",
                              aws_s3_path,
                              "data/population/worldpop-16bit/",
                              selected_city,
                              "-",
                              aoi_boundary_name,
                              # "-WorldPop-population.tif",
                              "-WorldPop-population-2020.tif",
                              sep = "")
      }
      
      
      # collect raster data
      city_pop = raster(pop_data_path)
      
      city_pop[city_pop < 0] <- NA
      
      city_pop_boundary = raster::mask(city_pop,
                                       boundary_aoi)
      
      # color pop
      pop_values = values(city_pop_boundary)[!is.na(values(city_pop_boundary))]
      
      pal_pop <- colorNumeric("RdYlBu",
                              pop_values,
                              na.color = "transparent",
                              reverse = TRUE)
      
    }
    
    
    # layers: Population with access to open space within 400 meters ----
    
    if(input$indicator %in% c(
      # urbanshift indicators
      "Proximity to public open space",
      # c4f indicators
      "Access to public open space")){
      
      
      pop_openspace_data_path = paste("/vsicurl/",
                                      aws_s3_path,
                                      "data/population/worldpop-16bit/",
                                      # "data/population/worldpop/",
                                      selected_city,
                                      "-",
                                      aoi_boundary_name,
                                      "-population-wOpenSpace-2020.tif",
                                      sep = "")
      
      
      # collect raster data
      city_pop_openspace = raster(pop_openspace_data_path)
      
      city_pop_openspace[city_pop_openspace < 0] <- NA
      
      city_pop_openspace_boundary = raster::mask(city_pop_openspace,
                                                 boundary_aoi)
      
      # color pop
      pop_openspace_values = values(city_pop_openspace_boundary)[!is.na(values(city_pop_openspace_boundary))]
      
      pal_pop_openspace <- colorNumeric("RdYlBu",
                                        pop_openspace_values,
                                        na.color = "transparent",
                                        reverse = TRUE)
      
    }
    
    # layers: tree cover----
    
    if(input$indicator %in% 
       # urbanshift indicators
       c("Proximity to tree cover",
         "Tree cover",
         # c4f indicators 
         "Access to tree cover",
         "Built land without tree cover") &
       # cities without tree cover data
       !input$city %in% cities_no_tml_data){
      if(selected_project == "urbanshift"){
        tml_data_path = paste(aws_s3_path,
                              # "data/",
                              # selected_project,
                              # "/tree_cover/tree_mosaic_land/",
                              "data/tree_cover/tree_mosaic_land/",
                              selected_city,
                              "-",
                              aoi_boundary_name,
                              "-TML-tree_cover-2020-50m.tif",
                              sep = "")
      } else if(selected_project == "cities4forests"){
        tml_data_path = paste("/vsicurl/",
                              aws_s3_path,
                              # "data/",
                              # selected_project,
                              # "/tree_cover/tree_mosaic_land/",
                              "data/tree_cover/tree_mosaic_land/",
                              selected_city,
                              "-",
                              aoi_boundary_name,
                              "-TML-tree_cover-2020-50m.tif",
                              sep = "")
      }
      
      
      
      
      # collect raster data
      city_tml = raster(tml_data_path)
      
      city_tml_boundary = raster::mask(city_tml,
                                       boundary_aoi)
      
      city_tml_boundary[city_tml_boundary==0] = NA
      
      
      
      # define color for tree cover
      pal_tml <- colorNumeric(palette = "Greens",
                              domain = values(city_tml_boundary), 
                              na.color = "transparent")
      
    }
    
    
    
    # layers: Population with access to at least 10% mean tree cover within 400 meters (persons per hectare) ----
    if(input$indicator %in%
       # urbanshift indicators
       c("Proximity to tree cover",
         # c4f indicators
         "Access to tree cover") &
       # cities without tree cover data
       !input$city %in% cities_no_tml_data){
      
      pop_tree_cover_data_path = paste("/vsicurl/",
                                       aws_s3_path,
                                       # "data/population/worldpop/",
                                       "data/population/worldpop-16bit/",
                                       selected_city,
                                       "-",
                                       aoi_boundary_name,
                                       "-population-wTreeCover-2020.tif",
                                       sep = "")
      
      
      # collect raster data
      pop_tree_cover_data = raster(pop_tree_cover_data_path)
      
      pop_tree_cover_data[pop_tree_cover_data < 0] <- NA
      
      city_pop_tree_cover = raster::mask(pop_tree_cover_data,
                                         boundary_aoi)
      
      # color pop
      pop_tree_cover_values = values(city_pop_tree_cover)[!is.na(values(city_pop_tree_cover))]
      
      pal_pop_tree_cover <- colorNumeric("RdYlBu",
                                         pop_tree_cover_values,
                                         na.color = "transparent",
                                         reverse = TRUE)
      
    }
    
    
    # layers: Flooding impervious surfaces  -----  
    if(input$indicator %in%
       # urbanshift indicators
       c("Permeable areas",
         # c4f indicators
         "Impervious surfaces")){
      
      impervious_data_path = paste("/vsicurl/",
                                   aws_s3_path,
                                   "data/impervious/tsinghua/",
                                   selected_city,
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
    
    
    # layers: Protected areas ----
    if(input$indicator %in% c("Protected areas",
                              "Protection of Key Biodiversity Areas") & !input$city %in% c("BRA-Belem",
                                                                                           "ARG-Mar_del_Plata",
                                                                                           "ARG-Ushuaia",
                                                                                           "BRA-Teresina",
                                                                                           "CHN-Ningbo",
                                                                                           "IDN-Balikpapan",
                                                                                           "IDN-Semarang",
                                                                                           "IND-Chennai",
                                                                                           "IND-Pune",
                                                                                           "IND-Surat",
                                                                                           "MAR-Marrakech",
                                                                                           "RWA-Kigali")){
      
      wdpa_data_path = paste(aws_s3_path,
                             "data/biodiversity/WDPA/",
                             selected_city,
                             "-",
                             aoi_boundary_name,
                             "-WDPA-2022.geojson",
                             sep = "")
      
      wdpa = st_read(wdpa_data_path)
      
    }
    
    # layers: Key Biodiversity Areas ----
    if(input$indicator %in% c("Protection of Key Biodiversity Areas",
                              "Built-up Key Biodiversity Areas") & !input$city %in% c("BRA-Belem",
                                                                                      "BRA-Teresina")){
      
      kba_data_path = paste(aws_s3_path,
                            "data/biodiversity/KBA/",
                            selected_city,
                            "-",
                            aoi_boundary_name,
                            "-KBA-2022.geojson",
                            sep = "")
      
      kba = st_read(kba_data_path)
      
      
    }
    
    
    # layers: GBIF - Vascular plant species  ----
    if(input$indicator %in% c("Vascular plant species")){
      
      # gbif_Tracheophyta_data_path = paste(aws_s3_path,
      #                       "data/",
      #                       selected_project,
      #                       "/biodiversity/gbif/Tracheophyta-",
      #                       selected_city,
      #                       "-",
      #                       aoi_boundary_name,
      #                       ".geojson",
      #                       sep = "")
      
      gbif_Tracheophyta_data_path = paste(aws_s3_path,
                                          "data/biodiversity/GBIF/Tracheophyta_observations/",
                                          selected_city,
                                          "-",
                                          aoi_boundary_name,
                                          "-Tracheophyta-2016-2021.geojson",
                                          sep = "")
      
      gbif_Tracheophyta = st_read(gbif_Tracheophyta_data_path)
      
      
      gbif_Tracheophyta = gbif_Tracheophyta %>% 
        mutate(long = unlist(map(gbif_Tracheophyta$geometry,1)),
               lat = unlist(map(gbif_Tracheophyta$geometry,2))) %>% 
        as.data.frame()
      
    }
    
    # layers: GBIF - Bird species  ----
    if(input$indicator %in% c("Bird species",
                              "Biodiversity in built-up areas (birds)")){
      
      # gbif_Aves_data_path = paste(aws_s3_path,
      #                                     "data/",
      #                                     selected_project,
      #                                     "/biodiversity/gbif/Aves-",
      #                                     selected_city,
      #                                     "-",
      #                                     aoi_boundary_name,
      #                                     ".geojson",
      #                                     sep = "")
      
      gbif_Aves_data_path = paste(aws_s3_path,
                                  "data/biodiversity/GBIF/Aves_observations/",
                                  selected_city,
                                  "-",
                                  aoi_boundary_name,
                                  "-Aves-2016-2021.geojson",
                                  sep = "")
      
      gbif_Aves = st_read(gbif_Aves_data_path)
      
      gbif_Aves = gbif_Aves %>% 
        mutate(long = unlist(map(gbif_Aves$geometry,1)),
               lat = unlist(map(gbif_Aves$geometry,2))) %>% 
        as.data.frame()
      
    }
    
    
    # layers: GBIF - Arthropoda  ----
    if(input$indicator %in% c("Arthropod species")){
      
      # gbif_Arthropod_data_path = paste(aws_s3_path,
      #                             "data/",
      #                             selected_project,
      #                             "/biodiversity/gbif/Arthropoda-",
      #                             selected_city,
      #                             "-",
      #                             aoi_boundary_name,
      #                             ".geojson",
      #                             sep = "")
      
      gbif_Arthropod_data_path = paste(aws_s3_path,
                                       "data/biodiversity/GBIF/Arthropoda_observations/",
                                       selected_city,
                                       "-",
                                       aoi_boundary_name,
                                       "-Arthropoda-2016-2021.geojson",
                                       sep = "")
      
      gbif_Arthropod = st_read(gbif_Arthropod_data_path)
      
      
      gbif_Arthropod = gbif_Arthropod %>% 
        mutate(long = unlist(map(gbif_Arthropod$geometry,1)),
               lat = unlist(map(gbif_Arthropod$geometry,2))) %>% 
        as.data.frame()
      
    }
    
    
    # layers: Flooding  -----
    if(input$indicator == "Exposure to coastal and river flooding"){
      
      flood_data_path = paste("/vsicurl/",
                              aws_s3_path,
                              "data/flooding/aqueduct/",
                              selected_city,
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
    
    # layers: Flooding  -----
    if(input$indicator == "Land near natural drainage"){
      
      hand_data_path = paste("/vsicurl/",
                             aws_s3_path,
                             "data/flooding/global30mHAND/",
                             selected_city,
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
    
    # layers: Riparian zones  -----
    if(input$indicator == "Riparian zones without vegetation cover"){
      
      riparian_data_path = paste("/vsicurl/",
                                 aws_s3_path,
                                 "data/flooding/riparian/",
                                 selected_city,
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
      
      slopes_data_path = paste("/vsicurl/",
                               aws_s3_path,
                               "data/slope/nasa/",
                               selected_city,
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
      
      lst_data_path = paste("/vsicurl/",
                            aws_s3_path,
                            "data/land-surface-temperature/landsat/",
                            selected_city,
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
      
      albedo_data_path = paste("/vsicurl/",
                               aws_s3_path,
                               "data/albedo/sentinel-2/",
                               selected_city,
                               "-",
                               aoi_boundary_name,
                               "-S2-albedo-2021-50m.tif",
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
    if(input$indicator == "Exposure to PM 2.5"){
      
      pm25_data_path = paste("/vsicurl/",
                             aws_s3_path,
                             "data/air_pollution/acag/pm25/",
                             selected_city,
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
    if(input$indicator %in% 
       # urbanshift indicators
       c("Climate change impact of trees",
         # c4f indicators 
         "Carbon flux from trees")){
      if(selected_project == "urbanshift"){
        carbonflux_data_path = paste(aws_s3_path,
                                     # "data/",
                                     # selected_project,
                                     # "/tree_cover/wri-forest-carbon-fluxes/",
                                     "data/tree_cover/wri-forest-carbon-fluxes/",
                                     selected_city,
                                     "-",
                                     aoi_boundary_name,
                                     "-WRI-ForestCarbonFluxes-MgCO2eperHA2001-2021-100m.tif",
                                     sep = "")
      } else if(selected_project == "cities4forests"){
        carbonflux_data_path = paste("/vsicurl/",
                                     aws_s3_path,
                                     "data/tree_cover/wri-forest-carbon-fluxes/",
                                     selected_city,
                                     "-",
                                     aoi_boundary_name,
                                     "-WRI-ForestCarbonFluxes-MgCO2eperHA2001-2021-100m.tif",
                                     sep = "")
      }
      
      
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
    
    
    # layers: Flooding NDVI  -----
    if(input$indicator %in% c("Vegetation cover in built areas",
                              "Vegetation cover in riparian zones",
                              "Built areas without vegetation cover",
                              "Riparian zones without vegetation cover")){
      
      ndvi_data_path = paste(aws_s3_path,
                             "data/vegetation/sentinel-2/",
                             selected_city,
                             "-",
                             aoi_boundary_name,
                             "-vegetation-cover-2020-NDVItheshold0.4-50m.tif",
                             sep = "")
      
      
      # collect raster data
      city_ndvi = raster(ndvi_data_path)
      
      city_ndvi_boundary = raster::mask(city_ndvi,
                                        boundary_aoi)
      city_ndvi_boundary[city_ndvi_boundary==0] = NA
      values(city_ndvi_boundary) = values(city_ndvi_boundary) *0.01
      
      # color
      ndvi_values = values(city_ndvi_boundary)[!is.na(values(city_ndvi_boundary))]
      
      pal_ndvi<- colorNumeric("Greens",
                              ndvi_values,
                              na.color = "transparent",
                              reverse = FALSE)
      
      
    }
    
    # layers: population ----
    if(input$indicator %in% c(
      # urbanshift indicators
      "Change in vegetation and water cover"))
    {
      
      veg_water_data_path = paste("/vsicurl/",
                                  aws_s3_path,
                                  "data/vegetation/sentinel-2/",
                                  selected_city,
                                  "-",
                                  aoi_boundary_name,
                                  "-VegWater-change-2019to2022-50m.tif",
                                  sep = "")
      
      # collect raster data
      city_veg_water = raster(veg_water_data_path)
      
      city_veg_water_boundary = raster::mask(city_veg_water,
                                             boundary_aoi)
      
      # color pop
      veg_water_values = values(city_veg_water_boundary)[!is.na(values(city_veg_water_boundary))]
      
      pal_veg_water <- colorNumeric("RdYlGn",
                                    veg_water_values,
                                    na.color = "transparent",
                                    reverse = FALSE)
      
    }
    
    ########################
    # map indicator ----
    ########################
    
    # indicator layer ----
    m = leaflet(boundary_aoi) %>%
      # addTiles(group = "OSM (default)") %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
      # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addTiles(group = "Streets and sites (OpenStreetMap)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (ESRI)") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Light (Stamen)") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark (CartoDB)") %>%
      addScaleBar() %>%
      fitBounds(~as.numeric(st_bbox(boundary_aoi)[1]),
                ~as.numeric(st_bbox(boundary_aoi)[2]),
                ~as.numeric(st_bbox(boundary_aoi)[3]),
                ~as.numeric(st_bbox(boundary_aoi)[4])) %>% 
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
      # indicator layer - value
      addPolygons(data = unit_indicators,
                  group = selected_indicator_legend,
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
                opacity = 1,
                title = selected_indicator_legend,
                group = selected_indicator_legend,
                position = "topright",
                labFormat = labelFormat(suffix = "")) %>%
      
      # Layers control
      addLayersControl(
        # baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      addFullscreenControl()
    
    # BIO-1: Natural Areas ----
    if(input$indicator  %in% c("Natural Areas")){
      m = m %>% 
        # plot layer: ESA world cover 
        addRasterImage(city_esa_worldcover,
                       colors = pal_worldcover,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Land cover classes (ESA World Cover)") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "Land cover classes (ESA World Cover)",
                  group = "Land cover classes (ESA World Cover)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Raster of natural areas
        addRasterImage(city_worldcover_natural_areas,
                       colors = "#65B96B",
                       opacity = 1,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Natural areas (derived from ESA World Cover)",
                       layerId = "Natural areas (derived from ESA World Cover)") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land cover classes (ESA World Cover)",
                            "Natural areas (derived from ESA World Cover)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Land cover classes (ESA World Cover)",
                    "Natural areas (derived from ESA World Cover)")) 
    }
    
    # BIO-2: Connectivity of ecological networks ----
    if(input$indicator  %in% c("Connectivity of ecological networks")){
      m = m %>% 
        # Raster of natural areas
        addRasterImage(city_worldcover_natural_areas,
                       colors = "#65B96B",
                       opacity = 1,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Natural areas (derived from ESA World Cover)",
                       layerId = "Natural areas (derived from ESA World Cover)") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Natural areas (derived from ESA World Cover)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Natural areas (derived from ESA World Cover)")) 
    }
    
    # BIO-3: Biodiversity in built-up areas (birds) ----
    if(input$indicator  %in% c("Biodiversity in built-up areas (birds)")){
      m = m %>% 
        # plot layer: ESA world cover 
        addRasterImage(city_esa_worldcover,
                       colors = pal_worldcover,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Land cover classes (ESA World Cover)") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "Land cover classes (ESA World Cover)",
                  group = "Land cover classes (ESA World Cover)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # add gbif layer
        addCircleMarkers(lat = gbif_Aves$lat,
                         lng = gbif_Aves$long,
                         radius = 3,
                         fillColor = "green",
                         color  = "black",
                         stroke = TRUE,
                         weight = 0.8,
                         fillOpacity = 0.6,
                         popup = gbif_Aves$species,
                         group ="Bird species") %>% 
        # add cluster markers
        addMarkers(lat = gbif_Aves$lat,
                   lng = gbif_Aves$long, 
                   clusterOptions = markerClusterOptions(),
                   group = "Bird species clusters") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land cover classes (ESA World Cover)",
                            "Bird species",
                            "Bird species clusters"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Land cover classes (ESA World Cover)",
                    "Bird species",
                    "Bird species clusters")) 
    }
    
    # BIO-4: Vascular plant species ----
    if(input$indicator  %in% c("Vascular plant species")){
      m = m %>% 
        # add gbif layer
        addCircleMarkers(lat = gbif_Tracheophyta$lat,
                         lng = gbif_Tracheophyta$long,
                         radius = 3,
                         fillColor = "green",
                         color  = "black",
                         stroke = TRUE,
                         weight = 0.8,
                         fillOpacity = 0.6,
                         popup = gbif_Tracheophyta$species,
                         group ="Vascular plant species") %>% 
        # add cluster markers
        addMarkers(lat = gbif_Tracheophyta$lat,
                   lng = gbif_Tracheophyta$long, 
                   clusterOptions = markerClusterOptions(),
                   group = "Vascular plant species clusters") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Vascular plant species",
                            "Vascular plant species clusters"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Vascular plant species",
                    "Vascular plant species clusters")) 
    }
    
    # BIO-5: Bird species ----
    if(input$indicator  %in% c("Bird species")){
      m = m %>% 
        # add gbif layer
        addCircleMarkers(lat = gbif_Aves$lat,
                         lng = gbif_Aves$long,
                         radius = 3,
                         fillColor = "green",
                         color  = "black",
                         stroke = TRUE,
                         weight = 0.8,
                         fillOpacity = 0.6,
                         popup = gbif_Aves$species,
                         group ="Bird species") %>% 
        # add cluster markers
        addMarkers(lat = gbif_Aves$lat,
                   lng = gbif_Aves$long, 
                   clusterOptions = markerClusterOptions(),
                   group = "Bird species clusters") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Bird species",
                            "Bird species clusters"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Bird species",
                    "Bird species clusters")) 
    }
    
    # BIO-6: Arthropod species ----
    if(input$indicator  %in% c("Arthropod species")){
      m = m %>% 
        # add gbif layer
        addCircleMarkers(lat = gbif_Arthropod$lat,
                         lng = gbif_Arthropod$long,
                         radius = 3,
                         fillColor = "green",
                         color  = "black",
                         stroke = TRUE,
                         weight = 0.8,
                         fillOpacity = 0.6,
                         popup = gbif_Arthropod$species,
                         group ="Arthropod species") %>% 
        # add cluster markers
        addMarkers(lat = gbif_Arthropod$lat,
                   lng = gbif_Arthropod$long, 
                   clusterOptions = markerClusterOptions(),
                   group = "Arthropod species clusters") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Arthropod species",
                            "Arthropod species clusters"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Arthropod species",
                    "Arthropod species clusters")) 
    }
    
    # ACC-1: Recreational space per capita ----
    if(input$indicator %in% c("Recreational space per capita")){
      m = m %>%
        # plot layer: OSM 
        addPolygons(data = osm_open_space,
                    group = "Open spaces for public use (OpenStreetMap)",
                    stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                    smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.3,
                      bringToFront = TRUE)) %>%
        # plot layer: POP
        addRasterImage(city_pop_boundary,
                       colors = pal_pop ,
                       opacity = 0.9,
                       group = "Population density (persons per hectare, WorldPop)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Population") %>%
        # Legend for population
        addLegend(pal = pal_pop ,
                  values = pop_values,
                  opacity = 0.9,
                  title = "Population density (persons per hectare, WorldPop)",
                  group = "Population density (persons per hectare, WorldPop)",
                  position = "bottomleft") %>%
        # Layers control ----
      addLayersControl(
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Open spaces for public use (OpenStreetMap)",
                          "Population density (persons per hectare, WorldPop)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
        hideGroup(c("Open spaces for public use (OpenStreetMap)",
                    "Population density (persons per hectare, WorldPop)"))
    }
    
    # ACC-2: Urban open space for public use----
    if(input$indicator %in% c(
      # urbanshift 
      "Urban open space for public use",
      # c4f
      "Open space for public use") & !input$city %in% c("MEX-Mexico_City","MEX-Monterrey")){
      m = m %>% 
        # plot layer: OSM 
        addPolygons(data = osm_open_space,
                    group = "Open spaces for public use (OpenStreetMap)",
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
                     group = "Land cover classes (ESA World Cover)") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "Land cover classes (ESA World Cover)",
                  group = "Land cover classes (ESA World Cover)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Layers control ----
      addLayersControl(
        # baseGroups = c("OSM (default)", "Esri", "Toner Lite"),
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Land cover classes (ESA World Cover)",
                          "Open spaces for public use (OpenStreetMap)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Land cover classes (ESA World Cover)",
                    "Open spaces for public use (OpenStreetMap)")) 
    }
    
    # ACC-3: Recreational space per capita ----
    if(input$indicator %in% c(
      # urbanshift
      "Proximity to public open space",
      # c4f
      "Access to public open space")){
      m = m %>% 
        # plot layer: OSM 
        addPolygons(data = osm_open_space,
                    group = "Open spaces for public use (OpenStreetMap)",
                    stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                    smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.3,
                      bringToFront = TRUE)) %>% 
        # plot layer: POP
        addRasterImage(city_pop_boundary,
                       colors = pal_pop ,
                       opacity = 0.9,
                       group = "Population density (persons per hectare, WorldPop)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Population density (persons per hectare, WorldPop)") %>% 
        # Legend for population 
        addLegend(pal = pal_pop ,
                  values = pop_values,
                  opacity = 0.9,
                  title = "Population density <br> (persons per hectare, WorldPop)",
                  group = "Population density (persons per hectare, WorldPop)",
                  position = "bottomleft") %>% 
        # plot layer: POP openspace
        addRasterImage(city_pop_openspace_boundary,
                       colors = pal_pop_openspace ,
                       opacity = 0.9,
                       group = "Population with access to open space within 400 meters (persons per hectare)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Population with access to open space within 400 meters (persons per hectare)") %>% 
        # Legend for population openspace
        addLegend(pal = pal_pop_openspace ,
                  values = pop_openspace_values,
                  opacity = 0.9,
                  title = "Population with access to open space <br> within 400 meters <br> (persons per hectare)",
                  group = "Population with access to open space within 400 meters (persons per hectare)",
                  position = "bottomleft") %>% 
        # Layers control ----
      addLayersControl(
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Open spaces for public use (OpenStreetMap)",
                          "Population density (persons per hectare, WorldPop)",
                          "Population with access to open space within 400 meters (persons per hectare)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Open spaces for public use (OpenStreetMap)",
                    "Population density (persons per hectare, WorldPop)",
                    "Population with access to open space within 400 meters (persons per hectare)")) 
    }
    
    # ACC-4: Proximity to tree cover ----
    if(input$indicator %in% c(
      # urbanshift
      "Proximity to tree cover",
      # c4f
      "Access to tree cover") & !input$city %in% cities_no_tml_data){
      m = m %>% 
        # Raster of tree cover
        addRasterImage(city_tml_boundary, 
                       colors = pal_tml,
                       opacity = 0.9,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       layerId = "Tree cover (% of pixel with tree cover)",
                       group = "Tree cover (% of pixel with tree cover)") %>%
        addLegend(pal = pal_tml,
                  values = values(city_tml_boundary), #values(city_tml_aggregate),
                  title = "Tree cover <br> (% of pixel with tree cover)",
                  group = "Tree cover (% of pixel with tree cover)",
                  position = "bottomleft") %>%
        # plot layer: POP
        addRasterImage(city_pop_boundary,
                       colors = pal_pop ,
                       opacity = 0.9,
                       group = "Population density (persons per hectare, WorldPop)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Population density (persons per hectare, WorldPop)") %>% 
        # Legend for population 
        addLegend(pal = pal_pop ,
                  values = pop_values,
                  opacity = 0.9,
                  title = "Population density <br> (persons per hectare, WorldPop)",
                  group = "Population density (persons per hectare, WorldPop)",
                  position = "bottomleft") %>% 
        # plot layer: POP with access to tree cover ----
      addRasterImage(city_pop_tree_cover,
                     colors = pal_pop_tree_cover,
                     opacity = 0.9,
                     group = "Population with access to at least 10% mean tree cover <br> within 400 meters (persons per hectare)",
                     layerId = "Population with access to at least 10% mean tree cover <br> within 400 meters (persons per hectare)",
                     project=FALSE,
                     maxBytes = 8 * 1024 * 1024) %>%
        addLegend(pal = pal_pop_tree_cover ,
                  values = pop_tree_cover_values,
                  opacity = 0.9,
                  title = "Population with access to <br> at least 10% mean tree cover <br> within 400 meters (persons per hectare)",
                  group = "Population with access to at least 10% mean tree cover <br> within 400 meters (persons per hectare)",
                  position = "bottomleft") %>%
        # Layers control ----
      addLayersControl(
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Tree cover (% of pixel with tree cover)",
                          "Population density (persons per hectare, WorldPop)",
                          "Population with access to at least 10% mean tree cover <br> within 400 meters (persons per hectare)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Tree cover (% of pixel with tree cover)",
                    "Population density (persons per hectare, WorldPop)",
                    "Population with access to at least 10% mean tree cover <br> within 400 meters (persons per hectare)")) 
    }
    
    # LND-1: Permeable areas ----
    if(input$indicator %in% c("Permeable areas")){
      m = m %>% 
        # plot layer:Impervious surfaces 
        addRasterImage(city_impervious_boundary,
                       colors = "black",
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Impervious surfaces (Tsinghua GAIA)") %>% 
        # addRasterImage(city_impervious_boundary,
        #                colors = pal_impervious,
        #                opacity = 0.7,
        #                maxBytes = 20 * 1024 * 1024,
        #                project=FALSE,
        #                group = "Impervious surfaces (Tsinghua GAIA)") %>%
        #   addLegend(pal = pal_impervious,
        #             values = impervious_values,
        #             title = "Impervious surfaces (Tsinghua GAIA)",
        #             group = "Impervious surfaces (Tsinghua GAIA)",
        #             position = "bottomleft") %>% 
      # Layers control 
      addLayersControl(
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Impervious surfaces (Tsinghua GAIA)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Impervious surfaces (Tsinghua GAIA)")) 
    }
    
    # LND-2: Tree cover ----
    if(input$indicator %in% c("Tree cover") & !input$city %in% cities_no_tml_data){
      m = m %>% 
        # Raster of tree cover
        addRasterImage(city_tml_boundary, 
                       colors = pal_tml,
                       opacity = 0.9,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Tree cover <br> (% of pixel with tree cover)") %>%
        addLegend(pal = pal_tml,
                  values = values(city_tml_boundary), #values(city_tml_aggregate),
                  title = "Tree cover <br> (% of pixel with tree cover)",
                  group = "Tree cover <br> (% of pixel with tree cover)",
                  position = "bottomleft") %>%
        # Layers control ----
      addLayersControl(
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Tree cover <br> (% of pixel with tree cover)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Tree cover <br> (% of pixel with tree cover)")) 
    }
    
    # LND-3: Change in vegetation and water cover ----
    if(input$indicator %in% c("Change in vegetation and water cover")){
      m = m %>%
        # plot layer
        addRasterImage(city_veg_water_boundary,
                       colors = pal_veg_water ,
                       opacity = 0.9,
                       group = "Change in vegetation and water cover",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Change_vegetation_water") %>%
        # Legend for population
        addLegend(pal = pal_veg_water ,
                  values = veg_water_values,
                  opacity = 0.9,
                  title = "Change in vegetation and water cover",
                  group = "Change in vegetation and water cover",
                  position = "bottomleft") %>%
        # city_glad_2000
        addRasterImage(city_glad_2000,
                       colors = pal_glad,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Land cover classes 2000 <br> (UDM GLAD)") %>% 
        addLegend(colors = glad_col,
                  labels = glad_labels,
                  title = "Land cover classes <br> (UDM GLAD)",
                  group = "Land cover classes 2000 <br> (UDM GLAD)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # city_glad_2020
        addRasterImage(city_glad_2020,
                       colors = pal_glad,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Land cover classes 2020 <br> (UDM GLAD)") %>% 
        addLegend(colors = glad_col,
                  labels = glad_labels,
                  title = "Land cover classes <br> (UDM GLAD)",
                  group = "Land cover classes 2020 <br> (UDM GLAD)",
                  position = "bottomleft",
                  opacity = 1) %>% 
        addRasterImage(city_glad_change,
                       colors = pal_glad_change,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)") %>% 
        addLegend(colors = glad_change_col,
                  labels = glad_change_labels,
                  title = "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                  group = "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                  position = "bottomleft",
                  opacity = 1) %>% 
        # Layers control ----
      addLayersControl(
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Change in vegetation and water cover",
                          "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                          "Land cover classes 2000 <br> (UDM GLAD)",
                          "Land cover classes 2020 <br> (UDM GLAD)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
        hideGroup(c("Change in vegetation and water cover",
                    "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                    "Land cover classes 2000 <br> (UDM GLAD)",
                    "Land cover classes 2020 <br> (UDM GLAD)"))
    }
    
    # LND-4: Habitat areas restored + Habitat types restored ----
    if(input$indicator %in% c("Proportion of natural areas restored",
                              "Number of habitat types restored")){
      m = m %>% 
        # city_glad_2000
        addRasterImage(city_glad_2000,
                       colors = pal_glad,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Land cover classes 2000 <br> (UDM GLAD)") %>% 
        addLegend(colors = glad_col,
                  labels = glad_labels,
                  title = "Land cover classes <br> (UDM GLAD)",
                  group = "Land cover classes 2000 <br> (UDM GLAD)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # city_glad_2020
        addRasterImage(city_glad_2020,
                       colors = pal_glad,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Land cover classes 2020 <br> (UDM GLAD)") %>% 
        addLegend(colors = glad_col,
                  labels = glad_labels,
                  title = "Land cover classes <br> (UDM GLAD)",
                  group = "Land cover classes 2020 <br> (UDM GLAD)",
                  position = "bottomleft",
                  opacity = 1) %>% 
        addRasterImage(city_glad_change,
                       colors = pal_glad_change,
                       opacity = 1,
                       maxBytes = 100 * 1024 * 1024,
                       project=FALSE,
                       group = "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)") %>% 
        addLegend(colors = glad_change_col,
                  labels = glad_change_labels,
                  title = "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                  group = "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                  position = "bottomleft",
                  opacity = 1) %>% 
        # addRasterImage(city_glad_change_loss,
        #                colors = "red",
        #                opacity = 1,
        #                maxBytes = 100 * 1024 * 1024,
        #                project=FALSE,
        #                group = "Habitat loss") %>% 
        # addRasterImage(city_glad_change_gain,
        #                colors = "purple",
        #                opacity = 1,
        #                maxBytes = 100 * 1024 * 1024,
        #                project=FALSE,
      #                group = "Habitat gain") %>% 
      # Layers control ----
      addLayersControl(
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)",
                          "Land cover classes 2000 <br> (UDM GLAD)",
                          "Land cover classes 2020 <br> (UDM GLAD)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Land cover classes 2000 <br> (UDM GLAD)",
                    "Land cover classes 2020 <br> (UDM GLAD)",
                    "Habitat changes between 2000 and 2020 <br> (derived from UDM GLAD)")) 
    }
    
    # LND-6: Protected areas ----
    if(input$indicator %in% c("Protected areas") & !input$city %in% c("BRA-Belem",
                                                                      "ARG-Mar_del_Plata",
                                                                      "ARG-Ushuaia",
                                                                      "BRA-Teresina",
                                                                      "CHN-Ningbo",
                                                                      "IDN-Balikpapan",
                                                                      "IDN-Semarang",
                                                                      "IND-Chennai",
                                                                      "IND-Pune",
                                                                      "IND-Surat",
                                                                      "MAR-Marrakech",
                                                                      "RWA-Kigali")){
      m = m %>% 
        # plot layer: OSM 
        addPolygons(data = wdpa,
                    group = "Protected areas (WDPA)",
                    stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                    smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.3,
                      bringToFront = TRUE)) %>% 
        # Layers control 
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Protected areas (WDPA)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Protected areas (WDPA)")) 
    }
    
    
    # LND-7: Protection of Key Biodiversity Areas ----
    if(input$indicator %in% c("Protection of Key Biodiversity Areas") & !input$city %in% c("BRA-Belem",
                                                                                           "ARG-Mar_del_Plata",
                                                                                           "ARG-Ushuaia",
                                                                                           "BRA-Teresina",
                                                                                           "CHN-Ningbo",
                                                                                           "IDN-Balikpapan",
                                                                                           "IDN-Semarang",
                                                                                           "IND-Chennai",
                                                                                           "IND-Pune",
                                                                                           "IND-Surat",
                                                                                           "MAR-Marrakech",
                                                                                           "RWA-Kigali")){
      m = m %>% 
        # plot layer: WDPA 
        addPolygons(data = wdpa,
                    group = "Protected areas (WDPA)",
                    stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                    smoothFactor = 0.5, fill = TRUE, fillColor = "green",fillOpacity = 0.5,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.3,
                      bringToFront = TRUE)) %>% 
        # plot layer: KBA 
        addPolygons(data = kba,
                    group = "Key biodiversity areas <br> (KBA Partnership)",
                    stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                    smoothFactor = 0.5, fill = TRUE, fillColor = "yellow",fillOpacity = 0.5,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.3,
                      bringToFront = TRUE)) %>% 
        # Layers control 
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Protected areas (WDPA)",
                            "Key biodiversity areas <br> (KBA Partnership)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Protected areas (WDPA)",
                    "Key biodiversity areas <br> (KBA Partnership)")) 
    }
    
    
    # LND-8: Built-up Key Biodiversity Areas ----
    if(input$indicator %in% c("Built-up Key Biodiversity Areas") & !input$city %in% c("BRA-Belem",
                                                                                      "BRA-Teresina")){
      m = m %>% 
        # plot layer: KBA 
        addPolygons(data = kba,
                    group = "Key biodiversity areas <br> (KBA Partnership)",
                    stroke = TRUE, color = "black", weight = 1,dashArray = "1",
                    smoothFactor = 0.5, fill = TRUE, fillColor = "yellow",fillOpacity = 0.5,
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
                     group = "Land cover classes (ESA World Cover)") %>%
        addLegend(colors = worldcover_col,
                  labels = worldcover_labels,
                  title = "Land cover classes (ESA World Cover)",
                  group = "Land cover classes (ESA World Cover)",
                  position = "bottomleft",
                  opacity = 1) %>%
        # Layers control ----
      addLayersControl(
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Key biodiversity areas <br> (KBA Partnership)",
                          "Land cover classes (ESA World Cover)"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        hideGroup(c("Key biodiversity areas <br> (KBA Partnership)",
                    "Land cover classes (ESA World Cover)")) 
    }
    
    # GHG-2: Climate change impact of trees ----
    if(input$indicator %in% c(
      # urbanshift
      "Climate change impact of trees",
      # c4f
      "Carbon flux from trees")){
      m = m %>%
        # plot layer: carbon flux
        addRasterImage(city_carbonflux_boundary,
                       colors = pal_carbonflux ,
                       opacity = 0.9,
                       group = "Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)",
                       project=FALSE,
                       maxBytes = 8 * 1024 * 1024,
                       layerId = "Carbon flux from trees") %>%
        # Legend for population
        addLegend(pal = pal_carbonflux ,
                  values = carbonflux_values,
                  opacity = 0.9,
                  title = "Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)",
                  group = "Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)",
                  position = "bottomleft") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup(c("Carbon flux from trees <br> (net, Mg CO2e/ha, 2001 to 2021)"))
    }
    
    # HEA-2: High land surface temperature ----
    if(input$indicator == "High land surface temperature"){
      m = m %>% 
        # plot layer: ESA world cover
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
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land surface temperature",
                            "Land cover types"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(c("Land cover types",
                    "Land surface temperature")) 
    }
    
    
    # HEA-3: Surface reflectivity ----
    if(input$indicator == "Surface reflectivity"){
      m = m %>% 
        # plot layer: Albedo 
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
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Surface albedo",
                            "Land cover types"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(c("Surface albedo",
                    "Land cover types")) 
    }
    
    # HEA-4: Built land without tree cover ----
    if(input$indicator == "Built land without tree cover"){
      m = m %>% 
        # Raster of tree cover 
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
        # plot layer: ESA world cover 
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
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land cover types",
                            "Tree cover"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Tree cover",
                    "Land cover types")) 
    }
    
    # AQ-3: Exposure to PM 2.5 ----
    if(input$indicator == "Exposure to PM 2.5"){
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
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
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
        # plot layer: ESA world cover 
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
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land cover types",
                            "Coastal/riverine flooding"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Coastal/riverine flooding",
                    "Land cover types")) 
    }
    
    # FLD-3: Land near natural drainage ----
    if(input$indicator == "Land near natural drainage"){
      m = m %>% 
        # plot layer: ESA world cover 
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
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land cover types",
                            "Land within 1m height above nearest drainage"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>% 
        hideGroup(c("Land cover types",
                    "Land within 1m height above nearest drainage")) 
    }
    
    # FLD-4: 	Impervious surfaces ----
    if(input$indicator == "Impervious surfaces"){
      m = m %>%
        # plot layer: ESA world cover 
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
        # plot layer:Impervious surfaces 
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
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land cover types",
                            "Impervious areas"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup(c("Land cover types",
                    "Impervious areas"))
    }
    
    # FLD-5: Built areas without vegetation cover ----
    if(input$indicator == "Built areas without vegetation cover"){
      m = m %>%
        # plot layer: ESA world cover 
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
        # plot layer:NDVI 
        addRasterImage(city_ndvi_boundary,
                       colors = pal_ndvi,
                       opacity = 0.9,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Vegetation cover") %>%
        addLegend(pal = pal_ndvi,
                  values = ndvi_values,
                  title = "Vegetation cover",
                  group = "Vegetation cover",
                  position = "bottomleft") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Land cover types",
                            "Vegetation cover"),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup(c("Land cover types",
                    "Vegetation cover"))
    }
    
    # FLD-6: Riparian zones without vegetation cover ----
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
        # plot layer:NDVI 
        addRasterImage(city_ndvi_boundary,
                       colors = pal_ndvi,
                       opacity = 0.9,
                       maxBytes = 20 * 1024 * 1024,
                       project=FALSE,
                       group = "Vegetation cover") %>%
        addLegend(pal = pal_ndvi,
                  values = ndvi_values,
                  title = "Vegetation cover",
                  group = "Vegetation cover",
                  position = "bottomleft") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Streets and sites (OpenStreetMap)",
                         "Satellite (ESRI)", 
                         "Light (Stamen)",
                         "Dark (CartoDB)"),
          overlayGroups = c("Administrative boundaries",
                            selected_indicator_legend,
                            "Riparian buffer areas",
                            "Vegetation cover"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("Riparian buffer areas",
                    "Vegetation cover"))
    }
    
    # FLD-7: Vulnerable steep slopes ----
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
        #   # plot layer:NDVI 
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
        baseGroups = c("Streets and sites (OpenStreetMap)",
                       "Satellite (ESRI)", 
                       "Light (Stamen)",
                       "Dark (CartoDB)"),
        overlayGroups = c("Administrative boundaries",
                          selected_indicator_legend,
                          "Hillside slopes"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
        hideGroup(c("Hillside slopes"))
    }
    
    # plot  map   ------
    output$indicator_map <- renderLeaflet({
      m 
      # addOpacityControls(collapsed = TRUE,
      #                    category = c("image"),
      #                    size = "s",
      #                    position = "bottomright")
      
    })
    
    
    #########################################
    # download spatial data ----
    
    unit_indicators_download = unit_indicators %>% 
      dplyr::select(geo_id,
                    geo_level,
                    geo_name,
                    geo_parent_name,
                    selected_indicator_name)
    
    
    output$download_geo_data <- downloadHandler(
      filename = function() {
        paste("data-geo-", 
              selected_city,
              "-",
              selected_indicator_label,
              # Sys.Date(), 
              ".geojson", sep="")
      },
      content = function(file) {
        st_write(unit_indicators_download, file, driver = "GeoJSON")
      }
    )
    
    # download map view -----
    
    # https://stackoverflow.com/questions/48685818/save-leaflet-map-in-shiny-with-user-selected-layers
    
    # saveWidget(m, "temp.html", selfcontained = FALSE)
    # webshot("temp.html", file = "plot.png", cliprect = "viewport")
    # 
    # output$download_map <- downloadHandler(
    #   filename <- "map.png",
    #   content <- function(file) {
    #     file.copy("plot.png", file)
    #   }
    # )
    
    # store the current user-created version
    # of the Leaflet map for download in 
    # a reactive expression
    # user.created.map <- reactive({
    #   
    #   # call the foundational Leaflet map
    #   m_download = m %>%
    #     
    #     # store the view based on UI
    #     setView( lng = input$map_center$lng
    #              ,  lat = input$map_center$lat
    #              , zoom = input$map_zoom
    #     )
    #   
    # }) 
    
    # output$download_map <- downloadHandler(
    #   filename = "customLeafletmap.png",
    #   content = function(file) {
    #     mapshot(x = m, 
    #             file = file
    #              # , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
    #              # , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
    #     )
    #   } # end of content() function
    # ) # end of downloadHandler() function
    
    
    #########################################
    ### Main indicators ----
    
    # city wide 
    city_wide_indicator_value = aoi_indicators %>%
      as.data.frame() %>%
      pull(selected_indicator_name) %>% 
      round(2)
    
    # unit
    
    city_wide_indicator_value_unit = "%"
    
    if(input$indicator %in% c("Natural Areas",
                              # "Connectivity of ecological networks",
                              "Biodiversity in built-up areas (birds)",
                              "Permeable areas",
                              "Tree cover",
                              "Proportion of natural areas restored",
                              "Number of habitat types restored",
                              "Protected areas",
                              "Protection of Key Biodiversity Areas",
                              "Built-up Key Biodiversity Areas",
                              "Urban open space for public use",
                              "Urban open space for public use",
                              "Proximity to public open space",
                              "Proximity to tree cover")){
      city_wide_indicator_value_unit = "%"
    } else if(input$indicator %in% c("Vascular plant species",
                                     "Bird species",
                                     "Arthropod species",
                                     "Connectivity of ecological networks")){
      city_wide_indicator_value_unit = ""
      
    } else if(input$indicator %in% c("Recreational space per capita")){
      city_wide_indicator_value_unit = "hectares"
      
    } else if(input$indicator %in% c("Change in greenhouse gas emissions")){
      city_wide_indicator_value_unit = "% change"
      
    } else if(input$indicator %in% c("Climate change impact of trees")){
      city_wide_indicator_value_unit = "Mg CO2 eq/hectare"
      
    } else if (input$indicator %in% c('Nb days air pollution (carbon monoxide)',
                                      "High pollution days")){
      city_wide_indicator_value_unit = "days"
      
    } else if(input$indicator %in% c("Greenhouse gas emissions")){
      city_wide_indicator_value_unit = "% change"
      
    } else if(input$indicator %in% c("Air pollutant emissions")){
      city_wide_indicator_value_unit = "% change" 
      
    } else if(input$indicator %in% c("Carbon flux from trees")){
      city_wide_indicator_value_unit = "Mt CO2 eq/ha"
      
    } 
    
    
    # output text
    output$city_wide_indicator <- renderText({
      paste("<center>","<font size=5px; weight=500; color=\"#168A06\"><b>", 
            city_wide_indicator_value, 
            city_wide_indicator_value_unit,"<br>",
            "<font size=2px; weight=500; color=\"#168A06\"><b>",
            selected_indicator_legend)
    })
    
    
    #########################################
    ### Table ----
    
    # Table plot
    
    
    # urbanshift indicators
    if(input$indicator %in% c("Change in greenhouse gas emissions")){
      table_plot_years = aoi_indicators %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        pivot_longer(
          cols = bc_agl_2000_co2e:nmvoc_tro_2020_co2e,
          names_to = c("gas", "sector","year","unit"),
          names_pattern = "(.*)_(.*)_(.*)_(.*)",
          values_to = "value") %>% 
        dplyr::select(geo_name,
                      gas,
                      sector, 
                      year,
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
      
      table_plot = table_plot_years %>% 
        dplyr::select("Sector" = "sector",
                      "Gas" = "gas",
                      "Year" = "year",
                      "Emissions (tonnes of CO2 equivalent)" = "value")
    } 
    # c4f indicators
    else if(input$indicator == "High pollution days"){
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
    } 
    else if(input$indicator %in%  c("Air pollution (by pollutant)",
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
    } 
    else if(input$indicator %in%  c("Air pollutant emissions")){
      table_plot = aoi_indicators %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        pivot_longer(
          cols = bc_agl_2000_usd:nmvoc_tro_2020_usd,
          names_to = c("gas", "sector","year","unit"),
          names_pattern = "(.*)_(.*)_(.*)_(.*)",
          values_to = "value") %>% 
        dplyr::select(geo_id,
                      gas,
                      sector, 
                      year,
                      value) %>% 
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
      
      
    } 
    else if(input$indicator %in%  c("Greenhouse gas emissions")){
      
      table_plot_years = aoi_indicators %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        pivot_longer(
          cols = bc_agl_2000_co2e:nmvoc_tro_2020_co2e,
          names_to = c("gas", "sector","year","unit"),
          names_pattern = "(.*)_(.*)_(.*)_(.*)",
          values_to = "value") %>% 
        dplyr::select(geo_name,
                      gas,
                      sector, 
                      year,
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
      
      table_plot = table_plot_years %>% 
        dplyr::select("Sector" = "sector",
                      "Gas" = "gas",
                      "Year" = "year",
                      "Emissions (tonnes of CO2 equivalent)" = "value")
    } 
    # main indicators
    else {
      table_plot = unit_indicators %>% 
        drop_na(selected_indicator_name, geo_name) %>% 
        as.data.frame() %>%
        dplyr::select(-geometry) %>% 
        dplyr::select(geo_name,selected_indicator_name) %>% 
        mutate_if(is.numeric, round, 2) %>% 
        distinct(geo_name, .keep_all = T) %>% 
        arrange(desc(selected_indicator_name)) 
      
      # remove empty city name
      table_plot$geo_name[table_plot$geo_name==""]<-NA
      table_plot = table_plot %>% 
        drop_na(geo_name)
      
      selected_indicator_legend_table = str_remove(selected_indicator_legend, "<br> ")
      names(table_plot) = c("Name",selected_indicator_legend_table)
    }
    
    
    # plot table
    
    table_color = "Greens"
    
    output$indicator_table <- DT::renderDataTable(
      if(input$indicator %in% c("Change in greenhouse gas emissions",
                                "High pollution days",
                                "Greenhouse gas emissions",
                                "Air pollutant emissions")){
        DT::datatable(table_plot,
                      options = list(pageLength = 10,order = list(list(2, 'desc')))) 
      } 
      else {
        DT::datatable(table_plot, 
                      options = list(pageLength = 10,
                                     order = list(list(2, 'desc')))) %>% formatStyle(
                                       selected_indicator_legend_table,
                                       backgroundColor = styleInterval(seq(from = min(table_plot[,selected_indicator_legend_table]),
                                                                           to = max(table_plot[,selected_indicator_legend_table]),
                                                                           length.out = 8), 
                                                                       brewer.pal(9, table_color)
                                       ),
                                       fontWeight = 'bold')
      }
    )
    
    # output data to download
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$city,
              "-", 
              input$indicator,
              ".csv", 
              sep="")
      },
      content = function(file) {
        write.csv(table_plot,
                  file,
                  row.names=FALSE)
      }
    )
    
    #########################################
    ### Chart ----
    
    chart_color = "#2A553E"
    
    # urbanshift indicators
    if(input$indicator %in% c("Change in greenhouse gas emissions")){
      
      table_plot_2000 = table_plot_years %>% 
        filter(year == 2000)
      
      table_plot_2020 = table_plot_years %>% 
        filter(year == 2020)
      
      fig_2000 <- plot_ly(table_plot_2000, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year,
                          showlegend = FALSE) %>% 
        layout(yaxis = list(title = 'Greenhouse gas emissions <br> (tonnes of CO2 equivalent)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      
      fig_2020 <- plot_ly(table_plot_2020, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year,
                          showlegend = TRUE) %>% 
        layout(yaxis = list(title = 'Greenhouse gas emissions <br> (tonnes of CO2 equivalent)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      annotations = list( 
        list( 
          x = 0.2,  
          y = 1.0,  
          text = "2000",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ),  
        list( 
          x = 0.8,  
          y = 1,  
          text = "2020",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ))
      
      fig <- subplot(fig_2000, fig_2020, shareY = TRUE, margin = 0.05) %>% 
        layout(legend=list(title=list(text='<b> Gases </b>')),
               annotations = annotations)
    }
    # c4f indicators
    else if(input$indicator == "High pollution days"){
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
      
      
    } 
    else if(input$indicator == "Air pollutant emissions"){
      
      table_plot_2000 = table_plot %>% 
        filter(year == 2000)
      
      table_plot_2020 = table_plot %>% 
        filter(year == 2020)
      
      fig_2000 <- plot_ly(table_plot_2000, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year) %>% 
        layout(yaxis = list(title = 'Social cost ($USD)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      fig_2020 <- plot_ly(table_plot_2020, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year,
                          showlegend = FALSE) %>% 
        layout(yaxis = list(title = 'Social cost ($USD)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      annotations = list( 
        list( 
          x = 0.2,  
          y = 1.0,  
          text = "2000",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ),  
        list( 
          x = 0.8,  
          y = 1,  
          text = "2020",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ))
      
      fig <- subplot(fig_2000, fig_2020, shareY = TRUE, margin = 0.05) %>% 
        layout(legend=list(title=list(text='<b> Pollutants </b>')),
               annotations = annotations)
      
      
    } 
    else if(input$indicator == "Greenhouse gas emissions"){
      
      table_plot_2000 = table_plot_years %>% 
        filter(year == 2000)
      
      table_plot_2020 = table_plot_years %>% 
        filter(year == 2020)
      
      fig_2000 <- plot_ly(table_plot_2000, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year) %>% 
        layout(yaxis = list(title = 'Greenhouse gas emissions <br> (tonnes of CO2 equivalent)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      
      fig_2020 <- plot_ly(table_plot_2020, 
                          x = ~sector, 
                          y = ~value, 
                          type = 'bar',
                          name = ~gas,
                          color = ~gas,
                          legendgroup= ~year,
                          showlegend = FALSE) %>% 
        layout(yaxis = list(title = 'Greenhouse gas emissions <br> (tonnes of CO2 equivalent)'), 
               xaxis = list(title = 'sectors',categoryorder = "total descending"),
               barmode = 'stack')
      
      annotations = list( 
        list( 
          x = 0.2,  
          y = 1.0,  
          text = "2000",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ),  
        list( 
          x = 0.8,  
          y = 1,  
          text = "2020",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ))
      
      fig <- subplot(fig_2000, fig_2020, shareY = TRUE, margin = 0.05) %>% 
        layout(legend=list(title=list(text='<b> Gases </b>')),
               annotations = annotations)
      
      
    }
    # main indicators
    else {
      fig = plot_ly(x = table_plot$`Name`,
                    y = table_plot[[colnames(table_plot)[2]]],
                    type = "bar",
                    orientation = "v",
                    name = names(table_plot)[2],
                    color = I(chart_color)) %>% 
        layout(yaxis = list(title = selected_indicator_legend),
               xaxis = list(categoryorder = "total descending",
                            tickangle=45))
      
      
    }
    
    
    output$indicator_chart <- renderPlotly({
      fig
      
    })
    
    
    ########################################
    
    # cities comparison ----
    
    indicators_comparison = indicators_comparison %>% 
      as.data.frame() %>%
      dplyr::select(geo_name,selected_indicator_name) %>%
      drop_na(selected_indicator_name, geo_name) %>% 
      mutate(geo_name = recode(geo_name,
                               "SLE-Freetown_city" = "SLE-Freetown")) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      arrange(desc(selected_indicator_name)) 
    
    print(indicators_comparison)
    
    # change names
    names(indicators_comparison) = c("City name",selected_indicator_label)
    
    city_num = which(indicators_comparison$`City name` == selected_city)
    city_color = rep("#A0D1B4",nrow(indicators_comparison)) #, "grey"
    city_color[city_num] = "#2A553E" #"green"
    
    
    
    cities_indicator_avg = round(mean(indicators_comparison[[colnames(indicators_comparison)[2]]]),2)
    
    
    output$cities_comparison_plot <- renderPlotly({
      fig = plot_ly(x = indicators_comparison$`City name`,
                    y = indicators_comparison[[colnames(indicators_comparison)[2]]],
                    type = "bar",
                    orientation = "v",
                    name = names(indicators_comparison)[2],
                    marker = list(color = city_color)) %>% 
        layout(yaxis = list(title = selected_indicator_legend), 
               xaxis = list(title = 'Cities',categoryorder = "total descending"),
               annotations = list(
                 x = 10,
                 y = cities_indicator_avg+cities_indicator_avg*0.1, 
                 text = paste("Cities' average: ", 
                              cities_indicator_avg, 
                              city_wide_indicator_value_unit, 
                              sep = ""),
                 showarrow = FALSE,
                 xanchor = "right"
               ))
      
      add_trace(fig,
                y = cities_indicator_avg, 
                type='scatter',
                mode = 'lines',
                name = 'Average', 
                showlegend = F,
                line = list(color = 'black', 
                            dash = 'dot',
                            width = 1)) 
      
    })
    
    
    # benchmark data to download
    output$downloadDataBenchmark <- downloadHandler(
      filename = function() {
        paste("Benchmark-", input$indicator,".csv", sep="")
      },
      content = function(file) {
        write.csv(indicators_comparison,
                  file,
                  row.names=FALSE,
                  fileEncoding = "latin1")
      }
    )
    
    
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
      paste("<right>","<font size=3px; weight=100; color=\"#2A553E\"><b>",
            "<font color=\"#2A553E\"><b>", " ", "<br>",
            "<font color=\"#2A553E\"><b>","Definition: ",
            "<font color=\"#242456\"><b>", indicator_def_text,
            "<br/>",
            "<font color=\"#2A553E6\"><b>", " ", "<br>",
            "<font color=\"#2A553E\"><b>","Data sources: ",
            "<font color=\"#242456\"><b>", indicator_data_sources,
            "<br/>",
            "<font color=\"#2A553E\"><b>", " ", "<br>",
            "<font color=\"#2A553E\"><b>","Importance: ",
            "<font color=\"#242456\"><b>", indicator_importance,
            "<br/>",
            "<font color=\"#2A553E\"><b>", " ", "<br>",
            "<font color=\"#2A553E\"><b>","Methods: ",
            "<font weight=50; color=\"#242456\"><b>", indicator_methods
      )
    })
    
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
    
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
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

# define aws s3 path

aws_s3_path = "https://cities-cities4forests.s3.eu-west-3.amazonaws.com/"

############### Load data

indicators_themes = c("Health - heat",
                      "Health - air quality", 
                      "Greenspace access",
                      "Flooding",
                      "Climate mitigation")


indicators_list = c("Percent of tree cover")
indicators_list = c("Percent of Tree cover")

indicators_definitions = read.csv(paste(aws_s3_path,
                                        "data/indicators/indicators_definition.csv",
                                        sep = ""))

# read boundaries georef -----

boundary_georef = read.csv(paste(aws_s3_path,
                                 "data/boundaries/v_0/geo_ref.csv",
                                 sep = ""),
                           fileEncoding="UTF-8-BOM")

cities = unique(boundary_georef$geo_name)
cities = cities[!cities %in% c("BRA-Salvador","MEX-Monterrey")]


# read indicator ------------


indicators = read.csv(paste(aws_s3_path,
                            "data/indicators/cities_indicators.csv",
                            sep = ""),
                      encoding="UTF-8")





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
                                                choices = c("COD-Bukavu","COG-Brazzaville","COL-Barranquilla",
                                                            "ETH-Addis_Ababa","ETH-Dire_Dawa","KEN-Nairobi",
                                                            "MDG-Antananarivo","MEX-Mexico_City"),
                                                selected = "COD-Bukavu",
                                                width = '100%'),
                                    
                                    # select theme ----
                                    selectizeInput(inputId = "theme",
                                                   label = "Theme",
                                                   choices = indicators_themes,
                                                   selected = indicators_themes[1],
                                                   multiple = FALSE,
                                                   width = '100%'),
                                    
                                    # select indicator ----
                                    selectizeInput(inputId = "indicator",
                                                   label = "Select indicator",
                                                   choices = indicators_list,
                                                   selected = indicators_list[1],
                                                   multiple = FALSE,
                                                   width = '100%'),
                                    
                                    # Main indicators
                                    
                                    h5("City wide level: "),
                                    htmlOutput("city_wide_indicator"),
                             ),
                             ### Specify plots ----
                             column(8,
                                    div(style = "background-color: red; width: 100%; height: 100%;"),
                                    tabsetPanel(type = "tabs",
                                                ### Map plot
                                                tabPanel("Map", 
                                                         leafletOutput("indicator_map", 
                                                                       height = 500)),
                                                ### Table plot
                                                tabPanel("Table", DT::dataTableOutput("indicator_table"),
                                                         downloadButton(outputId = "downloadData",
                                                                        label = "Download data")),
                                                ### timeseirs plot
                                                tabPanel("Chart", 
                                                         plotlyOutput("indicator_chart",
                                                                      height = 500)),
                                                ### Data description
                                                tabPanel("Definitions", htmlOutput("indicator_definition", 
                                                                                     height = 500))
                                    )
                             )
                         )
                ),
                
                ## Second tab panel ----
                tabPanel("Layers"
                )
                
                
                
)

# Define server
server <- function(input, output, session) {
    
    # # Update indicators based on selected theme
    # observeEvent(input$theme,{
    #     updateSelectInput(session,
    #                       'indicator',
    #                       choices=unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_name"]),
    #                       selected = unique(indicators_definitions[indicators_definitions$theme==input$theme, "indicator_name"])[1],
    #     )
    # })
    
    
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
        
        # define color palette for percent of tree cover ----
        pal_percent_tree_cover<- colorNumeric(palette = "Greens", 
                                              domain = unit_indicators$percent_of_tree_cover,
                                              na.color = "gray",
                                              revers = FALSE)
        
        # define labels for percent of tree cover
        labels_percent_tree_cover <- sprintf("<strong>%s</strong><br/>%s: %s",
                                             unit_indicators$geo_name,
                                             "Percent of tree cover",
                                             round(unit_indicators$percent_of_tree_cover, 2)) %>% 
          lapply(htmltools::HTML)

        
        # read tml raster ----
        
        tml_data_path = paste("/vsicurl/https://cities-cities4forests.s3.eu-west-3.amazonaws.com/data/tree_cover/tree_mosaic_land/v_0/",
                              geo_name,
                              "-",
                              aoi_boundary_name,
                              "-TML-tree_cover-2000.tif",
                              sep = "")
        
        # collect raster data
        city_tml = raster(tml_data_path)
        
        city_tml_boundary = raster::mask(city_tml,
                                         boundary_aoi)
        city_tml_boundary[city_tml_boundary<11] = NA
        
        # define color for tree cover
        pal_tml <- colorNumeric(palette = "Greens",
                                domain = values(city_tml_boundary), 
                                na.color = "transparent")
        
        
        
        # map indicator ----
        
        
        output$indicator_map <- renderLeaflet({
            leaflet(boundary_aoi) %>%
                addTiles() %>%
                # setView(lat = city_boundary_centroid_lat, lng = city_boundary_centroid_lng, zoom = 5)
                fitBounds(~as.numeric(st_bbox(boundary_aoi)[1]),
                          ~as.numeric(st_bbox(boundary_aoi)[2]),
                          ~as.numeric(st_bbox(boundary_aoi)[3]),
                          ~as.numeric(st_bbox(boundary_aoi)[4]))
        })
        

        
        # plot map ----
        leafletProxy(mapId = "indicator_map")  %>%
            clearControls() %>%
            clearShapes() %>%
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
          # Add TML raster
          addRasterImage(city_tml_boundary, 
                         colors = pal_tml,
                         opacity = 0.9,
                         maxBytes = 20 * 1024 * 1024,
                         project=FALSE,
                         group = "Tree cover") %>%
          addLegend(pal = pal_tml,
                    values = values(city_tml_boundary),
                    title = "Tree cover",
                    group = "Tree cover",
                    position = "bottomleft") %>%
          # Percent of tree cover
          addPolygons(data = unit_indicators,
                      group = "Percent of Tree cover",
                      fillColor = ~pal_percent_tree_cover(percent_of_tree_cover),
                      weight = 1,
                      opacity = 1,
                      color = "grey",
                      fillOpacity = 0.8,
                      label = labels_percent_tree_cover,
                      highlightOptions = highlightOptions(color = "black", weight = 2,
                                                          bringToFront = FALSE),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 6px"),
                        textsize = "15px",
                        direction = "auto")) %>%
          addLegend(pal = pal_percent_tree_cover,
                    values = unit_indicators$percent_of_tree_cover,
                    opacity = 0.9,
                    title = "Tree cover (%)",
                    group = "Percent of Tree cover",
                    position = "topright",
                    labFormat = labelFormat(suffix = "")) %>%
          # Layers control
          addLayersControl(
            overlayGroups = c("Administrative boundaries",
                              "Percent of Tree cover",
                              "Tree cover"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>% 
          hideGroup(c("Tree cover")) %>% 
          addFullscreenControl()
        
        #########################################
        ### Main indicators ----
        
        # city wide  ----
        city_wide_indicator_value = aoi_indicators %>%
          as.data.frame() %>%
          pull("percent_of_tree_cover") %>% 
          round(2)
        
        output$city_wide_indicator <- renderText({
            paste("<center>","<font size=5px; weight=500; color=\"#168A06\"><b>", city_wide_indicator_value, "%")
        })
        
        #########################################
        ### Table ----
        
        # Table plot
        
        table_plot = unit_indicators %>% 
          drop_na(percent_of_tree_cover,geo_name) %>% 
          as.data.frame() %>%
          dplyr::select(-geometry) %>% 
          mutate(percent_of_tree_cover = round(percent_of_tree_cover,2)) %>% 
          dplyr::select("city name" = geo_name,
                        "Percent of Tree cover" = percent_of_tree_cover) %>% 
          arrange(desc(`Percent of Tree cover`)) 
        
        output$indicator_table <- DT::renderDataTable(
          DT::datatable(table_plot, 
                        options = list(pageLength = 25)) %>% formatStyle(
                          "Percent of Tree cover", 
                          backgroundColor = styleInterval(seq(from = min(table_plot$`Percent of Tree cover`),
                                                              to = max(table_plot$`Percent of Tree cover`),
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
        
        output$indicator_chart <- renderPlotly({
          fig = table_plot %>% 
            arrange(desc(`Percent of Tree cover`)) %>% 
            plot_ly(color = I("green4")) %>% 
            add_trace(x = ~`city name`,
                      y = ~ `Percent of Tree cover`, 
                      type = "bar",
                      orientation = "v",
                      name = 'Percent of Tree cover',
                      text = ~paste("Tree cover percent: ",`Percent of Tree cover`)) %>% 
            layout(yaxis = list(title = 'Percent of Tree cover (%)'),
                   xaxis = list(title = 'Cities'))
          
          fig
          
        })
        
        #########################################
        ### Indicator definition text  ----
        
        indicator_def_text = indicators_definitions %>% 
          filter(indicator_name_viz == input$indicator) %>% 
          pull(indicator_definition)
        
        indicator_data_sources = indicators_definitions %>% 
          filter(indicator_name_viz == input$indicator) %>% 
          pull(data_sources)
        
        # plot text 
        output$indicator_definition <- renderText({
          paste("<right>","<font size=3px; weight=100; color=\"#168A06\"><b>",
                "<font color=\"#168A06\"><b>", " ", "<br>",
                "<font color=\"#168A06\"><b>","Definition: ",
                "<font color=\"#454545\"><b>", indicator_def_text,
                "<br/>",
                "<font color=\"#168A06\"><b>", " ", "<br>",
                "<font color=\"#168A06\"><b>","Data sources: ",
                "<font color=\"#454545\"><b>", indicator_data_sources
                )
        })
        
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
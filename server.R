shinyServer(function(input, output, session) {
  showModal(
    modalDialog(
      title = "INSTRUCTIONS",
      "Welcome to the Narrative Visualization.Follow these instructions for an effective experince.",
      "Through this narrative visualization we take you on journey to understand Renewable Energy and its 
      dependency on GDP, CO2 and Weather.",
      tags$ol(
        tags$li(" Begin with reading the About box in Blue on each page."),
        tags$li(" Proceed to explore the map."),
        tags$li("Finally read the Trends box in green."),
        tags$li("Ensure you read the Description or About text before the Trends Text.")
       
      ),
      "Click anywhere to exit",
      #allow to exit by clicking any where on the screeen
      easyClose = TRUE,
      footer = NULL
    )
  )
  
  #Provide instruction while waiting for GDP animation to load
  observeEvent(input$gdp, {
    showModal(
      modalDialog(
        title = "Please Wait",
        "Loading GDP animation. This may take a few seconds.Click anywhere to exit.",
        #allow to exit by clicking any where on the screeen
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  #Provide instruction while waiting for CO2 animation to load
  observeEvent(input$co, {
    showModal(
      modalDialog(
        title = "Please Wait",
        "Loading CO2 animation. This may take a few seconds.Click anywhere to exit.",
        #allow to exit while clicking anywhere on the screen
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  #Hide trends of GDP and CO2 till annimation is clicked
  shinyjs::hide("trends_section")
  
  # Show trends  when GDP or CO2 button is clicked
  observeEvent(input$gdp, {
    shinyjs::show("trends_section")
  })
  
  observeEvent(input$co, {
    shinyjs::show("trends_section")
  })
  # Load world map
  world_map <- st_read("ne_50m_admin_0_countries.shp", quiet = TRUE)
  
  # Read the CSV file for weather 
  data <- read.csv("weather.csv")
  
  # Calculate average temperature for each country
  avg_temp_data <- data %>%
    group_by(country) %>%
    summarize(average_temperature = mean(tavg, na.rm = TRUE))
  #Calculate average wind speed for each country
  avg_wind_data <- data %>%
    group_by(country) %>%
    summarize(average_wind = mean(wspd, na.rm = TRUE))
  
  # Filter the world data to include only the countries present in avg_temp_data
  world_filtered <- world_map %>%
    filter(admin %in% avg_temp_data$country)
  
  # Merge the filtered world data with the temperature data
  world_data <- world_filtered %>%
    left_join(avg_temp_data, by = c("admin" = "country"))
  # Merge the filtered world data with the wind data
  world_data <- world_data %>%
    left_join(avg_wind_data, by = c("admin" = "country"))
  
  # Read renewable energy data
  renewable_data <- read.csv("country.csv") %>%
    group_by(name) %>%
    summarize(
      ren = mean(renewable, na.rm = TRUE),
      gdp = mean(GDP, na.rm = TRUE),
      co2 = mean(CO2, na.rm = TRUE)
    )
  
  
  # Merge with world data
  world_data <- world_data %>%
    left_join(renewable_data, by = c("admin" = "name"))
  #Add variable to store which country is clicked to dispaly nested chart
  clicked_country <- reactiveVal(NULL)
  selected_region <- reactiveVal("Main")
  
  # render maps for specific regions
  render_region_map <- function(region, highlight_top = TRUE) {
    #Set the zoom level based on regions in each tab
    #Set the zoom level for the map in main page 
    if (region == "Main") {
      data_to_plot <- world_data
      center <- c(0, 0)  
      zoom <- 1
    } else {
      data_to_plot <- world_data %>%
        filter(region_un == region)
      #Set the zoom level for the map in africa page 
      if (region == "Africa") {
        center <- c(0, 20)
        zoom <- 2.5
        #Set the zoom level for the map in asia page 
      } else if (region == "Asia") {
        center <- c(30, 100)
        zoom <- 3
        #Set the zoom level for the map in europe page 
      } else if (region == "Europe") {
        center <- c(70, 70)
        zoom <- 1.7
        #Set the zoom level for the map in america page 
      } else if (region == "Americas") {
        center <- c(10, -100)
        zoom <- 2
        #Set the zoom level for the map in oceania page 
      } else if (region == "Oceania") {
        center <- c(-20, 140)
        zoom <- 3
      }
    }
    
    # Select 5 countries based on renewable energy generation
    #If top 5 is selected 
    if (highlight_top) {
      #Select 5 countries with highest renewable energy generation
      top_countries <- data_to_plot %>%
        arrange(desc(ren)) %>%
        head(5)
    } else {
      #Otherwise select 5 countries with least renewable energy generated
      top_countries <- data_to_plot %>%
        arrange(ren) %>%
        head(5)
    }
    
    # Get centroids from shape file of top 5 countries for add marker in leaflet
    top_countries_centroids <- st_centroid(top_countries$geometry)
    
    # Convert centroids to a data frame
    centroid_coords <- data.frame(
      lng = st_coordinates(top_countries_centroids)[, 1],
      lat = st_coordinates(top_countries_centroids)[, 2],
      admin = top_countries$admin
    )
    
    #Set the label text for the map 
    weather_param <- if (input$data_type == "Temperature") {
      paste("Temperature: ",
            round(data_to_plot$average_temperature, 2),
            "°C")
    } else {
      paste("Wind Speed: ",
            round(data_to_plot$average_wind, 2),
            " m/s")
    }
    
    mytext <- paste(
      "Country: ",
      data_to_plot$admin,
      "<br/>",
      weather_param,
      "<br/>",
      "Renewable: ",
      round(data_to_plot$ren, 2),
      "%",
      "<br/>",
      "GDP: $",
      round(data_to_plot$gdp / 1e12, 2),
      " Trillion",
      "<br/>",
      "CO2 Emissions: ",
      round(data_to_plot$co2, 2),
      " Metric Tons",
      "<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    #Generate map
    leaflet(data_to_plot) %>%
      addTiles() %>%
      addPolygons(
        #Fill colour based on weather parameter
        fillColor = ~ {
          if (input$data_type == "Temperature") {
            temp_palette <- colorNumeric("YlOrRd",
                                         data_to_plot$average_temperature,
                                         na.color = "transparent")
            temp_palette(average_temperature)
          } else {
            wind_palette <- colorNumeric("RdPu", data_to_plot$average_wind, na.color = "transparent")
            wind_palette(average_wind)
          }
        },
        weight = 1,
        opacity = 1,
        color = ~ "black",
        dashArray = ~ "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = ~ admin
      ) %>%
      #Add markers only for top5 or bottom 5 countries 
      addMarkers(
        data = centroid_coords,
        ~ lng,
        ~ lat,
        label = ~ admin,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = 'top',
          textOnly = TRUE
        )
      ) %>%
      #Set the zoom for each tab based on the region zoom values from before
      setView(lng = center[2],
              lat = center[1],
              zoom = zoom) %>%  # Set the view based on center and zoom
      addLegend(
        #Set legend based on weather type selected
        pal = colorNumeric(if (input$data_type == "Temperature") {
          "YlOrRd"
        } else{
          "RdPu"
        }, if (input$data_type == "Temperature") {
          data_to_plot$average_temperature
        } else{
          data_to_plot$average_wind
        }, na.color = "transparent"),
        values = if (input$data_type == "Temperature") {
          data_to_plot$average_temperature
        } else{
          data_to_plot$average_wind
        },
        title = if (input$data_type == "Temperature") {
          "Average Temperature"
        } else{
          "Average Wind Speed"
        },
        position = "bottomleft"
      )
  }
  
  # Render the main page
  output$main_map <- renderLeaflet({
    render_region_map("Main", input$topBottom == "Top 5")
  })
  # Render the Afric tab with only countries in africa highlighted 
  output$africa_map <- renderLeaflet({
    render_region_map("Africa")
  })
  # Render the Asia tab with only countries in africa highlighted 
  output$asia_map <- renderLeaflet({
    render_region_map("Asia")
  })
  # Render the Europe tab with only countries in africa highlighted 
  output$europe_map <- renderLeaflet({
    render_region_map("Europe")
  })
  # Render the America tab with only countries in africa highlighted 
  output$americas_map <- renderLeaflet({
    render_region_map("Americas")
  })
  # Render the Oceania tab with only countries in africa highlighted 
  output$oceania_map <- renderLeaflet({
    render_region_map("Oceania")
  })
  
  
  # Store the  tab selected
  observeEvent(input$tabs, {
    selected_tab <- input$tabs
    selected_region(selected_tab)
  })
  
  # Store the country clicked in the main page 
  observeEvent(input$main_map_shape_click, {
    clicked_country(input$main_map_shape_click$id)
  })
  # Store the country clicked in the africa page 
  observeEvent(input$africa_map_shape_click, {
    clicked_country(input$africa_map_shape_click$id)
  })
  # Store the country clicked in the asia page 
  observeEvent(input$asia_map_shape_click, {
    clicked_country(input$asia_map_shape_click$id)
  })
  # Store the country europe in the main page 
  observeEvent(input$europe_map_shape_click, {
    clicked_country(input$europe_map_shape_click$id)
  })
  # Store the country clicked in the america page 
  observeEvent(input$americas_map_shape_click, {
    clicked_country(input$americas_map_shape_click$id)
  })
  # Store the country clicked in the oceania page 
  observeEvent(input$oceania_map_shape_click, {
    clicked_country(input$oceania_map_shape_click$id)
  })
  
  
  
  # Render the nested chart
  render_nested_chart <- function(region) {
    #Idnetify which country has been clicked
    country <- clicked_country()
    #Return ull if no country clicked
    if (is.null(country) ||
        selected_region() != region)
      return(NULL)
    #Read the country csv
    country_info <- read.csv("country.csv")
    #Find the maximum values of CO2,GDP AND renewable energy for scaling
    max_values <- country_info %>%
      summarise(
        max_renewable_energy = max(renewable, na.rm = TRUE),
        max_co2_emissions = max(CO2, na.rm = TRUE),
        max_gdp = max(GDP, na.rm = TRUE)
      )
    #Filter out the countries
    country_data <- country_info %>%
      filter(name == country)
    #Find the average of the values over 2018 -2019
    GDP_avg <- sum(country_data$GDP) / 2
    RE_avg <- sum(country_data$renewable) / 2
    CO2_avg <- sum(country_data$CO2) / 2
    plot_data <- data.frame(
      parameter = c("Renewable Energy", "CO2 Emissions", "GDP"),
      value = c(RE_avg, CO2_avg, GDP_avg)
    )
    
    # Calculate scaling factors for CO2 and GDP
    plot_data <- plot_data %>%
      mutate(
        scaled_value = case_when(
          #Renewable enrgy is already out of 100%
          parameter == "Renewable Energy" ~ value / max_values$max_renewable_energy * 100,
          #For CO2 multiply into 1000, 100 or 10000 based on the range the data falls in to get it as a percentage of 100
          parameter == "CO2 Emissions" ~ case_when(
            value / max_values$max_co2_emissions > 0.01 &
              value / max_values$max_co2_emissions < 0.1   ~ value / max_values$max_co2_emissions * 1000,
            value / max_values$max_co2_emissions > 0.1 &
              value / max_values$max_co2_emissions < 1 ~ value / max_values$max_co2_emissions * 100,
            TRUE ~ value / max_values$max_co2_emissions * 10000
          ),
          #For GDP multiply into 1000, 100 or 10000 based on the range the data falls in to get it as a percentage of 100
          
          parameter == "GDP" ~ case_when(
            value / max_values$max_gdp > 0.01 &
              value / max_values$max_gdp < 0.1 ~ value / max_values$max_gdp * 1000,
            value / max_values$max_gdp > 0.1 &
              value / max_values$max_gdp < 1 ~ value / max_values$max_gdp * 100,
            TRUE ~ value / max_values$max_gdp * 10000
          )
        )
      )
    
    # Set the  positions for squares so that they are inside each other
    plot_data <- plot_data %>%
      #This is to ensure the bigger box is created first and then the smaller ones
      arrange(desc(scaled_value)) %>%
      mutate(
        x_min = 0,
        x_max = scaled_value,
        y_min = 0,
        y_max = scaled_value
      )
    
    # Plot the nested area chart
    ggplot(plot_data) +
      #Create the square boxes
      geom_rect(
        aes(
          xmin = x_min,
          xmax = x_max,
          ymin = y_min,
          ymax = y_max,
          fill = parameter
        ),
        color = "black"
      ) +
      #Set the text so that it moves when the box resizes
      geom_text(aes(
        x = (x_min + scaled_value) / 2,
        y = y_min + scaled_value - 5,
        label = parameter
      ),
      color = "black") +
      coord_fixed() +
      theme_void() +
      scale_fill_manual(
        values = c(
          "Renewable Energy" = "#098A24",
          "CO2 Emissions" = "#ECF00B",
          "GDP" = "#C117B2"
        )
      ) +
      labs(title = paste("Parameters for", country))
  }
  
  # Render the nested chart for each region
  output$nested_chart_africa <- renderPlot({
    render_nested_chart("Africa")
  })
  
  output$nested_chart_asia <- renderPlot({
    render_nested_chart("Asia")
  })
  
  output$nested_chart_europe <- renderPlot({
    render_nested_chart("Europe")
  })
  
  output$nested_chart_americas <- renderPlot({
    render_nested_chart("Americas")
  })
  
  output$nested_chart_oceania <- renderPlot({
    render_nested_chart("Oceania")
  })
  
  #Render the GDP animation
  render_gdp_animation <- function() {
    # Calculate the average GDP for the top 5 and bottom 5 countries based on renewable energy generation
    top_5_countries <- world_data %>%
      arrange(desc(ren)) %>%
      head(5)
    
    bottom_5_countries <- world_data %>%
      arrange(ren) %>%
      head(5)
    
    # Read csv with GDP and CO2 information
    country_info <- read.csv("country.csv")
    
    # Calculate average GDP for top 5 countries
    top_5_avg_gdp <- country_info %>%
      #filter countries that are in the top 5 
      filter(name %in% top_5_countries$admin) %>%
      #find the average GDP 
      summarise(avg_gdp = mean(GDP, na.rm = TRUE))
    
    # Calculate average GDP for bottom 5 countries
    bottom_5_avg_gdp <- country_info %>%
      #filter countries that are in the bottom 5 
      filter(name %in% bottom_5_countries$admin) %>%
      #find the average GDP 
      summarise(avg_gdp = mean(GDP, na.rm = TRUE))
    
    # craete a data frame to store the plotting data since we want animation and it requires groups
    plot_data <- data.frame(
      group = c(rep("Top 5", nrow(top_5_avg_gdp)), rep("Bottom 5", nrow(
        bottom_5_avg_gdp
      ))),
      avg_gdp = c(top_5_avg_gdp$avg_gdp, bottom_5_avg_gdp$avg_gdp),
      #set the states for animation
      state = c(rep("Top 5", nrow(top_5_avg_gdp)), rep("Bottom 5", nrow(
        bottom_5_avg_gdp
      )))
    )
    #Plot the data 
    plot <- ggplot(plot_data, aes(x = group, y = avg_gdp, fill = group)) +
      #create columns
      geom_col() +
      scale_fill_manual(values = c("Top 5" = "#C117B2", "Bottom 5" = "#C117B2")) +
      theme_minimal() +
       guides(fill = FALSE) +
      labs(title = "Average GDP of Top 10 and Bottom 10 Countries", x = "Group", y = "Average GDP") +
      #set the transition for animation
      transition_states(state,
                        transition_length = 2,
                        state_length = 1) +
      shadow_mark() +
      enter_grow() +
      enter_fade()
    #call the animate function 
    animate(
      plot,
      #set the time it should take for animation to complete
      duration = 4,
      fps = 20,
      renderer = gifski_renderer(
        loop = FALSE,
        width = 400,
        height = 400
      )
    )
  }
  
  # Display plot when button is clicked
  observeEvent(input$gdp, {
    output$gdp_animation <- renderImage({
      animate_gif <- tempfile(fileext = ".gif")
      anim <- render_gdp_animation()
      #Save the animation in file 
      anim_save(animate_gif, anim)
      list(src = animate_gif, contentType = 'image/gif')
    }, deleteFile = TRUE)
  })
  
  
  #render animation plot for co2
  render_co2_animation <- function() {
    # Calculate the average CO2 for the top 5 and bottom 5 countries based on renewable energy generation
    top_5_countries <- world_data %>%
      arrange(desc(ren)) %>%
      head(5)
    
    bottom_5_countries <- world_data %>%
      arrange(ren) %>%
      head(5)
    
    # Read the csv with co2 information
    country_info <- read.csv("country.csv")
    
    # Calculate average CO2 for top 5 countries
    top_5_avg_co2 <- country_info %>%
      filter(name %in% top_5_countries$admin) %>%
      summarise(avg_co2 = mean(CO2, na.rm = TRUE))
    
    # Calculate average CO2 for bottom 5 countries
    bottom_5_avg_co2 <- country_info %>%
      filter(name %in% bottom_5_countries$admin) %>%
      summarise(avg_co2 = mean(CO2, na.rm = TRUE))
    
    # Ccreate data fram
    plot_data <- data.frame(
      group = c(rep("Top 5", nrow(top_5_avg_co2)), rep("Bottom 5", nrow(
        bottom_5_avg_co2
      ))),
      avg_co2 = c(top_5_avg_co2$avg_co2, bottom_5_avg_co2$avg_co2),
      #set the states for transition
      state = c(rep("Top 5", nrow(top_5_avg_co2)), rep("Bottom 5", nrow(
        bottom_5_avg_co2
      )))
    )
    
    # Create plot for animation
    plot <- ggplot(plot_data, aes(x = group, y = avg_co2, fill = group)) +
      geom_col() +
      scale_fill_manual(values = c(
        "Top 5" = "#ECF00B",
        "Bottom 5" = "#ECF00B"
      )) +
      theme_minimal()  +
      guides(fill = FALSE) +
      labs(title = "Average CO2 Emissions of Top 5 and Bottom 5 Countries", x = "Group", y = "Average CO2 Emissions") +
      #Set transitionn length for animation
      transition_states(state,
                        transition_length = 2,
                        state_length = 1) +
      shadow_mark() +
      enter_grow() +
      enter_fade()
    #Generate animation
    animate(
      plot,
      duration = 4,
      fps = 10,
      renderer = gifski_renderer(
        loop = FALSE,
        width = 400,
        height = 400
      )
    )
  }
  
  # Genrate animation only when CO2 button is pressed 
  observeEvent(input$co, {
    output$ren_animation <- renderImage({
      animate_gif <- tempfile(fileext = ".gif")
      anim <- render_co2_animation()
      #Save the animation as an image
      anim_save(animate_gif, anim)
      list(src = animate_gif, contentType = 'image/gif')
    }, deleteFile = TRUE)
  })
  
  
  # render the stacked area chart for a region
  render_stacked_area_chart <- function(region) {
    #filter out only regions that are their in my data set
    data_region <- world_data %>%
      filter(region_un == region)
    
    plot_data <- data_region %>%
      select(admin, average_temperature, ren, average_wind) %>%
      #Convert data into long form for plotting
      pivot_longer(
        cols = c(average_temperature, ren, average_wind),
        names_to = "parameter",
        values_to = "value"
      )
    #select parameter for the chart based on weather type selected
    plot_data <- plot_data %>%
      filter(parameter %in% c(
        "ren",
        ifelse(
          input$data_type == "Temperature",
          "average_temperature",
          "average_wind"
        )
      ))
    
    ggplot(plot_data,
           aes(
             #set country as x axis
             x = admin,
             #set weather parameter and renewable energy values as y axis
             y = value,
             fill = parameter,
             group = parameter
           )) +
      #Create an area chart 
      geom_area(position = "stack", alpha = 0.5) +
      scale_fill_manual(
        name = "Parameter",
        values = c(
          "average_temperature" = "#F0650B",
          "ren" = "#098A24",
          "average_wind" = "#A717C1"
        ),
        labels = c(
          "average_temperature" = "Average Temperature",
          "ren" = "Renewable Energy",
          "average_wind" = "Average Wind Speed"
        )
        
      ) +
      theme_minimal() +
      labs(title = paste("Stacked Area Chart"),
           x = "Country",
           y = "Value") + theme(axis.text.x = element_text(
             angle = 90,
             vjust = 0.5,
             hjust = 1
           ))
    
  }
  
  # create the stacked area chart for  africa countries
  output$stacked_area_chart_africa <- renderPlot({
    render_stacked_area_chart("Africa")
  })
  # create the stacked area chart for asia countries
  output$stacked_area_chart_asia <- renderPlot({
    render_stacked_area_chart("Asia")
  })
  # create the stacked area chart for europ countries
  output$stacked_area_chart_europe <- renderPlot({
    render_stacked_area_chart("Europe")
  })
  # create the stacked area chart for america countries
  output$stacked_area_chart_americas <- renderPlot({
    render_stacked_area_chart("Americas")
  })
  # create the stacked area chart for oceania region
  
  output$stacked_area_chart_oceania <- renderPlot({
    render_stacked_area_chart("Oceania")
  })
  #Display url for data sources
  url <- a("country-by-country data collection since 1960", href="https://www.kaggle.com/datasets/tirant/country-by-country-data-collection-since-1960")
  output$tab <- renderUI({
    tagList("Data Source A:", url)
  })
  url2 <- a("Historical weather data of 194 country capitals", href="https://www.kaggle.com/datasets/balabaskar/historical-weather-data-of-all-country-capitals")
  output$tab2 <- renderUI({
    tagList("Data Source B:", url2)
  })
})
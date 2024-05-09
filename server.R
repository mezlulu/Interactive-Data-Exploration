list.of.packages <- c("ggplot2", 
                      "DT",
                      "shiny",
                      "plotly",
                      "dplyr",
                      "purrr",
                      "tidyverse",
                      "sf",
                      "leaflet",
                      "mosaic")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# load all these
lapply(list.of.packages, require, character.only = TRUE)

acs_data_final <- readRDS("data/acs_data.rds")

acs_data_final <- acs_data_final

us_states <- st_read("data/tl_rd22_us_state.shp")
usstate_acs <- us_states %>%
  left_join(acs_data_final, by = "GEOID")

bbox <- st_as_sfc(st_bbox(c(xmin = -130, xmax = -60, ymin = 25, ymax = 50), 
                          crs = st_crs(usstate_acs))) # Define the bounding box

usstate_acs_filtered <- usstate_acs[st_intersects(usstate_acs, bbox, sparse = FALSE), ]

variable_descriptions <- list(
  median_age = "Median age of the population.",
  per_capita_income = "Per Capita Income in the Past 12 Months (In 2021 Inflation-Adjusted Dollars)" ,
  bachelor_degree_percentage = "Educational Attainment for the Population 25 Years and Over (Bachelor's Degree). It is calculated by dividing the number of individuals aged 25 and over who have a bachelor's degree by the total number of individuals in the same age group. It focuses on the proportion of the population aged 25 years and older who have attained a bachelor's degree.",
  employed_percentage = "Employed Portion of the Civilian Labor Force for the Population 16 Years and Over / Employment Status for the Population 16 Years and Over.  It is calculated by dividing the number of employed individuals by the total number in the labor force. Refers to the calculation of the employment rate within a specified population.",
  poverty_percentage = "Poverty Status in the Past 12 Months by Sex by Age (Income below Poverty Level) / Poverty Status in the Past 12 Months by Sex by Age (Total). This rate is determined by dividing the number of individuals below the poverty level by the total number of individuals, with both counts disaggregated by sex and age.",
  owner_occupied_percentage = "Tenure (Owner Occupied Housing Units) / Tenure (Total Housing Units). is calculated by dividing the number of owner-occupied housing units by the total number of housing units. Calculates the proportion of all housing units which are occupied by their owners ",
  own_child_percentage = "Households with the owner's children under 18 years present. / Total Households. Dividing the number of households where the children living in the home are the biological, step, or adopted children of the homeowner, and these children are under the age of 18 by the total number of households in the area being studied",
  median_earnings = "!Median earnings in the past 12 months (in 2021 inflation-adjusted dollars)",
  college_enrollment_percentage = "School Enrollment by Level of School for the Population 3 Years and Over (Enrolled in College, Undergraduate Years) / School Enrollment by Level of School for the Population 3 Years and Over (Total). refers to the calculation of the percentage of the population (aged 3 years and over) enrolled in college at the undergraduate level, compared to the total school-enrolled population of the same age range.",
  family_household_percentage = "Household Type by Household Size (Family Households) / Household Type by Household Size (Total Households), indicates a measure of the proportion of households classified as family households in relation to the total number of households, with consideration given to household size."
)

# Extract GEOID from usstate_acs_filtered
filtered_geo_ids <- usstate_acs_filtered$GEOID

# Filter acs_data_final to make its rows consistent with usstate_acs_filtered
acs_data_final_filtered <- acs_data_final %>%
  filter(GEOID %in% filtered_geo_ids)

acs_data_final_filtered1 <-acs_data_final_filtered %>%
  filter(!NAME == "District of Columbia")

acs_data_num <- acs_data_final_filtered1[,-c(1,2)]
data_scaled <- scale(acs_data_num)

precompute_kmeans <- function(data_scaled, max_k = 8) {
  set.seed(123)
  lapply(1:max_k, function(k) {
    kmeans(data_scaled, centers = k, nstart = 50)
  })
}

k_means_list <- precompute_kmeans(data_scaled)

# Server Logic
server <- function(input, output) {

    # Variable description
  output$variableDescription <- renderUI({
    req(input$variable)
    selected_description <- variable_descriptions[[input$variable]]
    HTML(paste0("<p><strong>Description:</strong> ", selected_description, "</p>"))
  })
  
  # Histogram
  output$histPlot <- renderPlotly({
    req(input$variable)
    ggplot(acs_data_final, aes_string(x = input$variable)) +
      geom_histogram(bins = 30) +
      labs(
        title = paste("Histogram of", input$variable),
        x = input$variable,
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # Boxplot
  output$boxPlot <- renderPlotly({
    req(input$variable)
    ggplot(acs_data_final, aes_string(y = input$variable)) +
      geom_boxplot() +
      labs(
        title = paste("Boxplot of", input$variable),
        x = "",
        y = input$variable
      ) +
      theme_minimal()
  })

  
  # Summary Table
  output$summaryTable <- DT::renderDataTable({
    req(input$variable)
    variable_data <- acs_data_final[[input$variable]] # Extract the column based on input
    summary_data <- favstats(variable_data, na.rm = TRUE) # Compute favstats
    
    # Construct the summary table
    summary_data <- data.frame(
      Min = summary_data$min,
      Q1 = summary_data$Q1,
      Median = summary_data$median,
      Q3 = summary_data$Q3,
      Max = summary_data$max,
      Mean = summary_data$mean,
      SD = summary_data$sd,
      Missing = sum(is.na(variable_data))
    )
    
    DT::datatable(summary_data)
  })
  
  
  # Map for spatial distribution of the variables
  output$mapPlot <- renderLeaflet({
    req(input$variable)
    
    # Transform us_states to WGS84 CRS
    us_states_transformed <- st_transform(us_states, crs = st_crs(4326))
    
    # Join us_states with acs_data_final
    map_data <- left_join(us_states_transformed, acs_data_final, by = "GEOID")
    
    # Create a color palette function
    color_pal <- colorNumeric(palette = "YlOrRd", domain = NULL, na.color = "transparent")
    
    # Generate the map
    leaflet(map_data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(fillColor = ~color_pal(map_data[[input$variable]]),
                  fillOpacity = 0.7,
                  color = "#232323",
                  weight = 1) %>%
      addLegend(pal = color_pal, values = ~map_data[[input$variable]], title = input$variable) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)
  })
  
  # server logic for unsupervised learning
  
  output$plotElbow <- renderPlotly({
    wss_values <- sapply(k_means_list, function(km) km$tot.withinss)
    df_elbow <- data.frame(k = 1:length(wss_values), wss = wss_values)
    p <- ggplot(df_elbow, aes(x = k, y = wss)) +
      geom_point() +
      geom_line() +
      labs(x = "Number of Clusters", y = "Total Within Sum of Squares") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Reactive expression for k-means results
  k_means_results_reactive <- reactive({
    req(input$kInput)
    set.seed(123)
    acs_data_final_filtered1 <-acs_data_final_filtered %>%
      filter(!NAME == "District of Columbia")
    
    acs_data_num <- acs_data_final_filtered1[,-c(1,2)]
    data_scaled <- scale(acs_data_num)
    kmeans(data_scaled, centers = input$kInput, nstart = 50)
  })
  
  output$plotKMeans <- renderPlot({
    k_means_results <- k_means_results_reactive()
    acs_data_final_filtered1$cluster <- k_means_results$cluster
    
    usstate_acs <- us_states %>%
      left_join(acs_data_final_filtered1, by = "GEOID")  
    
    bbox <- st_as_sfc(st_bbox(c(xmin = -130, xmax = -60, ymin = 25, ymax = 50), 
                              crs = st_crs(usstate_acs))) # Define the bounding box
    
    usstate_acs_filtered <- usstate_acs[st_intersects(usstate_acs, bbox, sparse = FALSE), ]
    
    ggplot(data = usstate_acs_filtered) +
      geom_sf(aes(fill = factor(cluster)), color = "black", fillOpacity = 0.5) +  
      geom_sf_label(aes(label = NAME.x), size = 3, check_overlap = TRUE) +
      scale_fill_viridis_d() +  
      labs(
        title = "K-Means Clustering of States",
        fill = "Cluster",
        x = "Longitude",
        y = "Latitude"
        ) +
      coord_sf(expand = FALSE)
  })
  
   output$clusterTable <- DT::renderDataTable({
     req(input$kInput)
     k_means_results <- k_means_results_reactive()
     cluster_sizes <- table(k_means_results$cluster)
     data.frame(Cluster = names(cluster_sizes), Size = as.numeric(cluster_sizes))
   })
  
  output$summaryCluster <- DT::renderDataTable({
    set.seed(123)
    req(input$kInput)
    k_means_results <- k_means_results_reactive()
    variable <- c("median_age", "per_capita_income", "bachelor_degree_percentage", "employed_percentage", "poverty_percentage", "owner_occupied_percentage",  "own_child_percentage", "median_earnings", "college_enrollment_percentage", "family_household_percentage")
    acs_data_final1 <- acs_data_final_filtered1[,-1]
    acs_data_final1$cluster <- k_means_results$cluster

    #   Summarizing the data
    stats_table <- acs_data_final1 %>%
      group_by(cluster) %>%
      summarise(across(all_of(variable), ~paste0(round(mean(., na.rm = TRUE), 2), " (", round(sd(., na.rm = TRUE), 2), ")"))) %>%
      ungroup()
    DT::datatable(stats_table, options = list(scrollX = TRUE, scrollY = TRUE))
  })
}

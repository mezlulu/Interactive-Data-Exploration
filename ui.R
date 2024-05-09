library(shiny)
library(plotly)
library(shinydashboard)
library(sf)
library(leaflet)
library(DT)
library(mosaic)

acs_data_final <- readRDS("data/acs_data.rds")
us_states <- st_read("data/tl_rd22_us_state.shp")

ui <- dashboardPage(
  dashboardHeader(title = "Data Exploration App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Variables", tabName = "variables", icon = icon("dashboard")),
      menuItem("Unsupervised Learning", tabName = "unsupervised_learning", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # Variables Tab
      tabItem(tabName = "variables",
              fluidRow(
                column(4,
                       h3("Dataset Overview"),
                       p("This application explores data from the American Community Survey (ACS), specifically from the 2021 ACS 1-year estimates. The data includes a range of demographic and socio-economic variables for each U.S. state."),
                       p("Variables in the dataset include:"),
                       tags$ul(
                         tags$li("Median age of the total population(B01002)"),
                         tags$li("Per capita income(B19301)"),
                         tags$li("Bachelor's degree attainment percentage(B15003)"),
                         tags$li("Employment percentage(B23025)"),
                         tags$li("Poverty percentage(B17001)"),
                         tags$li("Homeownership rates(B25003)"),
                         tags$li("Own child percentage(B23010)"),
                         tags$li("Median earnings(B20002)"),
                         tags$li("College enrollment percentage(B14001)"),
                         tags$li("Family household percentage(B11016)")
                       ),
                       h3("Interactive Data Exploration"),
                       p("Use the controls in this panel to explore the data and visualize different aspects of the ACS data."),
                       selectInput("variable", "Select Variable:", choices = names(acs_data_final)[-c(1, 2)]),
                       uiOutput("variableDescription")
                )
              ),
              fluidRow(
                column(6, plotlyOutput("histPlot")),
                column(6, plotlyOutput("boxPlot")),
                column(12, DT::dataTableOutput("summaryTable"))
              ),
              fluidRow(
                column(12, h3("Geographical Data Visualization")),
                column(12, p("The map below presents a geographical overview of the selected variable across different states, providing a spatial context to the data.")),
                column(12, leafletOutput("mapPlot"))
              )
      ),
      # Unsupervised Learning Tab
      tabItem(tabName = "unsupervised_learning",
              # Add your text summary here
              h3("K-means Clustering"),
              p("The elbow plot and k-means clustering plots provide a visual representation of the clustering analysis. The number of clusters (k) can be adjusted using the slider."),
              
              plotlyOutput("plotElbow"),
              
              p("The map showing the individual US states colored according to their assigned cluster."),
              
              fluidRow(
                column(4,
                       selectInput("kInput", "Number of Clusters (k):",
                                   choices = 1:5, selected = 2)),
                column(8, plotOutput("plotKMeans", height = "600px", width = "100%"))
              ),
              p("Cluster 1: This cluster appears to include a mix of states from different regions,  including the West (California, Oregon), 
                the central (Colorado, Utah, Kansas), the South (Texas), and the Northeast (New Jersey, New York"),
              p("Cluster 2: This cluster consists of states primarily located in the midwest and southeast parts of the country."),
              
              h3("Cluster Analysis Summary"),
              p("The tables below show the size for each cluster."),
              
              DTOutput("clusterTable"),
              
              p("The tables below show the detailed statistics for each cluster. We can interpret the properties of these clusters by comparing the characteristic 
                    means and standard deviations of each cluster."),
              DTOutput("summaryCluster"),
              
              p("This table shows the mean (Mean) and standard deviation (SD, Standard Deviation) 
                    of the features of the two clusters obtained by K-means clustering. The clusters
                    are labeled 1, 2, and the sample size (N) for each cluster is also given."),
              p("In summary, Cluster 1 is generally wealthier, more educated, and has a higher 
                employment rate compared to Cluster 2, which has a slightly older population, 
                lower income and education levels, higher poverty rates, but a relatively higher 
                rate of homeownership. Both clusters have similar rates of college enrollment and 
                family-based households."),
              h3("Conclusion"),
              p("Cluster 1 states could be seen as economically vibrant with a highly skilled workforce, 
                while Cluster 2 states might focus on policy interventions to improve economic conditions, 
                enhance job opportunities, and support educational programs.")
      )
    )
  )
) 

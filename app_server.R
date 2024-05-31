# server.R
library(dplyr)
library(shiny)
library(bslib)
library(tidyverse)
library(dplyr)
library(scales)
source("app_ui.R")
# read in population data
population <- read.csv(url("https://raw.githubusercontent.com/cestastanford/historical-us-city-populations/master/data/1790-2010_MASTER.csv"))
population <- population %>% filter(X2010 > 0) %>% filter(X1900 > 0)

# reading in region/state mapping
region_state <- read.csv("https://raw.githubusercontent.com/info-201b-sp24/exploratory-analysis-boyan4975/main/data/region-state.csv")

# reading city information and filter out duplicate
city_info <- read.csv("https://raw.githubusercontent.com/info-201b-sp24/exploratory-analysis-boyan4975/main/data/city_info.csv") %>% distinct(Name)
city_id <- read.csv("https://raw.githubusercontent.com/info-201b-sp24/exploratory-analysis-boyan4975/main/data/city_info.csv") %>%
  select("Name", "ID") %>% distinct(Name, ID)

# filter out cities not in the list of cities that has weather information
population <- population %>% inner_join(city_info, by = c("City" = "Name")) %>% drop_na(ID)

# match population with region
population_with_region <- left_join(population, region_state, by = c("ST" = "State"))

# calculate population change between 1900 and 2010 and add two columns for population change and
# population change percentage
population_with_region <- population_with_region %>% 
  mutate(Population_Change = X2010 -X1900, Population_Change_Percentage = round(Population_Change * 100 / X1900, digits=0), .after = CityST) %>% group_by(Region)

# read in climate dataset, calculate average temperature and total precipitatioin change of each city from 1900 to 2010
weather <- read.csv("https://raw.githubusercontent.com/info-201b-sp24/exploratory-analysis-boyan4975/main/data/climate.csv") %>%
  inner_join(city_id, by = "ID") %>% relocate(Name, .after = ID) %>% select(-"ID") %>% 
  arrange(Name) %>% group_by(Name) %>% filter(!is.na(Avg.Temp) & !is.na(ToT.Prcp) & Year >= 1900) %>% filter(Year == first(Year) | Year == 2010) %>%
  mutate(Temp_Change = round(Avg.Temp - lag(Avg.Temp), digits=2), Prcp_Change = round(ToT.Prcp -lag(ToT.Prcp), digits = 2)) %>%
  drop_na(Temp_Change) %>% drop_na(Prcp_Change)

# Joint the weather df and population df, sort by region then by Population_Change_Percentage
population_climate_1900_2010 <- population_with_region %>% left_join(weather, by = c("City" = "Name"))  %>%
  relocate(Temp_Change, Prcp_Change, .after = Population_Change_Percentage) %>% select(c("City", "ST", "Region", "Population_Change", "Population_Change_Percentage", "Temp_Change", "Prcp_Change", "LAT", "LON")) %>%
  ungroup() %>% arrange(desc(Region), -Population_Change_Percentage)

# Define server function
server <- function(input, output) {
  output$boxplot1 <- renderPlot({
    ggplot(population_climate_1900_2010, aes(x = ST, y = Population_Change_Percentage, fill = Temp_Change)) +
      geom_boxplot(outlier.shape = 8, outlier.color = "red", outlier.size = 2) + 
      geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  
      facet_wrap(~Region, scales = "free_x") + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(min(population_climate_1900_2010$Temp_Change), max(population_climate_1900_2010$Temp_Change)), name = "Temp Change") +
      labs(title = "Population Change Percentage and Temperature Change by State and Region (1900-2010)",
           subtitle = "Analysis of population dynamics with respect to historical weather changes",
           x = "State",
           y = "Population Change Percentage") +
      theme_tufte(base_size = 14) +  
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
            strip.text.x = element_text(size = 10, face = "bold"))
  })
  
  output$tlat_chart <- renderPlot({
    ggplot(population_climate_1900_2010, aes(x = LAT, y = Temp_Change)) +
      geom_point(aes(color = Region), alpha = 0.6) +
      labs(title = "Temperature Change vs. Latitude",
           x = "Latitude",
           y = "Temperature Change (°C)") +
      theme_minimal()
  })
  
  output$plat_chart <- renderPlot({
    ggplot(population_climate_1900_2010, aes(x = LAT, y = Prcp_Change)) +
      geom_point(aes(color = Region), alpha = 0.6) +
      labs(title = "Precipitation Change vs. Latitude",
           x = "Latitude",
           y = "Precipitation Change (mm)") +
      theme_minimal()
  })
  
  output$tlon_chart <- renderPlot({
    ggplot(population_climate_1900_2010, aes(x = LON, y = Temp_Change)) +
      geom_point(aes(color = Region), alpha = 0.6) +
      labs(title = "Temperature Change vs. Longitude",
           x = "Longitude",
           y = "Temperature Change (°C)") +
      theme_minimal()
  })
  
  output$plon_chart <- renderPlot({
    ggplot(population_climate_1900_2010, aes(x = LON, y = Prcp_Change)) +
      geom_point(aes(color = Region), alpha = 0.6) +
      labs(title = "Precipitation Change vs. Longitude",
           x = "Longitude",
           y = "Precipitation Change (mm)") +
      theme_minimal()
  })
  
  output$chart_3 <- renderPlotly({
    Phonie_dataset <- read.csv("./Phoenix.csv")
    
    Phoniex_plot <- ggplot(data = Phonie_dataset) +
      geom_line(mapping = aes(x = Year, y = Avg.Temp)) +
      ggtitle("Average Temperature Change for Phoenix")
    
    Astoria_CSV <- read.csv('./Astoria.csv');
    
    Astoria_plot <- ggplot(data = Astoria_CSV) +
      geom_line(mapping = aes(x = Year, y = Avg.Temp), color = input$color_input) +
      ggtitle("Average Temperature Change for Astoria")
    
    return(Astoria_plot)
  })
}

shinyApp(ui = ui, server = server)

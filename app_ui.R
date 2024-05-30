library(shiny)
library(bslib)
library(plotly)
library(tidyverse)
library(dplyr)
library(scales)
library(ggthemes)
page_one <- tabPanel(
  "Introduction",
  titlePanel("Page 1")
)

page_two <- tabPanel(
  "Second page",
  titlePanel("page 2")
)

page_three <- tabPanel(
  "Third Page",
  titlePanel("Page 3"), 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "color_input",
        label = "Choose a color for the graph" ,
        choices = c("Red", "Blue", "Orange", "Green", "Black", "White", "Purple", "Brown")
      )
    ),
    mainPanel(
      p("This graph shows the temperature change in Astoria, Oregon from the 
      1920s to 2010. The graph displaying the average temperature of Astoria 
      reveals significantly lower temperatures compared to other cities in the 
      United States. The average temperature in Astoria ranges from 45 to 55 
      degrees. Although there is a dip at some point, the temperature has generally
      remained within this range.

The graph below is interactive, allowing you to change the colors to white, red, orange, 
black, etc. You can also zoom into the graph to see different data points in more detail. 
For example, you can zoom into the 1920s to closely examine the temperature changes during 
that period. Additionally, you can zoom into a specific year for a more detailed view.
"),
      plotlyOutput(outputId = "chart_3")
    )
  )
)

page_four <- tabPanel(
  "Fourth Page",
  titlePanel("page 4")
)

page_five <- tabPanel(
  "Conclusion",
  titlePanel("Page 5")
)

ui<-navbarPage(
  "Analysis of Relationship between Human Population and Climate Change",
  page_one,
  page_two,
  page_three,
  page_four,
  page_five
)

library(shiny)
library(bslib)

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
  titlePanel("Page 3")
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


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
#  headerPanel("Old Faithful Geyser Data"),
  headerPanel("Multivariate Normal Data"),
  
  # Sidebar with a slider input for number of samples
  sidebarPanel(
    sliderInput("n1",
                "Number in group 1:",
                min = 1,
                max = 200,
                value = 100),
    sliderInput("n2",
                "Number in group 2:",
                min = 1,
                max = 200,
                value = 100),
    sliderInput("meanX1n1",
                "Mean X1, group 1:",
                min = -100,
                max = 100,
                value = 1),
    sliderInput("meanX2n1",
                "Mean X2, group 1:",
                min = -100,
                max = 100,
                value = 1),
    sliderInput("varX1n1",
                "Var X1, group 1:",
                min = 0,
                max = 100,
                value = 1),
    sliderInput("varX2n1",
                "Var X2, group 1:",
                min = 0,
                max = 100,
                value = 1),
    sliderInput("corn1",
                "Correlation, group 1:",
                min = 0,
                max = 1,
                value = 0),
    sliderInput("meanX1n2",
                "Mean X1, group 2:",
                min = -100,
                max = 100,
                value = -1),
    sliderInput("meanX2n2",
                "Mean X2, group 2:",
                min = -100,
                max = 100,
                value = -1),
    sliderInput("varX1n2",
                "Var X1, group 2:",
                min = 0,
                max = 100,
                value = 2),
    sliderInput("varX2n2",
                "Var X2, group 2:",
                min = 0,
                max = 100,
                value = 2),
    sliderInput("corn2",
                "Correlation, group 2:",
                min = 0,
                max = 1,
                value = 0)
  ),
#  sidebarPanel(
#    sliderInput("n0",
#                "Number in group 1:",
#                min = 1,
#                max = 98,
#                value = 50),
#    sliderInput("n1",
#                "Number in group 2:",
#                min = 1,
#                max = 104,
#                value = 50),
#    sliderInput("xx",
#                "eruptions offset:",
#                min = 0,
#                max = 20,
#                value = 10),
#    sliderInput("yy",
#                "waiting offset:",
#                min = 0,
#                max = 28,
#                value = 10)
#  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot", height = "800px")
  )
))

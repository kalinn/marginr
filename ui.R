
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
                "Number in group 1 (control):",
                min = 10,
                max = 300,
                value = 150,
                step = 10),
    sliderInput("p",
                "Number of noise features:",
                min = 0,
                max = 5,
                value = 0,
                step = 1),
    sliderInput("varX1n1",
                "Var X1, group 1:",
                min = 0.25,
                max = 10,
                value = 1,
                step = 0.25),
    sliderInput("varX2n1",
                "Var X2, group 1:",
                min = 0.25,
                max = 10,
                value = 1,
                step = 0.25),
    sliderInput("varX1n2",
                "Var X1, group 2:",
                min = 0.25,
                max = 10,
                value = 5,
                step = 0.25),
    sliderInput("varX2n2",
                "Var X2, group 2:",
                min = 0.25,
                max = 10,
                value = 5,
                step = 0.25),
    sliderInput("corn1",
                "Correlation, group 1:",
                min = -1,
                max = 1,
                value = -0.2,
                step = 0.1),
    sliderInput("corn2",
                "Correlation, group 2:",
                min = -1,
                max = 1,
                value = -0.6,
                step = 0.1),
    sliderInput("meanX1n2",
                "Mean X1, group 2:",
                min = -10,
                max = 10,
                value = -3,
                step = 0.25),
    sliderInput("meanX2n2",
                "Mean X2, group 2:",
                min = -10,
                max = 10,
                value = 1,
                step = 0.25)
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
    plotOutput("distPlot", height = "800px", width="400px")
  )
))

library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Departure and Arrival Delays and Taxi times in US Airports for Jan 2018"),
    fluidRow(
      column(12,
             # Text input for picking airport
             uiOutput("Chose_Airport")
      )
    ),
    fluidRow(
      column(12,
             # Plot output
             plotOutput("distOut", height = "600px", width = "1100px")
      )
    )
  )
)

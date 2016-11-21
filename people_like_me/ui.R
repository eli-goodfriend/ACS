library(shiny)
source("~/Dropbox/Code/ACS/people_like_me/possibleCategories.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("America the Beautifully Diverse"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        'colname', 'Category:', choices = possible.categories,
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      selectizeInput(
        'value', 'Value:', choices = possible.values,
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    
    # might want conditional panels to make new choices available?
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

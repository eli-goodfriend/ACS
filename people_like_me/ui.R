library(shiny)
source("~/Dropbox/Code/ACS/people_like_me/possibleCategories.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("America the Beautifully Diverse"),

  sidebarLayout(
    # sidebar with a dropdown list for the data category of interest
    sidebarPanel(
      selectizeInput(
        'colname', 'Category:', choices = possible.categories,
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      # if the data category is a factor, make another dropdown of possible values
      conditionalPanel(condition = "output.isFactor == 1",
        selectizeInput(
          'valueFac', 'Value:', choices = possibleValues,
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
            )
        )
      ),
      # if the data category is not a factor, it's numeric; pick cutoff value
      conditionalPanel(condition = "output.isFactor == 0",
        numericInput(
          'valueNum', 'Bigger than:', 0 # also min, max, step, width
          )
      )
      # if the data category is a number, make a text entry box for a number
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

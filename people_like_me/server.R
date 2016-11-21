library(shiny)
source("~/Dropbox/Code/ACS/people_like_me/plotter.R") # also opens db
source("~/Dropbox/Code/ACS/people_like_me/possibleCategories.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, clientData, session) {
  
  # update the input boxes
  getVals <- reactive({
    colname <- input$colname
    new.possible.values <- getPossibleValues(colname)
    return(new.possible.values)
  })
  
  observe({
    updateSelectizeInput(session,"value",
                         label = "Value:",
                         choices = getVals())
  }) # TODO why does this only work once?!
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({

    if (nchar(input$value[1])<1){return()}
    
    criteriaIn <- paste(input$colname,"==",input$value, sep=".")
    likeMe(criteriaIn)
    # x    <- faithful[, 2]  # Old Faithful Geyser data
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    # # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #      main = paste(input$colname, input$value))
  })
})

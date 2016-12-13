library(shiny)
source("~/Dropbox/Code/ACS/people_like_me/plotter.R") # also opens db
source("~/Dropbox/Code/ACS/people_like_me/possibleCategories.R")

# Define server logic required to draw a plot of the USA
shinyServer(function(input, output, clientData, session) {
  
  # choose the type of input for the value of the category
  output$isFactor <- reactive({
    colname <- input$colname
    return(getIsFactor(colname))
  })
  
  outputOptions(output, "isFactor", suspendWhenHidden=FALSE)
  
  # update the input boxes
  getVals <- reactive({
    colname <- input$colname
    new.possible.values <- getPossibleValues(colname)
    return(new.possible.values)
  })
  
  observe({
    updateSelectizeInput(session,"valueFac",
                         label = "Value:",
                         choices = getVals())
  }) # TODO why does this only work once?!
     # it does work multiple times! it just takes a long time because
     # it tries to plot in between :/
  
  # Expression that generates a plot. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    isFactor <- getIsFactor(input$colname)
   
    if (nchar(input$colname[1])<1){return()} # haven't picked a category yet

    criteriaIn <- c()
    if (isFactor==1 & nchar(input$valueFac[1])>0){
      criteriaIn <- c(criteriaIn, 
                      paste(input$colname,"==",input$valueFac, sep="`"))
    } else if (isFactor==0 & (input$valueLL < input$valueUL)) { 
      criteriaIn <- c(criteriaIn,
                      paste(input$colname,">=",input$valueLL, sep="`"),
                      paste(input$colname,"<" ,input$valueUL, sep="`"))
    } else {
      return() # don't have a value input yet
    }
    
    likeMe(criteriaIn)

  })
})

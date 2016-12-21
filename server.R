options(rgl.useNULL=TRUE)
if (!require("devtools")){
  install.packages("devtools")
  library("devtools")
}
if (!require("shiny")){
  install.packages("shiny")
  library("shiny")
}
if (!require("rgl")){
  install.packages("rgl")
  library("rgl")
}
if (!require("rglwidget")){
  install.packages("rglwidget")
  library("rglwidget")
}
if (!require("shinyRGL")){
  install.packages("shinyRGL")
  library("shinyRGL")
}
if (!require("reshape2")){
  install.packages("reshape2")
  library("reshape2")
}
if (!require("RColorBrewer")){
  install.packages("RColorBrewer")
  library("RColorBrewer")
}
if (!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}
############# CODE FOR THE IRIS DATA ################################
data(iris)
dfiris <- iris
dfiris $ Petal.Area = dfiris$Petal.Width * dfiris$Petal.Length

data(diamonds)
data("mpg")

####BEGINNING OF SHINY CODE ###########
shinyServer(function(input, output){
  
  #############2D Tab#########
  
  # This function renders the regression plot
  output$plot2d <- renderPlot({
      # Grab whatever the chosen data is,
      plotData <- getData()
      
      # And as long as it's not null,
      if (!is.null(plotData)) {
        
        # Check to make sure there isn't too much data
        response <- plotData[, input$responseName]
        if (length(response) > 500 & !input$pointLimitOverride) {
          stop("Your response variable contains over 500 values. If you want to plot it anyway check Override Point Limit. Note: this can result in instabilities with the App.")
        }
        
        # Plot a simple regression plot, or
        if(input$method == "lm"){
          # Check if our data can be used for a linear model
          if(sum(is.na(as.numeric(gsub("\\%|\\,|\\$*", "", as.character(response))))) > 0) {
            stop("The selected response variable is not numeric.")
          } else {
            # Generate our model
            formChar <- paste(input$responseName, "~", input$explanatoryName)
            model <- lm(as.formula(formChar), data = plotData)
            ggplotRegression(model)
          }
        } else if (input$method == "categorical") {
          categorical <- plotData[input$categoricalName]
          if (nrow(unique(categorical)) > 10 & !input$categoryOverride) {
            stop("Your categorical variable has more than 10 categories, if you want to plot it anyway check Override Category Limit. Note: this can result in instabilities with the App.")
          }
          ggplotCategory()
        }
      }
  })
  
  # This function renders the residual analysis plot
  output$res2d <- renderPlot({
    # Grab the chosen data,
    plotData <- getData()
    
    # If the data exists,
    if (!is.null(plotData) & (input$method == "lm" | input$method == "categorical")) {
      # Check to make sure there isn't too much data
      response <- plotData[, input$responseName]
      if (length(response) > 500 & !input$pointLimitOverride) {
        stop("Your response variable contains over 500 values. If you want to plot it anyway check Override Point Limit. Note: this can result in instabilities with the App.")
      }
      
      # Make the plot
        formChar <- paste(input$responseName, "~", input$explanatoryName)
        model <- lm(as.formula(formChar), data = plotData)
        g <- ggplot(data = model, aes(.fitted, .resid) )
        g + geom_point() +
          stat_smooth(method = "lm", level = 0) + geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
          xlab("Fitted values") + ylab("Residuals") + 
          ggtitle("Residuals vs. Fitted Values")
    }
    })
  
  # Tis function plots the Q-Q plot
  output$qq2d <- renderPlot({
    # Grab the data,
    plotData <- getData()
    # If it exists,
    if (!is.null(plotData) & (input$method == "lm" | input$method == "categorical")) {
      # Check to make sure there isn't too much data
      response <- plotData[, input$responseName]
      if (length(response) > 500 & !input$pointLimitOverride) {
        stop("Your response variable contains over 500 values. If you want to plot it anyway check Override Point Limit. Note: this can result in instabilities with the App.")
      }
      
      # Make the normality plot
      formChar <- paste(input$responseName, "~", input$explanatoryName)
      model <- lm(as.formula(formChar), data = plotData)
      ggQQ(model)
    }
  })

  # This function creates a Q-Q plot based on a linear model
  # We found this on the web.
  ggQQ <- function(LM) # argument: a linear model
  {
    y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
    p <- ggplot(LM, aes(sample=.resid)) +
      stat_qq(alpha = 0.5) +
      geom_abline(slope = slope, intercept = int, color="blue") +
      xlab("Theoretical Quantiles") + ylab("Standardized Residuals") + 
      ggtitle("Normal Q-Q")
    
    return(p)
  }
  
  # This function plots the active data based on the given model
  ggplotScatter <- function() {
    plotData <- getData()
    title = paste("Plot of", input$responseName, "vs.", input$explanatoryName)
    g <- ggplot(data = plotData, aes_string(x = input$explanatoryName, y = input$responseName))
    g + geom_point() + ggtitle(title)
  }  

  # This function plots the active data based on the given model
  ggplotSmoothMethod <- function(method) {
    plotData <- getData()
    title = paste(method, "Plot of", input$responseName, "vs.", input$explanatoryName)
    g <- ggplot(data = plotData, aes_string(x = input$explanatoryName, y = input$responseName))
    g + geom_point() + stat_smooth(method = method) + ggtitle(title)
  }  
  
  # This function returns a linear regression plot based on a linear model
  # We found this on the web, and have modified it.
  ggplotRegression <- function (fit) {
    plotData <- getData()
    
    # Build the title
    title <- ""
    if (!is.null(summary(fit)$adj.r.squared)) {
      title <- paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5), title)
    }
    if (!is.null(fit$coef[[1]])) {
      title <- paste(title, "Intercept =",signif(fit$coef[[1]],5 ))
    }
    if (!is.null(fit$coef[[2]])) {
      title <- paste(title, " Slope =",signif(fit$coef[[2]], 5))
    }
    if (!is.null(summary(fit)$coef[2,4])) {
      title <- paste(title, " P =",signif(summary(fit)$coef[2,4], 5))
    }
    
    if (input$ci) {
      level <- as.numeric(input$ciLevel)
    } else {
      level <- 0.0
    }
    
    g <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
      geom_point() +
      stat_smooth(aes(col = "Linear Fit"), method = "lm", level = level) +
      labs(title = title)
    
    if (input$loess) {
      g <- g + geom_smooth(aes(col = "Loess"), method = "loess", level = level)
    }
    
    g
  }

  # Generate a ggplot that colors by category
  ggplotCategory <- function() {
    plotData <- getData()
    
    if (input$ci) {
      level <- as.numeric(input$ciLevel)
    } else {
      level <- 0.0
    }
    
    # Make sure that the caregory is a factor
    category <- plotData[, input$categoricalName]
    plotData[, input$categoricalName] <- factor(category)
    
    # Calculate the interaction variable
    formChar <- paste(input$responseName, "~", input$explanatoryName, "*", input$categoricalName)
    model <- lm(as.formula(formChar), data = plotData)
    anovaTable <- anova(model)
    print(anovaTable)
    fVal <- anovaTable[3, 4]
    pVal <- anovaTable[3, 5]
    dfInt <- anovaTable[3, 1]
    dfRes <- anovaTable[4, 1]
    title <- paste("F-Interact:", round(fVal, 3), "Df-Interact:", dfInt, "Df-Residuals:", dfRes,"P-Interact(>F):", signif(pVal, 3))
    
    g <- ggplot(data = plotData, aes_string(x = input$explanatoryName, y = input$responseName, color = input$categoricalName))
    g <- g + geom_point() + stat_smooth(method = "lm", level = level) + ggtitle(title)
    
    if (input$loess) {
      g <- g + geom_smooth(aes(col = "Loess"), method = "loess", level = level)
    }
    
    g
  }
  
  # This function grabs whatever data is active in the shinyApp
  getData <- function() {
    plotData <- NULL
    
    if (input$dataset1 == "iris"){
      plotData <- iris
    } else if (input$dataset1 == "diamonds") {
      plotData <- diamonds
    } else if (input$dataset1 == "mpg") {
      plotData <- mpg
    }
    else if (input$dataset1 == "upload") {
      if (!is.null(input$file)) {
        plotData <- read.csv(input$file$datapath, header = input$header, sep = input$sep, quote = input$quote)
      }
    }
    
    plotData
  }
  
  # This function generates a dropdown menu based on the variable names of the active data
  output$explanatoryNames<-renderUI({
    plotData <- getData()

    selectInput("explanatoryName", "Select Explanatory Variable", choices=names(plotData), selected=names(plotData)[1])
  })
  
  # This function generates a dropdown menu based on the variable names of the active data
  output$responseNames<-renderUI({
    plotData <- getData()
    
    selectInput("responseName", "Select Response Variable", choices=names(plotData), selected=names(plotData)[2])
  })
  
  # This function generates a dropdown menu based on the variable names of the active data
  output$categoricalNames<-renderUI({
    plotData <- getData()
    
    selectInput("categoricalName", "Select Categorical Variable", choices=names(plotData), selected=names(plotData)[3])
  })
})
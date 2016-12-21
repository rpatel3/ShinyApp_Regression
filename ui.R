# ------------------------------------------------
#  App Title: Regression Visualization  
#     Author: Keenan M. Reimer, Ruchita Patel, Riya Makhijani, Rowan Kardos
# ------------------------------------------------
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

# Here starts the Shiny app
shinyUI(navbarPage("Regression Visualization",
                   # Here starts the tabs next to the page title at the top. Currently there are only one
                   tabPanel("2D",
                            sidebarLayout(
                              # Here starts the sidebar on the left hand side of the page
                              sidebarPanel(selectInput("dataset1",label = "Select a dataset", choices = c("Iris"= "iris",
                                                                                                          "Diamonds" = "diamonds",
                                                                                                          "MPG" = "mpg",
                                                                                                          "Custom" = "upload")),
                                           # This coniditional pannel only shows if upload is selected
                                           conditionalPanel(condition = "input.dataset1 === 'upload'",
                                                            fileInput("file", "Browse for a file",
                                                                      accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                                            strong("File Format"),
                                                            checkboxInput("header", "Header", TRUE),
                                                            radioButtons("sep", "Separator:",
                                                                         c(Comma=",",Semicolon=";",Tab="\t"), ","),
                                                            radioButtons("quote", "Quote:",
                                                                         c(None="","Double Quote"='"',"Single Quote"="'"), "")
                                           ),
                                           
                                           # Dropdown selector for x
                                           uiOutput("explanatoryNames"),
                                           # Dropdown selector for y
                                           uiOutput("responseNames"),
                                           # Dropdown selector for categorical variable
                                           conditionalPanel(condition = "input.method === 'categorical'", 
                                                            uiOutput("categoricalNames")),
                                           
                                           # Here starts the regression parameters section, which only shows if the regression tab is active
                                           conditionalPanel(condition = "input.tab === 'Regression'", 
                                                            strong("Regression Method"),
                                                            
                                                            radioButtons("method",label = "",
                                                                         choices = c("Linear Model" = "lm", 
                                                                                     "Interaction" = "categorical")),
                                                            strong("Regression Options"),
                                                            checkboxInput("loess", label = "Loess Line", value = TRUE),
                                                            checkboxInput("ci", label = "Confidence Interval", value = TRUE),
                                                            conditionalPanel(condition = "input.ci",
                                                                             textInput("ciLevel", label = "Confidence Interval Level", value = "0.95")),
                                                            conditionalPanel(condition = "input.method === 'categorical'", 
                                                                             checkboxInput("categoryOverride", label = "Override Category Limit", value = FALSE))),
                                           checkboxInput("pointLimitOverride", label = "Override Point Limit", value = FALSE),
                                           div("Shiny app by", 
                                               a(href="https://www.linkedin.com/in/keenan-reimer-3321a483",target="_blank", 
                                                 "Keenan M. Reimer,"),
                                               a(href="https://www.linkedin.com/in/ruchita21",target="_blank", 
                                                 "Ruchita Patel,"),
                                               a(href="https://www.linkedin.com/in/riya-makhijani-026167102",target="_blank", 
                                                 "Riya Makhijani,"),
                                               a(href="https://www.linkedin.com/in/rowan-kardos-b3233b50",target="_blank", 
                                                                       "Rowan Kardos"), align="right", style = "font-size: 8pt"),
                                           
                                           div("Layout Inspired by", 
                                               a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank", 
                                                 "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),
                                           
                                           div("Shiny source files:",
                                               a(href="https://drive.google.com/open?id=0B4V_0jp9DSsKVjNHVDRySXQ4REE",
                                                 target="_blank","Google Drive"),align="right", style = "font-size: 8pt"),
                                           
                                           div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
                                                 "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
                              ),
                              # Here starts the area for the plotting
                              mainPanel(
                                tabsetPanel( id = "tab",
                                  # The main plot panel
                                  tabPanel("Regression",
                                           plotOutput("plot2d")
                                           ),
                                  # The residual analysis plot panel
                                  tabPanel("Residual vs. Predicted", plotOutput("res2d")),
                                  # The normality plot panel
                                  tabPanel("Normality of Residuals", plotOutput("qq2d"))
                                )
                              )
                            )
                   )
  ))

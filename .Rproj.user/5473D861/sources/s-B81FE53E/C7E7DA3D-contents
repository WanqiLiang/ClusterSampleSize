


library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)


source("global.R")


# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("united"),
  title = "Sample size calculator",
  id = "tabs",
  tabPanel(title = "ADEPT Design", value = "ADEPT", 
           
           
           useShinydashboard(),
           
           fluidRow(
             column(width = 4,
                    
                    radioButtons("opt", 
                                 "Options: Are you interested in finding sample size or power?", 
                                 c("Sample size" = 1, "Power" = 2), 
                                 1, 
                                 inline = TRUE),
                    conditionalPanel(
                      condition = "input.opt == 1",
                      numericInput("beta", "beta: The type 2 error (1 - power)",0.2, min = 0.01, max = 0.99, width = "100%"),
                      
                    ),       
                    
                    conditionalPanel(
                      condition = "input.opt == 2",
                      numericInput("N", "N: The total number of clusters",10, min = 1, step = 1, width = "100%"),
                      
                    ),
                    
                    
                    numericInput("alpha", "alpha: The type 1 error", 0.05, min = 0.01, max = 0.99, width = "100%"),
                    numericInput("m", "m:  minimum cluster size",10, min = 1, width = "100%"),
                    numericInput("delta", "delta: The standardized effect size",0.282, width = "100%"),
                    
                    numericInput("ICC", "ICC: The inter cluster correlation",0.01, width = "100%"),
                    
                    numericInput("p_1", "P(R = 1 | A_1 = 1):",0.2, min = 0.01, max = 0.99, width = "100%"),
                    
                    numericInput("Cor_Y_X", "Cor_Y_X: Correlation between Y and the cluster level covariate X. If no cluster level covariance, please enter 0.",
                                 0.2, min = 0.01, step = 0.99, width = "100%"),
                    
                    actionBttn("Calculate", label = "Calculate",
                               color = "success",
                               style = "pill",
                               icon = icon("sliders"),
                               block = TRUE),
                    br(),
                    textOutput("result")
             ),
             column(width = 8,
                    h2("Sample size calculator for ADEPT design"),
                    img(src='schema.png', align = "center",height="80%", width="80%"),
                    
             )
           )
           
           
           
           
           
  ),
  

  tabPanel(title ="Prototypical Design", value = "prototypical", 
           
           
           useShinydashboard(),
           
           fluidRow(
             column(width = 4,
                    
                    radioButtons("opt2", 
                                 "Options: Are you interested in finding sample size or power?", 
                                 c("Sample size" = 1, "Power" = 2), 
                                 1, 
                                 inline = TRUE),
                    conditionalPanel(
                      condition = "input.opt2 == 1",
                      numericInput("b2", "beta: The type 2 error (1 - power)",0.2, min = 0.01, max = 0.99, width = "100%"),
                      
                    ),       
                    
                    conditionalPanel(
                      condition = "input.opt2 == 2",
                      numericInput("N2", "N: The total number of clusters",10, min = 1, step = 1, width = "100%"),
                      
                    ),
                    
                    numericInput("alpha2", "alpha: The type 1 error", 0.05, min = 0.01, max = 0.99, width = "100%"),
                    numericInput("m2", "m:  minimum cluster size",10, min = 1, width = "100%"),
                    numericInput("delta2", "delta: The standardized effect size",0.282, width = "100%"),
                    
                    numericInput("ICC2", "ICC: The inter cluster correlation",0.01, width = "100%"),
                    
                    numericInput("p_12", "P(R = 1 | A_1 = 1):",0.2, min = 0.01, max = 0.99, width = "100%"),
                    
                    numericInput("p_neg12", "P(R = 1 | A_1 = -1):",0.3, min = 0.01, max = 0.99, width = "100%"),
                    
                    numericInput("Cor_Y_X2", "Cor_Y_X: Correlation between Y and the cluster level covariate X. If no cluster level covariance, please enter 0.",
                                 0.2, min = 0.01, step = 0.99, width = "100%"),
                    
                    actionBttn("Prototypical", label = "Calculate Prototypical",
                               color = "success",
                               style = "pill",
                               icon = icon("sliders"),
                               block = TRUE),
                    br(),
                    textOutput("result2")
             ),
             column(width = 8, 
                    h2("Sample size calculator for prototypical SMART design"),
                    img(src='prototypical_SMART_design.png', align = "center",height="80%", width="80%"),
             )
           )
  ),
  
  tabPanel("About",
           
           h2("Title here"),
           p("This is content.")
  )
  
  
)




server <- function(input, output, session) {
  
  res <- eventReactive(input$Calculate, {
    
    if(input$opt == 1) {
      
      
      ADEPT_sample_size(N = NULL, beta = input$beta, alpha = input$alpha, m = input$m, 
                        delta = input$delta, 
                        ICC = input$ICC,
                        p_1 = input$p_1, Cor_Y_X = input$Cor_Y_X)
      
      
    }else {
      
      ADEPT_sample_size(N = input$N, beta = NULL, alpha = input$alpha, m = input$m, 
                        delta = input$delta, 
                        ICC = input$ICC,
                        p_1 = input$p_1, Cor_Y_X = input$Cor_Y_X)
      
    }
    
  })
  output$result <- renderText({
    res()$info
  })
  
  output$result_power <- renderValueBox({
    
    valueBox(
      res()$power,  "Power", icon = icon("star"),
      color = "teal"
    )
  })
  
  output$result_sample_size <- renderValueBox({
    
    valueBox(
      res()$sample_size,  "Sample Size", icon = icon("star"),
      color = "teal"
    )
  })
  res2 <- eventReactive(input$Prototypical, {
    
    if(input$opt2 == 1) {
      
      
      prototypical_sample_size(N = NULL, beta = input$b2, alpha = input$alpha2, m = input$m2, 
                               delta = input$delta2, 
                               ICC = input$ICC2,
                               p_1 = input$p_12, p_neg1= input$p_neg12, Cor_Y_X = input$Cor_Y_X2)
      
      
      
    }else {
      
      prototypical_sample_size(N = input$N2, beta = NULL, alpha = input$alpha2, m = input$m2, 
                               delta = input$delta2, 
                               ICC = input$ICC2,
                               p_1 = input$p_12,  p_neg1= input$p_neg12, Cor_Y_X = input$Cor_Y_X2)
      
    }
    
  })
  
  output$result2 <- renderText({
    res2()$info
  })
  
  output$result_power2 <- renderValueBox({
    
    valueBox(
      res2()$power,  "Power", icon = icon("star"),
      color = "teal"
    )
  })
  
  output$result_sample_size2 <- renderValueBox({
    
    valueBox(
      res2()$sample_size,  "Sample Size", icon = icon("star"),
      color = "teal"
    )
  })
  
}

  
 

# Run the application 
shinyApp(ui = ui, server = server)
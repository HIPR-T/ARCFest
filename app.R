### ArcFest circa 20-11-2020 ###
### access_equity_trade ###

options(scipen = 999)

library(shiny)
library(ggplot2)
library(cowplot)


# Data prep

eqdf <- data.frame( "ccg" = c("CCG1", "CCG2"),
            "preval_depress_pc" = c(20,10), 
            "waiting_months" = c(3,6),
            "preval_reduction" = c(0.9,0.9),
            "waiting_reduction"= c(0.9,0.9),
            "total_budget" = c(1000000,1000000),
            "share" = c(0.18,0.82)
)

allocdf <- data.frame( "CCG" = c("CCG 1", "CCG 2"),
                       "Prevalence of depression %" = NA, 
                       "Waiting times - months" = NA)

allocdf$CCG <- factor(allocdf$CCG)
allocdf$`Prevalence of depression %` <- eqdf$preval_depress_pc * (eqdf$preval_reduction ^ ((eqdf$total_budget * eqdf$share)/100000))
allocdf$`Waiting times - months` <- eqdf$waiting_months * (eqdf$waiting_reduction ^ ((eqdf$total_budget * eqdf$share)/100000))

# Shiny ###############################

ui_model <- fluidPage(
  
  titlePanel( "Health Access Equity Trade Exercise", windowTitle = "ARCFest WorkShop"),
  
  # comments etc here
  tags$div(
    HTML("ARCFest WorkShop</br>","University of Liverpool</br>")
    ),
  
  br(), # white line - seperator
  
  # plotOutput_P('plot_prev'),
  # plotOutput_W('plot_wait'),
  
  # tags$head(
  #   tags$style(type="text/css",
  #              "label{ display: table-cell; text-align: left; vertical-align: middle; } .form-group { display: table-row;}",
  #              "label { font-size:90%; }") #margin-top: -85px;
  # ),

  # tags$head(
  #   tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
  # ),

  tags$head(
    tags$style("label { font-size:80%; }")
  ),
  
  sidebarLayout(
    
  sidebarPanel(
  # fluidRow(
    
    wellPanel(width = 6,
      h5(em("CCG Share")),
      h6("Suppose we have a budget of £1 million to distribute between two areas. Change the slider bar below to see how adjusting the share affects differences in access and outcomes."),
      # tags$div(HTML("Share going to CCG 1 (%) :")),
      sliderInput("share1", "Share going to CCG 1 (%) :",
                  min = 0, max = 100, value = 18 ),
      # tags$div(HTML("Share going to CCG 2 (%) :")),
      sliderInput("share2", "Share going to CCG 2 (%) :",
                  min = 0, max = 100, value = 82)
    ),
    
    
    wellPanel(width = 6,
                 h5(em("Baseline CCG Charactersitics")),
                 fluidRow(

                 column(6,
                        h6("Prevalence of depression (%)"),
                        
                        # tags$div(HTML("CCG 1")),
                        numericInput("prev1", "CCG 1", 20, min = 0, max = 100, width = 90),
                        
                        # tags$div(HTML("CCG 2")),
                        numericInput("prev2", "CCG 2", 10, min = 0, max = 100, width = 90)
                 ),
                 column(6,
                        h6("Waiting times (months)"),
                        
                        # tags$div(HTML("CCG 1")),
                        numericInput("wait1", "CCG 1" , 3, min = 0, max = 24, width = 90),
                        
                        # tags$div(HTML("CCG 2")),
                        numericInput("wait2", "CCG 2", 6, min = 0, max = 24, width = 90)
                        )
                 )
                ),
                 
    wellPanel(width = 6,
                 # h5(em("Scenario")),
                 fluidRow(
                   column(6,
                          numericInput("prev_reduct", "Every £100,000 additional funding reduces prevalence by (relative):", 
                              0.90, min = 0, max = 1, step = 0.01)
                          ),
                   column(6,
                          numericInput("wait_reduct", "Every £100,000 additional funding reduces waiting time by (relative):", 
                              0.90, min = 0, max = 1, step = 0.01)
                          )
                   # column(5,
                   #        numericInput("budget", "Total additional budget (£) :" , 
                   #            1000000, min = 100000, max = 1000000, step = 100000)
                   #        )
                   )
                 )
    

  ),
  
  mainPanel(plotOutput("eq_plot"), width = 7)
  
  )
) # end fluidpage
  
server_model <- function(input, output, session) {
  
  observe({
    updateSliderInput(session, "share1", min = 0, max = 100, value = 100 - input$share2)
  })
  output$slider <- renderUI({
    sliderInput("share2", "Share going to CCG 2 (%) :", min = 0, max = 100, value = 100 - input$share1)
  })
  
  observe({
    updateSliderInput(session, "share2", min = 0, max = 100, value = 100 - input$share1)
  })
  output$slider <- renderUI({
    sliderInput("share1", "Share going to CCG 1 (%) :", min = 0, max = 100, value = 100 - input$share2)
  })
  

  output$eq_plot <- renderPlot({
    
    # update inputs
    eqdf$share <- c(input$share1, input$share2)
    
    eqdf$preval_depress_pc <- c(input$prev1, input$prev2)
    eqdf$preval_reduction <- c(input$prev_reduct, input$prev_reduct)
    
    eqdf$waiting_months <- c(input$wait1, input$wait2)
    eqdf$waiting_reduction <- c(input$wait_reduct, input$wait_reduct)
    
    # update calcs
    allocdf$`Prevalence of depression %` <- eqdf$preval_depress_pc * (eqdf$preval_reduction ^ (eqdf$total_budget * (eqdf$share/100)/100000))
    allocdf$`Waiting times - months` <- eqdf$waiting_months * (eqdf$waiting_reduction ^ (eqdf$total_budget * (eqdf$share/100)/100000))
    
    # Plots
    gg_prev <- ggplot(data = allocdf, aes(x = CCG, y = `Prevalence of depression %`)) +
      geom_bar(stat="identity", fill = "steelblue", width = 0.7) + 
      labs(x = "", y = "Prevalence of depression (%)", color = "") +
      theme_light() +
      theme(axis.text=element_text(size=12))
    
    gg_wait <- ggplot(data = allocdf, aes(x = CCG, y = `Waiting times - months`)) +
      geom_bar(stat="identity", fill = "darkcyan", width = 0.7) + 
      labs(x = "", y = "Waiting times (months)") +
      theme_light() +
      theme(axis.text=element_text(size=12))
      
    
    plot_grid(
      gg_prev, gg_wait,
      nrow = 2
    )

  }, height = 640) # renderPlot
  
} # server_model


# Run
shinyApp(ui = ui_model, server = server_model)

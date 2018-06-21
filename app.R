#
#   Project:  IWG SCC - Interactive results viewer
#   Author:   Cora Kingdon
#   Date:     June 2018
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)

# scc.results.raw = read.csv("scc_results.csv")
scc.results.raw = read.csv("FAKE_scc_results.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # shinythemes::themeSelector(),
  theme = shinytheme("cosmo"),
  
  titlePanel("IWG - Social Cost of Carbon Values"),
  
  tabsetPanel(
    type="tabs",
    tabPanel("Timeseries",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "ts_model",
                      label = "Model selection:",
                      choices = c("FUND", "DICE", "PAGE"),
                      selected = "FUND",
                      multiple = TRUE),
          
          radioButtons(inputId = "ts_discount",
                       label = "Discount rate:",
                       choices = c("2.5%" = 0.025,
                                   "3.0%" = 0.03,
                                   "5.0%" = 0.05),
                       selected = 0.03),
          
          radioButtons(inputId = "ts_scope",
                       label = "Scope of damages:",
                       choices = c("Global", "Domestic"),
                       selected = "Global"),
          
          checkboxInput(inputId = "show_errors",
                        label = "Show error bars",
                        value = FALSE)
        ),
        mainPanel(
          plotOutput("timeseries")
        )
      )
    ),
    tabPanel("Histogram",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "h_model",
                     label = "Model selection:",
                     choices = c("FUND", "DICE", "PAGE"),
                     selected = "FUND",
                     multiple = TRUE),
         
          radioButtons(inputId = "h_discount",
                      label = "Discount rate:",
                      choices = c("2.5%" = 0.025,
                                  "3.0%" = 0.03,
                                  "5.0%" = 0.05),
                      selected = 0.03),
         
          radioButtons(inputId = "h_scope",
                      label = "Scope of damages:",
                      choices = c("Global", "Domestic"),
                      selected = "Global")
       ),
       mainPanel(
         titlePanel(
           sliderInput(inputId = "histyear",
                       label = "SCC year:",
                       min = 2010,
                       max = 2050,
                       value = 2020,
                       sep="",
                       width='95%')
         ),
         plotOutput("histogram")
       )
      )
    ),
    tabPanel("Values table",
      sidebarLayout(
        sidebarPanel(
          numericInput(inputId = "dt_year",
                      label = "SCC year:",
                      min = 2010,
                      max = 2050,
                      step = 1,
                      value = 2020),
          
          radioButtons(inputId = "dt_scope",
                       label = "Scope of damages:",
                       choices = c("Global", "Domestic"),
                       selected = "Global")
        ),
        mainPanel(
          textOutput("scc_year"),
          dataTableOutput("values_table")
        )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # Tab 1: Timeseries plot
   output$timeseries <- renderPlot({
     
     # Require a model selection
     req(input$ts_model)
     
     # Filter on selected user inputs
     scc.results.selected = scc.results.raw %>%
       filter(Model %in% input$ts_model) %>%
       filter(Discount == input$ts_discount) %>%
       filter(Scope == input$ts_scope) %>%
       group_by(Model,Year,Discount,Scope) %>%
       summarise(se=sd(SCC),SCC = mean(SCC))
     
     if(input$show_errors) {
       ggplot(data=scc.results.selected, 
              aes(x=Year, y=SCC, color=Model, ymin=SCC-se, ymax=SCC+se)) + geom_point() + geom_errorbar(width=.5) + expand_limits(x=c(2010,2050), y=c(0,65))
       
     } else {
       ggplot(data=scc.results.selected, 
              aes(x=Year, y=SCC, color=Model)) + geom_point() + expand_limits(x=c(2010,2050), y=c(0,65))
       
     }
      
   })
   
   # Tab 2: Histogram plot
   output$histogram <- renderPlot({
     
     # Require model selection
     req(input$h_model)
     
     # Filter data on user inputs
     scc.hist = scc.results.raw %>%
       filter(Model %in% input$h_model) %>%
       filter(Discount == input$h_discount) %>%
       filter(Scope == input$h_scope) %>%
       filter(Year==input$histyear)
     
     bw = 1
     if(input$h_scope=="Domestic"){bw = 0.25}
     
     # Plot histogram
     ggplot(data = scc.hist, aes(x=SCC, fill=Model)) + geom_histogram(binwidth=bw) #+ expand_limits(x=c(0,65))
     
   })
   
    # Tab 3: Values data table
    output$values_table <- DT::renderDataTable({
      scc.values = scc.results.raw %>%
        filter(Scope == input$dt_scope) %>%
        filter(Year==input$dt_year) %>%
        group_by(Model, Discount) %>%
        summarise(SCC = mean(SCC)) %>%
        spread(Model, SCC)

      DT::datatable(scc.values, 
                    options=list(dom='t'),
                    rownames=FALSE) %>% 
        formatCurrency(c("FUND","DICE","PAGE"),'$') %>%
        formatPercentage("Discount",1)
      
   })
    output$scc_year <- renderText({paste("Displaying SCC values for ", input$dt_year, ":")})
}

# Run the application 
shinyApp(ui = ui, server = server)

# Deploy
# rsconnect::deployApp(".")
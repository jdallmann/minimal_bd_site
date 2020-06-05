#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(magrittr)
library(lubridate)
library(ggthemes)
library(scales)
library(shiny)

#######################
### DATA SOURCING
#######################
load("good_for_a_city-202005.RData")
tax_pct <- tax_pct %>%
    mutate(service = ifelse(service == "Road Maintenance", "Roads", service),
           service = ifelse(service == "MIsc Capital Expenditure", "Misc Capital Expenditure", service)) %>%
    group_by(service) %>%
    summarise(scaled_pct = sum(scaled_pct)) %>%
    ungroup()



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Service Breakdown 2019"),
   
   # Get the address
   sidebarLayout(
      sidebarPanel(
          selectizeInput("address", "Address:",
                      choices=NULL),
          uiOutput("a_val_str"),
          uiOutput("tax_str"),
          uiOutput("levy_str")
      ),
      
      # Show a data table of results
      mainPanel(
          plotOutput("gg_service")
      )
      # ,
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Pass values to UI selector
    updateSelectizeInput(session, 'address', 
                         choices = assessments$full_address, 
                         server = TRUE)
    
    # values <- reactiveValues(display_df = NULL)
    
    dat <- reactive({
        assessments %>%
            filter(full_address %in% input$address)
    })
    
    reactive({
    updateSelectizeInput(session, 'address', 
                         choices = dat()$full_address[1:100], 
                         server = TRUE)})
      
    
    output$raw <- renderTable({
        dat()},
        rownames = FALSE)
    
    output$a_val <- renderText({
        dat() %$%
        scales::dollar(as.numeric(assessed_value_1))})
    
    output$a_val_str <- renderUI({
        list(tags$b("Assessment Value:"), 
             tags$br(),
             tags$p(
                 dat() %$%
                 scales::dollar(as.numeric(assessed_value_1), 
                                accuracy = 1))
        )
    })
    
    output$tax_str <- renderUI({
        list(tags$b("Property Tax:"), 
             tags$br(),
             tags$p(
                 dat() %$%
                     scales::dollar(as.numeric(tax), 
                                    accuracy=1))
        )
    })
    
    output$levy_str <- renderUI({
        list(tags$b("Fronage Levy:"), 
             tags$br(),
             tags$p(
                 dat() %$%
                     scales::dollar(as.numeric(frontage_levy), 
                                    accuracy=1))
        )
    })
    
    output$gg_service <- renderPlot({
        tax_pct %>%
            mutate(service_tax = scaled_pct/100 * dat()$tax[1] + 
                       scaled_pct/100 * dat()$frontage_levy[1]) %>%
            ggplot(aes(x=reorder(service, service_tax), y = service_tax)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = scales::dollar(service_tax)), hjust = "inward") + 
            coord_flip() +
            ggthemes::theme_fivethirtyeight() +
            labs(title = "2019 PROPERTY TAX BY SERVICE")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)


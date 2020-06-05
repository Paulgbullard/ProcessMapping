#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DiagrammeR)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Pathway Mapping"),
    
    sidebarLayout(
        
        sidebarPanel(fileInput(inputId = 'file1', 
                               label = "Upload dataset",
                               multiple = FALSE,
                               ),
                     actionButton("go","Build Map")
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", grVizOutput("process_map")),
                        tabPanel("Summary")
                
            )
        )
        
    )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    event_log <- eventReactive(input$go, {
        
        dat <- readxl::read_excel(paste(input$file1$datapath),sheet = "Sheet1")
        
        dat$AppointmentDateTransformed <- as.integer(dat$AppointmentDateTransformed)
        
        dat$AppointmentDateTransformed <- as.Date(dat$`AppointmentDateTransformed`, origin = "1900-01-01")
        
        small_dat <- dat %>% 
            rename(`Professionalinvolved` = `Professional involved`)
        
        
        Event_log2 <- small_dat %>% 
            mutate(status = "complete",
                   activity_instance = 1:nrow(small_dat)) 
        
        Event_log3 <- Event_log2 %>% 
            eventlog(
                case_id = "PtReferralDateTime",
                activity_id = "SlotTypeTransformed",
                activity_instance_id = "activity_instance",
                timestamp = "AppointmentDateTransformed",
                lifecycle_id = "status",
                resource_id = "Professionalinvolved")
        
        return(Event_log3) 
        }
    )
    
    output$process_map <- renderGrViz({process_map(event_log())})
    
    }

# Run the application 
shinyApp(ui = ui, server = server)

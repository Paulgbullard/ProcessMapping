#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bupaR)
library(DiagrammeR)
library(ggplot2)
library(shinyBS)
library(shinycssloaders)
library(rsvg)
library(DiagrammeRsvg)
library(svgPanZoom)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Pathway Mapping"),
    
    sidebarLayout(
        
        sidebarPanel(fileInput(inputId = 'file1', 
                               label = "Upload dataset",
                               multiple = FALSE,
                               ),
                     
                     selectInput("colSelectCase","Select a CaseID", choices = "Upload data"),
                     bsTooltip(id = "colSelectCase",
                               title = "Select the unique patient identifier",
                               placement = "right",
                               trigger = "hover"),
        
                     selectInput("colSelectActivity", "Select an ActivityID", choices = "Upload data"),
                     bsTooltip(id = "colSelectActivity", 
                               title = "Select the appointment/activity type",
                               placement = "right",
                               trigger = "hover"),
                     
                     
                     selectInput("colSelectTimestamp", "Select a Timestamp", choices = "Upload data"),
                     bsTooltip(id = "colSelectTimestamp", 
                               title = "Select the timestamp for the appointment/activity",
                               placement = "right",
                               trigger = "hover"),
                     
                     selectInput("colSelectResourceID", "Select a ResourceID", choices = "Upload data"),
                     bsTooltip(id = "colSelectResourceID", 
                               title = "Select the resource for the appointment/activity",
                               placement = "right",
                               trigger = "hover"),
                     
                     actionButton("go","Build Map")
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", withSpinner(svgPanZoomOutput("process_map"))),
                        tabPanel("Flow", plotOutput("rel_ant"))
                
            )
        )
        
    )
   
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    dat <- eventReactive(input$file1,{readxl::read_excel(paste(input$file1$datapath),sheet = "Sheet1")})
    
    observe({
        updateSelectInput(session, "colSelectCase", choices = c("Please select...",names(dat())))
        updateSelectInput(session, "colSelectActivity", choices = c("Please select...",names(dat())))
        updateSelectInput(session,"colSelectTimestamp", choices = c("Please select...",names(dat())))
        updateSelectInput(session, "colSelectResourceID", choices = c("Please select...",names(dat())))
    })
    
    event_log <- eventReactive(input$go, {
        
        dat <- dat()
        
        dat$AppointmentDateTransformed <- as.integer(dat$AppointmentDateTransformed)
        
        dat$AppointmentDateTransformed <- as.Date(dat$`AppointmentDateTransformed`, origin = "1900-01-01")
        
        Event_log <- dat %>% 
            mutate(status = "complete",
                   activity_instance = 1:nrow(dat)) 
        
        Event_log <- Event_log %>% 
            eventlog(
                case_id = input$colSelectCase,
                activity_id = input$colSelectActivity,
                activity_instance_id = "activity_instance",
                timestamp = input$colSelectTimestamp,
                lifecycle_id = "status",
                resource_id = input$colSelectResourceID)
        
        return(Event_log) 
        }
    )
    
    output$process_map <- renderSvgPanZoom({process_map(event_log(),
                                                        type_nodes = frequency("absolute"),
                                                        type_edges = frequency("absolute"),                                                                                                              ,
                                                        render = FALSE) %>% 
            generate_dot() %>%
            grViz(width = 1000, height = 1800) %>%
            export_svg %>%
            svgPanZoom(height = 1800, controlIconsEnabled = TRUE)})
    
    output$rel_ant <- renderPlot({rel_ant <- event_log() %>% 
        precedence_matrix(type = "relative-antecedent") 
    
    ggplot(data = rel_ant, aes(x=consequent, y=antecedent, fill=rel_antecedent)) + 
        geom_tile() +
        scale_fill_gradient(low = "#33B8FF", high = "blue")+
        geom_text(aes(consequent, antecedent, label=sprintf("%1.1f%%", rel_antecedent*100)), color = "white", size = 4, ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1))
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)

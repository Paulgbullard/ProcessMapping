#todo - relative/absolute % on process map

library(shiny)
library(bupaR)
library(DiagrammeR)
library(ggplot2)
library(shinyBS)
library(shinycssloaders)
library(rsvg)
library(DiagrammeRsvg)
library(svgPanZoom)

# Define UI
ui <- navbarPage("Process Mapping Tool",
    tabPanel("Help",
             navlistPanel(
                 tabPanel("Data Preparation",
                              titlePanel("Preparing your data for processing"),
    
                                p("In order to process your data, it needs to be in a format that the tool can understand."),
                                p("When setting up your dataset, each row will need at least:"),
                                p("1. A case identifier, unique for each case e.g. a patient ID ", 
                                  br(),
                                  "2. A date or datetime stamp for each activity, e.g. arrival at 01/04/2020 08:00", 
                                  br(),
                                  "3. An activity identifier, which will be a ", strong('node'), "on the process map e.g. \'blood test\'",
                                  br(),
                                  "4. A resource identifier e.g. seen by a ", strong('nurse')),
                                p("Additionally, you may have multiple \'transactions\' for each activity, such as a check-in, started, finished, check-out. If there are timestamps for each of these, the additional optional parameters should be used:"),
                                p("5. A transactional lifecycle identifier, the type of transaction for each activity, e.g. check-in, started, finished, check-out",
                                  br(),
                                  "6. An activity instance identifier, e.g. a patient has had multiple surgeries, each with check-in and check-out datetimes, each step in each surgery should have its relevant surgery identifier"),
                                br(),
                                titlePanel("Example"),
                                p("An example dataset is shown below, with the different aspects indicated."),
                                img(src = "Patient Example.png"),      
                                list(tags$cite("Example taken from bupaR resources"), tags$a("here", href="https://www.bupar.net"))
                        ),
                 tabPanel("Using the tool",
                          titlePanel("How to use the pathway mapping tool"),
                          p("The pathway mapping tool is designed to make pathway mapping a process easy, once the data is in the format detailed on the ",strong("data processing"), "tab. To complete the pathway map, simply:"),
                          br(),
                          p("1. Upload the dataset from your PC",
                            br(),
                            "2. Assign each element to a column name in the dataset",
                            br(),
                            "3. ", em("Optional"), " - assign the two optional elements of activity instance and lifecycle ID",
                            br(),
                            "4. Click \'Build Map\' to process the data, and generate the outputs."),
                          br(),
                          img(src = "ProcessingData.png", height = 400),
                          p(em("Each number corresponds to the action described above"))
                          ),
                 tabPanel("Process Map Output",
                          titlePanel("The Process Map"),
                          p("Once the data has been uploaded and the configuration set, several outputs will be produced that look at ", 
                            strong("the process map, the flow between appointment types, and the resources used."),
                            "These can be used to explore the processes used within your system."),
                            br(),
                            p("The first output to consider is the ",
                              strong("pathway map"),
                              " which displays the movement of cases between different activities."),
                            img(src = "Process Map.png", width = 800),
                            br(),
                            p("As process maps grow, the information can be harder to discern. Therefore, it may be prudent to hide some of the more infrequent flows. This can be done at the bottom of the pathway map. Setting the value to 80%, for example, will trim the pathway map down to the most frequent activities up to the 80th percentile of events, i.e. activities that make up 80% of events."),
                            img(src = "Process Map 80.png", width = 800),
                            p(em("Setting the 'Percentile' slider to 80% changes the process map to show only the top 80% most frequent pathways")),
                            br(),
                            p("The flow betwwen the activities can be viewed as either absolute numbers (the number of cases moving between each activity), or relative (the percentage of cases moving from one activity to each other activity)."),
                            img(src = "Process Map Relative.png", width = 800),
                            p(em("Setting the 'Flow Type' to relative will show relative flow rates rather than absolute numbers"))
                          ),
                 tabPanel("Flow Output",
                          titlePanel("The Flow Map"),
                          p("The flow map shows an antecedent/consequent plot of the data entered. 
                            It shows the percentage of cases that move from one type of activity to another.
                            An example is shown below."),
                          img(src = "AntCons.png", width = 1000),
                          p(em("An example of an Antecedence/Consequence (flow) diagram")),
                          p("The diagram is read horizontally and then vertically. 
                            In the diagram above, for example, 47.4% of cases from Triage and Assessment subsequently go directly to a Blood Test, 52.5% go to X-ray, and for a small minority of 0.4% the Triage and Assessment is the last action taken."),
                          img(src = "AntConsRead.png", width = 1000),
                          p(em("How to read the flow diagram - horizontally follow the black lines for the antecendent, and vertically down the blue lines for the consequent action."))),
                 tabPanel("Resource Output")
             )),
    
    tabPanel("Mapping",
    titlePanel("Pathway Mapping"),
    
    sidebarLayout(
        
        sidebarPanel(
            fileInput(inputId = 'file1', 
                               label = "Upload dataset",
                               multiple = FALSE,
                               ),
            uiOutput("required"),
                     
            tags$hr(style="border-color: black;"),
            h3("Optional"),
            uiOutput("optional"),
                    
            actionButton("go","Build Map"),
            bsTooltip(id = "go",
                title = "Click here to generate pathway analysis",
                placement = "right",
                trigger = "hover")
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot",
                                withSpinner(svgPanZoomOutput("process_map")),
                                fluidRow(column(width = 6,  
                                                wellPanel(h4("Percentile Selection"),div(style = "font-size:15px", 
                                                              sliderInput("Percentile",
                                                                          "Select top % to show", 
                                                                          min = 0, 
                                                                          max = 100, 
                                                                          value = 100, 
                                                                          post = "%", 
                                                                          width = "100%")))),
                                fluidRow(column(width = 6,
                                                wellPanel(h4("Flow Type"),radioButtons("AbsRel",
                                                                       "Display 'Absolute' or 'Relative' flow rates",
                                                                       #choices = c("absolute", "relative"),
                                                                       selected = "absolute",
                                                                       choiceNames = c("Absolute", "Relative"),
                                                                       choiceValues = c("absolute", "relative")))))))
                                            ,
                        tabPanel("Flow", 
                                 plotOutput("rel_ant")),
                        tabPanel("Resources", plotOutput("resource"))
                
            )
        )
        
    )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    dat <- eventReactive(input$file1,{
        
        if (endsWith(input$file1$name, '.xlsx')| endsWith(input$file1$name, '.xls'))
            {readxl::read_excel(paste(input$file1$datapath),sheet = "Sheet1")}
        else
            {read.csv(input$file1$datapath)}
            
        })
    
    output$required <- renderUI(
                        tagList(
                            tipify(
                                selectInput("colSelectCase","Select a CaseID", choices = "Upload data"),
                                title = "Select the unique patient identifier",
                                placement = "right",
                                trigger = "hover"),
                            tipify(
                                selectInput("colSelectActivity", "Select an ActivityID", choices = "Upload data"),
                                title = "Select the appointment/activity type",
                                placement = "right",
                                trigger = "hover"),
                            tipify(
                                selectInput("colSelectTimestamp", "Select a Timestamp", choices = "Upload data"),
                                title = "Select the timestamp for the appointment/activity",
                                placement = "right",
                                trigger = "hover"),
                            tipify(
                                selectInput("colSelectResourceID", "Select a ResourceID", choices = "Upload data"),
                                title = "Select the resource for the appointment/activity",
                                placement = "right",
                                trigger = "hover"),
                                )
                            )
    output$optional <- renderUI(
                        tagList(
                            tipify(
                                selectInput("colSelectActivityInstance", "Select an Activity Instance ID", choices = "Upload data"),
                                title = "Select the activity instance ID for the appointment/activity",
                                placement = "right",
                                trigger = "hover"),
                            tipify(
                                selectInput("colSelectLifecycleID", "Select a Lifecycle ID", choices = "Upload data"),
                                title = "Select the lifecycle ID for the appointment/activity",
                                placement = "right",
                                trigger = "hover")
                                )
                            )
    
    observe({
        updateSelectInput(session, "colSelectCase", choices = c("Please select...",names(dat())))
        updateSelectInput(session, "colSelectActivity", choices = c("Please select...",names(dat())))
        updateSelectInput(session,"colSelectTimestamp", choices = c("Please select...",names(dat())))
        updateSelectInput(session, "colSelectResourceID", choices = c("Please select...",names(dat())))
        updateSelectInput(session, "colSelectActivityInstance", choices = c("Not Applicable",names(dat())))
        updateSelectInput(session, "colSelectLifecycleID", choices = c("Not Applicable",names(dat())))
    })
    
    event_log <- eventReactive(input$go, {
        
        dat <- dat()
        
        dat[input$colSelectTimestamp] <- as.integer(unlist(dat[input$colSelectTimestamp]))
        
        dat[input$colSelectTimestamp] <- as.Date(unlist(dat[input$colSelectTimestamp]), origin = "1900-01-01")
        
        if (input$colSelectActivityInstance == "Not Applicable") {
        
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
                resource_id = input$colSelectResourceID) }
        
        else
        {
            Event_log <- dat
            
            Event_log <- Event_log %>% 
            eventlog(
                case_id = input$colSelectCase,
                activity_id = input$colSelectActivity,
                activity_instance_id = input$colSelectActivityInstance,
                timestamp = input$colSelectTimestamp,
                lifecycle_id = input$colSelectLifecycleID,
                resource_id = input$colSelectResourceID)
        }   
        
        return(Event_log) 
        }
    )
    
    output$process_map <- 
        renderSvgPanZoom({
        event_log() %>% 
            filter_activity_frequency(percentage = input$Percentile/100) %>% 
                                                        
                                                            process_map(
                                                            type_nodes = frequency(input$AbsRel),
                                                            type_edges = frequency(input$AbsRel),
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
    
    output$resource <- renderPlot({resources <- event_log() %>% 
        resource_frequency("resource-activity")
    
    ggplot(data = resources, aes_string(x=input$colSelectActivity,
                                        y=input$colSelectResourceID,
                                        fill="relative_resource")) + 
        geom_tile() +
        scale_fill_gradient(low = "#33B8FF", high = "blue")+
        geom_text(aes(label=sprintf("%1.1f%%", relative_resource*100)), color = "white", size = 4, ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1))
    
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)

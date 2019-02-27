
header <- dashboardHeader(
  title = "CDFCP conservation prioritization 0.30",
  titleWidth = 450
)

body <- dashboardBody(tags$head(tags$style(HTML('
        .box { margin-bottom: 8px; } 
        
        [class*="col-lg-"],[class*="col-md-"],
        [class*="col-sm-"],[class*="col-xs-"]{
        padding-right:4px !important;
        padding-left:4px !important;
        }
        '))),
  
  fluidRow(
    column(width = 4,
           box(
             title = "",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "tabset1", width = NULL,height = "100%",#"600px", 
             actionButton("mrun",HTML("<h4>Run Optimization</h4>")), 
             helpText("================================="), 
             checkboxInput("MultiScen", "Run multiple scenarios"),
             conditionalPanel(
               condition = "input.MultiScen == true",  
               helpText(HTML("You can either set the scenario parameters directly in the 'Scenario List' table on the right, or upload your scenario file below.<br>
                             Make sure that your file follows the same structure as the table on the right.<br>
                             For details about the columns please refer to the tool manual.")),
               fileInput('scen_file', 'Choose scenario file to upload',
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           '.csv'))
               ),
             conditionalPanel(
               condition = "input.MultiScen == false",
               #actionButton("tree.update",HTML("Update tree community input")), 
               helpText("================================="),
               helpText(HTML("<h4><strong>Global parameters:</strong></h4>")),
               #selectInput("time", "What time period/scenario do you want to use:",
               #            c("present" = "curr")),
               #    selectInput("scale", "What planning unit size do you want to use:",
               #                c("100m" = "100",
               #                  "250m" = "250",
               #                  "500m" = "500")),
               #sliderInput("FTcutoff", label = "Cutoff value for high quality features", 
               #            min=0, max=100, value=0, step = 1),    
               #    selectInput("connectivity", "Include connectivity in the analysis:",
               #                c("No" = "no",
               #                  "Yes" = "yes")),
               selectInput("cost", "What cost metric should be used:",
                           c("Assessed land value ($)" = "dollar",
                             "Human score" = "human",
                             "Property size (area)" = "area")),
               selectInput("protected", "How to deal with protected areas:",
                           c("Locked in" = "locked",
                             "Available" = "avail"))
             ,
             
             #    numericInput("Nreps", "Number of repeats:",100,min=1),
             #    checkboxInput("IndRuns", "Generate output for individual runs"),    
             helpText("================================="),  
             helpText(HTML("<h4><strong>Property exclusions:</strong></h4>")),
             helpText(HTML("If you don't want to exclude properties simply leave values at 0")),
             sliderInput("RoadD", label = "Road density (km/km2). Marxan will only select properties with road densities smaller than cutoff.", 
                         min=0, max=20, value=0.0, step = 0.1),    
             sliderInput("AreaHa", label = "Parcel size (ha). Marxan will only select properties bigger than cutoff.", 
                         min=0, max=10, value=0, step=0.5),     
             sliderInput("AgrD", label = "Agriculture density (km2/km2). Marxan will only select properties with agricultural densities smaller than cutoff.", 
                         min=0, max=1, value=0.0, step = 0.01)#,    
             )
             
                      )
    ),
    column(width = 8,
           tabBox(
             title = "",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "toto", width = NULL,height = "100%", #"600px",
             tabPanel("Edit Target", rHandsontableOutput("hot_feat")),
             tabPanel("Scenario List", rHandsontableOutput("hot_multi",width="100%",height="500px")),
             tabPanel("Input Layers",leafletOutput("InMap",height=700)),
             tabPanel("Results + Download",
                      helpText(HTML("<h4>Result Summary Table</h4>")),
                      dataTableOutput("summary"),
                      helpText(HTML("<br>")),
                      helpText(HTML("<h4>Results download (property selection):</h4>")),
                      downloadButton("download_selfr", label = "Property selection"),
                      helpText(HTML("<br>")),
                      helpText(HTML("<h4>Results download (summary of outputs):</h4>")),
                      downloadButton("download_ssoln",label = "Results download"),
                      helpText(HTML("<br>")),
                      helpText(HTML("<h4>Download the entire cadastral fabric:</h4>")),
                      downloadButton("downloadSHP",label = "Results download")
             ),
             tabPanel("Result Map",leafletOutput("cadMap",height=700))
             
             
           ))
    )
  )


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)


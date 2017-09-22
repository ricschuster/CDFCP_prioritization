library(shiny)
library(shinyIncubator)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("CDFCP conservation prioritization v0.17.2"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
#    strong(textOutput("clickcoordsnmds")),
    #submitButton("Run Marxan")
    actionButton("mrun",HTML("<h4>Run Marxan</h4>")), 
    helpText("================================="),  
    helpText(HTML("<h4><strong>Global parameters:</strong></h4>")),
    selectInput("protected", "How to deal with protected areas:",
                c("Locked in" = "locked",
                  "Available" = "avail")),
    selectInput("connectivity", "Include connectivity in the analysis:",
                c("No" = "no",
                  "Yes" = "yes")),
    selectInput("cost", "What cost metric should be used:",
                c("Property size (area)" = "area",
                  "Assessed land value ($)" = "cost",
                  "Human score" = "human")),
    numericInput("Nreps", "Number of repeats:",100,min=1),
    checkboxInput("IndRuns", "Generate output for individual runs"),    
    helpText("================================="),  
    helpText(HTML("<h4><strong>Property exclusions:</strong></h4>")),
    helpText(HTML("If you don't want to exclude properties simply leave values at 0")),
    sliderInput("RoadD", label = "Road density (km/km2). Marxan will only select properties with road densities smaller than cutoff.", 
                min=0, max=5, value=0.0, step = 0.1),    
    sliderInput("AreaHa", label = "Parcel size (ha). Marxan will only select properties bigger than cutoff.", 
                min=0, max=10, value=0, step=0.5),     
    sliderInput("AgrD", label = "Agriculture density (km2/km2). Marxan will only select properties with agricultural densities smaller than cutoff.", 
                min=0, max=1, value=0.0, step = 0.01),    
    helpText("================================="),  
    helpText(HTML("<h4><strong>Protection Targets:</strong></h4>")),
    numericInput("Target", "Global Protection Target:",17,min=1,max=100),
    checkboxInput("LocalTar", "Set Individual Targets (Global Target will be ignored)"),
    conditionalPanel(
      condition = "input.LocalTar == true",
      numericInput("OFtarget", "Old Forest Birds Target:",17,min=0,max=100),
      numericInput("SAVtarget", "Savannah Birds Target:",17,min=0,max=100),
      numericInput("WETtarget", "Wetland Birds Target:",0,min=0,max=100),
      numericInput("SHRtarget", "Shrub Birds Target:",0,min=0,max=100),
      numericInput("HUMtarget", "Human Birds Target:",0,min=0,max=100),
      numericInput("HUM1target", "Avoid Human Birds Target:",0,min=0,max=100),
      numericInput("BETAtarget", "Bird Beta Diversity Target:",17,min=0,max=100),
      numericInput("StCtarget", "Standing Carbon Target:",17,min=0,max=100),
      numericInput("SeqCtarget", "Carbon Sequestration Potential Target:",17,min=0,max=100),
      numericInput("EOtarget", "TEM Element Occurrence Target:",17,min=0,max=100),
      numericInput("Planttarget", "Garry Oak Plant Species Target:",17,min=0,max=100),
      numericInput("SEItarget", "SEI Target (all classes get the same target):",0,min=0,max=100),
      numericInput("Areatarget", "Area Target:",17,min=0,max=100)
#      numericInput("RD_BLtarget", "HectaresBC Red and Blue Listed Target:",0,min=0,max=100),
#      numericInput("ENDEMtarget", "HectaresBC Endemic Target:",0,min=0,max=100),
#      numericInput("SARAtarget", "HectaresBC SARA Target:",0,min=0,max=100)
    ),    
    helpText("================================="),
    
    helpText(HTML("<h4><strong>Species penalty factor:</strong></h4>")),
    # Specifiy minimum calibration value
    numericInput("min", "Minimum value:",3,min=1e-20),
    # Specifiy maximum calibration value
    numericInput("max", "Maximum value:",100,min=1e-20),
    
    # Simple integer interval
    sliderInput("iterations", "Number of values:", 
                min=1, max=10, value=1),
    checkboxInput("exponential", "increase by orders of magnitide of min", TRUE),
    
    helpText("================================="),
    helpText(HTML("<h4><strong>Connectivity:</strong></h4>")),
    # Specifiy minimum calibration value
    numericInput("BLMmin", "Minimum value:",0,min=0),
    # Specifiy maximum calibration value
    numericInput("BLMmax", "Maximum value:",0,min=0),
    
    # Simple integer interval
    sliderInput("BLMiterations", "Number of values:", 
                min=1, max=10, value=1),
    checkboxInput("BLMexponential", "increase by orders of magnitide of min", TRUE),
    
    helpText("================================="),
    helpText(HTML("<h4><strong>Number of iterations:</strong></h4>")),
    numericInput("ITmin", "Minimum value:",100000,min=1),
    # Specifiy maximum calibration value
    numericInput("ITmax", "Maximum value:",10000000,min=1),
    
    # Simple integer interval
    sliderInput("ITiterations", "Number of values:", 
                min=1, max=10, value=1),
    checkboxInput("ITexponential", "increase by orders of magnitide of min", FALSE)    

    
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    #progressInit(),
    h3(textOutput("caption")),

#    plotOutput("cfdPlot"),
    tabsetPanel(
      tabPanel("Cost (SPF)", plotOutput("cfdPlot")), 
      tabPanel("Connectivity", plotOutput("BLMPlot")),
      tabPanel("Solution Score (# Iterations)", plotOutput("niterPlot")), 
#      tabPanel("BLM click", plotOutput("BLMClick",clickId="clicknmds")),
      tabPanel("Download",
               helpText(HTML("<h4>Download the entire cadastral fabric:</h4>")),
               downloadButton("downloadSHP", label = "CDFCP Shapefile"),
               helpText(HTML("<br>")), 
               helpText(HTML("<h4>Marxan results download (summed solutions and best solution):</h4>")),
               downloadButton("download_ssoln",label = "Summary atribute table"),
               helpText(HTML("<br>")), 
               helpText(HTML("<h4>Marxan results download (individual runs):</h4>")),
               helpText(HTML("For most users this download will not be necessary and the summed solutions and/or best solution would be sufficient")),
               downloadButton("download_detail",label = "Individual run attribute table"),
               helpText(HTML("<br>"))
               ),
      tabPanel("Map",leafletOutput("cadMap"))
    ),
    
    uiOutput("tabsets")
  )
))

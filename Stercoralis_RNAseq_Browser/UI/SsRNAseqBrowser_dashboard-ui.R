
# Header ----
header <- dashboardHeader(title = "Browser Mode")

# Sidebar ----
# Sidebar ----
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("by Gene(s)", tabName = "genebrowser", icon = icon("fas fa-dna")),
        menuItem("by Life Stage", tabName = "lifestagebrowser", icon = icon("fas fa-recycle"))
    )
)
# Body ----
body <- dashboardBody(
    tabItems(
        ## Browse by Gene Tab ----
        tabItem(tabName = "genebrowser",
                fluidRow(
                    shinydashboard::box(
                        ## User Input Options ----
                        title = tagList(shiny::icon("fas fa-sliders-h"), "Step 1: Input Genes"),
                        width = 4,
                        status = "warning",
                        
                        ### GeneID (text box)
                        textAreaInput('idtext',
                                      'Gene Stable IDs (comma separated)',
                                      rows = 10, 
                                      resize = "vertical"),
                        
                        ### Upload list of GeneIDs
                        fileInput('loadfile',
                                  'Gene Stable ID List (.csv)',
                                  multiple = FALSE),
                        
                        ### Action Button
                        actionButton('goGene',
                                     'Submit',
                                     width = '40%',
                                     icon = icon("paper-plane"))
                    ),
                    
                    # Reactive Outputs ----
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-chart-bar"),
                                        "Gene Expression Across Life Stages"), 
                        width = 8,
                        status = "warning",
                        side = "right",
                        withSpinner(plotOutput('CPM'))
                    ),
                    
                    
                ),
                fluidRow(
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-sliders-h"), "Step 2: Pick Life Stage Comparisons"),
                        width = 4,
                        status = "success",
                        
                        # Select Target Life Stage
                        h4('Option A: Select Single Pairwise Comparison'),
                    
                        pickerInput("selectTarget_GW",
                                    "Select One or More Targets",
                                    choices = list("FLF",
                                                   "PF",
                                                   "iL3",
                                                   "iL3a",
                                                   "ppL1",
                                                   "ppL3",
                                                   "pfL1"),
                                    #selected = "iL3",
                                    multiple = TRUE),
                        # Select Contrast Life Stage
                        pickerInput("selectContrast_GW",
                                    "Select One or More Contrasts",
                                    choices = list("FLF",
                                                   "PF",
                                                   "iL3",
                                                   "iL3a",
                                                   "ppL1",
                                                   "ppL3",
                                                   "pfL1"),
                                    #selected = "FLF",
                                    multiple = TRUE),
                        tags$br(),
                        tags$hr(style="border-color: black;"),
                        tags$br(),
                        
                        h4('Option B: Manually Input Multiple Pairwise Comparisons'),
                        
                        # Text Input for Multiple Contrasts
                        textAreaInput('multiContrasts',
                                      label = ('Input At Least Two Comma-Separated Pairwise Comparisons'),
                                      placeholder = ('e.g. iL3-FLF, iL3-PF, (iL3+iL3a)-(PF+FLF)'),
                                      rows = 5, 
                                      resize = "vertical"),
                        p('Note: using this option will automatically control for multiple testing.'),
                        
                        ### Action Button
                        actionButton('goGeneLS',
                                     'Submit',
                                     width = '40%',
                                     icon = icon("paper-plane"))
                        ),
                    
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-table"),
                                        "Pairwise Differential Gene Expression"), 
                        width = 8,
                        status = "success",
                        uiOutput('contrastDisplaySelection'),
                        h4("Volcano Plot"),
                        withSpinner(plotOutput('volcano')),
                        tags$br(),
                        tags$hr(style="border-color: black;"),
                        tags$br(),
                        h4("DEG Table"),
                        withSpinner(DTOutput('highlight.df'))
                    )
                ),
                # fluidRow(
                #     shinydashboard::box(
                #         title = tagList(shiny::icon("fas fa-table"),
                #                         "Pairwise Differential Gene Expression Table"), 
                #         width = 12,
                #         status = "success",
                #         withSpinner(DTOutput('highlight.df'))
                #     )
                # )
        ),
        
        ## Browse by Life Stage Tab ----
        tabItem(tabName = "lifestagebrowser",
                fluidRow(
                    ## User Input Options ----
                    
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-sliders-h"), "Inputs"),
                        width = 4,
                        status = "warning",
                        
                        # Select Target Life Stage
                        checkboxGroupInput("selectTarget",
                                    h4("Select Target(s)"),
                                    choices = list("FLF",
                                                   "PF",
                                                   "iL3",
                                                   "iL3a",
                                                   "ppL1",
                                                   "ppL3",
                                                   "pfL1"),
                                    selected = "iL3"),
                        # Select Contrast Life Stage
                        checkboxGroupInput("selectContrast",
                                    h4("Select Contrast(s)"),
                                    choices = list("FLF",
                                                   "PF",
                                                   "iL3",
                                                   "iL3a",
                                                   "ppL1",
                                                   "ppL3",
                                                   "pfL1"),
                                    selected = "FLF"),
                        
                        ### Action Button
                        actionButton('goLifeStage',
                                     'Submit',
                                     width = '40%',
                                     icon = icon("paper-plane"))
                        
                        
                    ),
                    
                    
                    # Reactive Outputs ----
                    
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-table"),
                                        "Pairwise Differential Gene Expression Table"), 
                        width = 8,
                        status = "success",
                        DTOutput('tbl')
                    )
                )
        )
    )
    
)

ui <- dashboardPage(header, sidebar, body, title = "SsRNAseqBrowser")

# Header ----
header <- dashboardHeader()

# Sidebar ----
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(h4("by Gene(s)"), tabName = "genebrowser"),
        menuItem(h4("by Life Stage"), tabName = "lifestagebrowser")
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
                        title = tagList(shiny::icon("fas fa-dna"), "Step 1: Input Genes"),
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
                        actionButton('goGW',
                                     'Submit',
                                     width = '40%',
                                     icon = icon("paper-plane"))
                    ),
                    
                    # Gene Outputs ----
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-chart-bar"),
                                        "Gene Expression Across Life Stages"), 
                        width = 8,
                        status = "warning",
                        side = "right",
                        withSpinner(plotOutput('CPM'))
                    ),
                    
                    # User Input: Life Stage Comparisons ----
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
                        textAreaInput('multiContrasts_GW',
                                      label = ('Input At Least Two Comma-Separated Pairwise Comparisons'),
                                      placeholder = ('e.g. iL3-FLF, iL3-PF, (iL3+iL3a)-(PF+FLF)'),
                                      rows = 5, 
                                      resize = "vertical"),
                        
                        strong("Correct for Multiple Comparisons?"),
                        switchInput(
                            inputId = "multipleContrastsYN_GW",
                            onLabel = "Yes",
                            offLabel = "No",
                            onStatus = "success"
                        ),
                        
                        
                        awesomeCheckbox('multipleContrastsYN_GW',
                                      label = 'Correct for Multiple Comparisons?'),
                        
                        
                        ### Action Button
                        actionButton('goLifeStage_GW',
                                     'Submit',
                                     width = '40%',
                                     icon = icon("paper-plane"))
                    ),
                    
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-table"),
                                        "Select Contrast to Display"), 
                        width = 4,
                        status = "warning",
                        uiOutput('contrastDisplaySelection_GW')
                    ),
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-table"),
                                        "Pairwise Differential Gene Expression: Volcano Plot"), 
                        width = 8,
                        status = "success",
                        withSpinner(plotOutput('volcano_GW'))
                    ),
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-table"),
                                        "Pairwise Differential Gene Expression: Table"), 
                        width = 12,
                        status = "success",
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
                        title = tagList(shiny::icon("fas fa-recycle"), "Pick Life Stage Comparisons"),
                        width = 4,
                        status = "warning",
                        
                        # Select Target Life Stage
                        # 
                        pickerInput("selectTarget_LS",
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
                        pickerInput("selectContrast_LS",
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
                        
                        # Text Input for Multiple Contrasts
                        textAreaInput('multiContrasts_LS',
                                      label = ('Input At Least Two Comma-Separated Pairwise Comparisons'),
                                      placeholder = ('e.g. iL3-FLF, iL3-PF, (iL3+iL3a)-(PF+FLF)'),
                                      rows = 5, 
                                      resize = "vertical"),
                        
                        strong("Correct for Multiple Comparisons?"),
                        switchInput(
                            inputId = "multipleContrastsYN_LS",
                            onLabel = "Yes",
                            offLabel = "No",
                            onStatus = "success"
                        ),
                        
                        ### Action Button
                        actionButton('goLS',
                                     'Submit',
                                     width = '40%',
                                     icon = icon("paper-plane"))
                        
                        
                    ),
                    
                    
                    # Reactive Outputs ----
                    
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-table"),
                                        "Select Contrast to Display"), 
                        width = 4,
                        status = "warning",
                        uiOutput('contrastDisplaySelection_LS')
                    ),
                    
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-table"),
                                        "Pairwise Differential Gene Expression: Volcano Plot"), 
                        width = 8,
                        status = "success",
                        withSpinner(plotOutput('volcano_LS'))
                    ),
                    
                    shinydashboard::box(
                        title = tagList(shiny::icon("fas fa-table"),
                                        "Pairwise Differential Gene Expression: Table"), 
                        width = 12,
                        status = "success",
                        withSpinner(DTOutput('tbl_LS'))
                    )
                    
                    
                )
        )
    )
    
)

ui <- dashboardPage(header, sidebar, body, title = "SsRNAseqBrowser")
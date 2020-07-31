
# Header ----
navbarPage(("Strongyloides stercoralis RNAseq Browser"),
           theme = shinytheme("flatly"), collapsible = TRUE,
           
           tabPanel(h4("Browse By Gene"),
                    
                    fluidRow(
                        column(3,
                               # Select Genes to Browse ----
                               panel(
                                   heading = tagList(h4(shiny::icon("fas fa-dna"), "Step 1A: Input Genes")),
                                   status = "primary",
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
                                                width = '30%',
                                                icon = icon("fas fa-share"),
                                                class = "btn-primary")
                               )),
                        column(6,
                               conditionalPanel(condition = "input.goGW != 0",
                                                # Display Selected Gene Expression ----
                                                panel(
                                                    heading = tagList(h4(shiny::icon("fas fa-chart-bar"),
                                                                         "Gene Expression Across Life Stages")),
                                                    status = "primary",
                                                    withSpinner(plotOutput('CPM'),
                                                                color = "#2C3E50")
                                                )
                               )
                        ),
                        column(3,
                               conditionalPanel(condition = "input.goGW !=0",
                                                uiOutput("geneDisplaySelection_GW")))
                    ),
                    fluidRow(
                        column(3,
                               conditionalPanel(condition = "input.goGW != 0",
                                                panel(
                                                    # Select Life Stage Comparisons ----
                                                    heading = tagList(h4(shiny::icon("fas fa-sliders-h"), 
                                                                         "Step 2A: Pick Life Stage Comparisons")),
                                                    status = "primary",
                                                    # Select Target Life Stage
                                                    h4('Option A: Select Single Pairwise Comparison'),
                                                    
                                                    selectInput("selectTarget_GW",
                                                                "Select One or More Targets",
                                                                choices = list("FLF",
                                                                               "PF",
                                                                               "iL3",
                                                                               "iL3a",
                                                                               "ppL1",
                                                                               "ppL3",
                                                                               "pfL1"),
                                                                
                                                                multiple = TRUE),
                                                    # Select Contrast Life Stage
                                                    selectInput("selectContrast_GW",
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
                                                    
                                                    tags$hr(style="border-color: black;"),
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
                                                    
                                                    
                                                    ### Action Button
                                                    actionButton('goLifeStage_GW',
                                                                 'Process',
                                                                 width = '30%',
                                                                 icon = icon("fas fa-share"),
                                                                 class = "btn-primary")
                                                ))
                        ),
                        
                        column (6,
                                conditionalPanel(condition = "input.goLifeStage_GW != 0",
                                                 panel(
                                                     heading = tagList(h4(shiny::icon("fas fa-chart-area"),
                                                                          "Pairwise Differential Gene Expression: Volcano Plot")),
                                                     status = "primary",
                                                     plotOutput('volcano_GW',
                                                                            hover = hoverOpts("plot_hover", 
                                                                                              delay = 100, 
                                                                                              delayType = "debounce")),
                                                                 uiOutput("hover_info"),
                                                                 color = "#2C3E50")
                                )
                        ),
                        column(3,
                               uiOutput('contrastDisplaySelection_GW')
                        ),
                        column(9,
                               conditionalPanel(condition = "input.goLifeStage_GW != 0",
                                                panel(
                                                    heading = tagList(h4(shiny::icon("fas fa-table"),
                                                                         "Pairwise Differential Gene Expression: Table")),
                                                    status = "primary",
                                                    withSpinner(DTOutput('highlight.df'),
                                                                color = "#2C3E50")
                                                )
                               )
                        )
                    ) 
                    # ),
                    # fluidRow(
                    #     column(3),
                    #     column(9,
                    #            conditionalPanel(condition = "input.goLifeStage_GW != 0",
                    #                             panel(
                    #                                 heading = tagList(h4(shiny::icon("fas fa-table"),
                    #                                                      "Pairwise Differential Gene Expression: Table")),
                    #                                 status = "primary",
                    #                                 withSpinner(DTOutput('highlight.df'),
                    #                                             color = "#2C3E50")
                    #                             )
                    #            )
                    #     )
                    # )
                    
           ),
           
           # Life Stage Browser ----
           tabPanel(h4("Browse by Life Stage"),
                    fluidRow(
                        column(3,
                               panel(
                                   heading = tagList(h4(shiny::icon("fas fa-sliders-h"), 
                                                        "Step 1A: Pick Life Stage Comparisons")),
                                   status = "primary",
                                   h4('Option A: Select Single Pairwise Comparison'),
                                   selectInput("selectTarget_LS",
                                               "Select One or More Targets",
                                               choices = list("FLF",
                                                              "PF",
                                                              "iL3",
                                                              "iL3a",
                                                              "ppL1",
                                                              "ppL3",
                                                              "pfL1"),
                                               
                                               multiple = TRUE),
                                   # Select Contrast Life Stage
                                   selectInput("selectContrast_LS",
                                               "Select One or More Contrasts",
                                               choices = list("FLF",
                                                              "PF",
                                                              "iL3",
                                                              "iL3a",
                                                              "ppL1",
                                                              "ppL3",
                                                              "pfL1"),
                                               
                                               multiple = TRUE),
                                   
                                   tags$hr(style="border-color: black;"),
                                   h4('Option B: Manually Input Multiple Pairwise Comparisons'),
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
                                                width = '30%',
                                                icon = icon("fas fa-share"),
                                                class = "btn-primary")
                                   
                               )
                        ),
                        
                        column (6,
                                conditionalPanel(condition = "input.goLS != 0",
                                                 panel(
                                                     heading = tagList(h4(shiny::icon("fas fa-chart-area"),
                                                                          "Pairwise Differential Gene Expression: Volcano Plot")),
                                                     status = "primary",
                                                     withSpinner(plotOutput('volcano_LS'),
                                                                 color = "#2C3E50"))
                                )
                        ),
                        column(3,
                               uiOutput('contrastDisplaySelection_LS')
                        ),
                        column(9,
                               conditionalPanel(condition = "input.goLS != 0",
                                                panel(
                                                    heading = tagList(h4(shiny::icon("fas fa-table"),
                                                                         "Pairwise Differential Gene Expression: Table")),
                                                    status = "primary",
                                                    withSpinner(DTOutput('tbl_LS'),
                                                                color = "#2C3E50")
                                                )
                               )
                        )
                    )
                    # ),
                    # fluidRow(
                    #     column(3),
                    #     column(9,
                    #            conditionalPanel(condition = "input.goLS != 0",
                    #                             panel(
                    #                                 heading = tagList(h4(shiny::icon("fas fa-table"),
                    #                                                      "Pairwise Differential Gene Expression: Table")),
                    #                                 status = "primary",
                    #                                 withSpinner(DTOutput('tbl_LS'),
                    #                                             color = "#2C3E50")
                    #                             )
                    #            )
                    #     )
                    # )
                    
                    
           )
           
           
)



# Header ----
navbarPage(h3(em("Strongyloides"), "RNAseq Browser"),
           windowTitle = "Str-RNAseq Browser",
           theme = shinytheme("flatly"), 
           collapsible = TRUE,
           id = "tab",
           
           # Gene Browser Tab ----
           tabPanel(h4("Browse By Gene"),
                    value = "GW",
                    useShinyjs(),
                    div(id = "GW",
                    
                        ## Fluid Row 1: Species Selection Panel + Download Study Info Dropdown Menu ----    
                    fluidRow(
                        
                        column(3,
                               
                               panel(
                                   heading = tagList(h5(shiny::icon("fas fa-sliders-h"),
                                                         "Select Species")),
                                   status = "primary",
                                   id = "speciesPanelID_GW",
                                   selectInput("selectSpecies_GW",
                                               label = h6("Pick a species"),
                                               choices = list("S. stercoralis",
                                                              "S. ratti",
                                                              "S. papillosus",
                                                              "S. venezuelensis")
                                               ),
                                   actionButton('speciesGW',
                                                'Initialize',
                                                icon = icon("fas fa-share"),
                                                class = "btn-primary")
                               )),
                        column(3,
                               conditionalPanel(condition = "input.speciesGW != 0",
                                                panel(heading = tagList(h5(shiny::icon("fas fa-info-circle"),
                                                                           "Download Study Info Files")),
                                                      status = "default",
                                                pickerInput("which.Experimental.Info.GW",
                                                            NULL, 
                                                            c("Study Design",
                                                              "Log2CPM Gene Counts",
                                                              "vDGEList",
                                                              "Discarded Gene Counts"),
                                                            options = list(style = 'btn btn-default')),
                                                uiOutput("StudyInfo.panel.GW")
                                                )
                               ),
                               offset = 6
                        )
                        ),
                    ## Fluid Row 2: Input Gene Panel + Life Stage Legend + Gene Dropdown Menu + Gene Expression Plots Panel----
                    fluidRow(
                        
                        column(3,
                               
                               conditionalPanel(condition = "input.speciesGW != 0",
                                   uiOutput("genePanelinputs")
                               )),
                        column(3,
                               conditionalPanel(condition = "input.goGW !=0",
                                                id = "geneSelection_conditionalPanel",
                                                uiOutput("geneDisplaySelection_GW"))
                               
                        ),
                        column(6,
                               conditionalPanel(condition = "input.goGW !=0 && output.geneDisplaySelection_GW",
                                                id = "lifeStageLegend_GW",
                                                uiOutput("Legend_GW"))
                               
                        ),
                        
                        column(9,
                               conditionalPanel(condition = "input.goGW != 0",
                                                id = "geneplot_conditionalPanel",
                                                uiOutput("genePlotPanel_GW")
                                                
                               )
                        )
                        
                    ),
                    ## Fluid Row 3: Input Pairwise Comparisons Panel + Volcano Plot Panel + Contrasts Dropdown Menu + Differential Gene Expression Table  ----
                    fluidRow(
                        column(3,
                               conditionalPanel(condition = "(output.downloadbuttonsGenes || output.volcano_GW)",
                                                id = "lifeStageInputPanel",
                                                uiOutput("pairwiseSelector_GW")
                               )
                        ),
                        
                        column(7,
                               conditionalPanel(condition = "(output.downloadbuttonsGenes && input.goLifeStage_GW != 0)",
                                                uiOutput("volcano_GW")
                               )
                        ),
                        column(2,
                               conditionalPanel(condition = "output.downloadbuttonsGenes && input.goLifeStage_GW !=0",
                               id = "contrastSelectionPanel_GW",
                               uiOutput('contrastDisplaySelection_GW')
                               )
      
                        ),
                        column(9,
                               conditionalPanel(condition = "output.downloadbuttonsGenes && input.goLifeStage_GW != 0 && output.contrastDisplaySelection_GW && output.volcano_GW",
                                                panel(
                                                    heading = tagList(h5(shiny::icon("fas fa-table"),
                                                                         "Pairwise Differential Gene Expression: Table")),
                                                    status = "primary",
                                                    
                                                    DTOutput('highlight.df'),
                                                    uiOutput("downloadbuttonGW")        
                                                    
                                                )
                               )
                        )
                    ) 
                    )
           ),
           
           # Life Stage Browser Tab ----
           tabPanel(h4("Browse by Life Stage"),
                    value = "LS",
                    useShinyjs(),
                    div(id = "LS",
                        ## Fluid Row 1: Species Selection Panel + Life Stage Legend + Download Study Info Dropdown Menu ----
                    fluidRow(
                        column(3,
                               panel(
                                   heading = tagList(h5(shiny::icon("fas fa-sliders-h"),
                                                         "Select Species")),
                                   status = "primary",
                                   id = "speciesPanelID_LS",
                                   selectInput("selectSpecies_LS",
                                               h6("Pick a species"),
                                               choices = list("S. stercoralis",
                                                              "S. ratti",
                                                              "S. papillosus",
                                                              "S. venezuelensis")),
                                   actionButton('speciesLS',
                                                'Initialize',
                                                icon = icon("fas fa-share"),
                                                class = "btn-primary")
                                   )
                               ),
                        column(7,
                               
                               conditionalPanel(condition = "input.speciesLS != 0",
                                                id = "lifeStageLegend_LS",
                                                uiOutput("Legend_LS"))
                        ),
                        column(2,
                               conditionalPanel(condition = "input.speciesLS != 0",
                                                panel(heading = tagList(h5(shiny::icon("fas fa-info-circle"),
                                                                           "Download Study Info Files")),
                                                      status = "default",
                                                      pickerInput("which.Experimental.Info.LS",
                                                                  NULL, 
                                                                  c("Study Design",
                                                                    "Log2CPM Gene Counts",
                                                                    "vDGEList",
                                                                    "Discarded Gene Counts"),
                                                                  options = list(style = 'btn btn-default')),
                                                      uiOutput("StudyInfo.panel.LS")
                                                )
                               )
                        )
                        ),
                    
                        
                    ## Fluid Row 2: Input Pairwise Comparisons Panel + Volcano Plot Panel + Contrasts Dropdown Menu + Differential Gene Expression Table ----    
                    fluidRow(
                        column(3,
   
                               conditionalPanel(condition = 'input.speciesLS != 0',
                               uiOutput("pairwiseSelector_LS")
                        )),
                        column(3,
                               conditionalPanel(condition = "output.pairwiseSelector_LS && input.goLS !=0",
                                                id = "contrastDisplaySelectionPanel_LS",
                                                uiOutput('contrastDisplaySelection_LS'))
                        ),
                        
                        column(9,
                               conditionalPanel(condition = "output.pairwiseSelector_LS && input.goLS != 0",
                                                uiOutput('volcano_LS')               
                               )
                        ),
                        
                        column(9,
                               conditionalPanel(condition = "output.pairwiseSelector_LS && input.goLS != 0 && output.volcano_LS",
                                                panel(
                                                    heading = tagList(h5(shiny::icon("fas fa-table"),
                                                                         "Pairwise Differential Gene Expression: Table")),
                                                    status = "primary",
                                                    DTOutput('tbl_LS'),
                                                    uiOutput('downloadbuttonLS')
                                                )
                               )
                        )
                        
                    ),
                    ## Fluid Row 3: Gene Set Enrichment Analysis Plot and Table Panels ----
                    fluidRow(
                        
                        column(6,
                               conditionalPanel(condition = "output.pairwiseSelector_LS && input.goLS != 0 && output.volcano_LS",
                                                panel(
                                                    heading = tagList(h5(shiny::icon("fas fa-braille"),
                                                                         "Gene Set Enrichment Analysis: Plot")),
                                                    status = "primary",
                                                    withSpinner(plotOutput('GSEAPlot_LS'),
                                                                color = "#2C3E50",
                                                                type = 7),
                                                    downloadButton("downloadGSEAPlot_LS",
                                                                   "Download Plot as PDF",
                                                                   class = "btn-primary")
                                                ))
                        ),
                        column(6,
                               conditionalPanel(condition = "output.pairwiseSelector_LS && input.goLS != 0 && output.volcano_LS",
                                                panel(
                                                    heading = tagList(h5(shiny::icon("fas fa-table"),
                                                                         "Gene Set Enrichment Analysis: Data Table")),
                                                    status = "primary",
                                                    DTOutput('GSEATbl_LS'),
                                                    uiOutput("downloadGSEAtbl_LS")
                                                )))
                    )
                    )
           ),
           
           # About Tab ----
           tabPanel(h4("About"),
                    value = "about",
                    fluidRow(
                        column(12,
                               
                               source('UI/static_App_Info-ui.R', local = T)$value,
                               
                               # App Credits ----
                               panel( heading =  tagList(h5(shiny::icon("fas fa-poop"),
                                                            "Who is responsibe for this?")),
                                      status = "primary",
                                      p('This app was created by', 
                                        tags$a(
                                            href = "https://scholar.google.com/citations?user=uSGqqakAAAAJ&hl=en", 
                                            'Astra S. Bryant, PhD'),'with assistance from Stephanie DeMarco, PhD for the ',
                                        tags$a(href="http://www.hallemlab.com/",'Hallem Lab'), 'at UCLA.', 
                                        tags$br(),
                                        'The underlying code is available on Github:', 
                                        tags$a(
                                            href = 'https://github.com/astrasb/Strongyloides_Bioinformatics/tree/master/Strongyloides_RNAseq_Browser', "Strongyloides RNAseq Browser App Repository"))
                               )
                        )
                    )
           )
           
)



# Header ----
navbarPage(h3(em("Strongyloides"), "RNAseq Browser"),
           windowTitle = "Str-RNAseq Browser",
           theme = shinytheme("flatly"), 
           collapsible = TRUE,
           id = "tab",
           
           
           # Gene-Wise Browser ----
           tabPanel(h4("Browse By Gene"),
                    value = "GW",
                    fluidRow(
                        useShinyjs(),
                        column(2,
                               
                               ## Select Genes to Browse ----
                               panel(
                                   id = "GeneInputBox",
                                   heading = tagList(h4(shiny::icon("fas fa-dna"), "Step 1: Input Genes / Keywords")),
                                   status = "primary",
                                   ### GeneID (text box)
                                   h5('Pick Genes', class = 'text-danger', style = "margin: 0px 0px 5px 0px"),
                                   p(tags$em('Users may type gene stable IDs or keywords that will be matched against Wormbase Parasite Gene Descriptions and an Ensembl Compara database of gene families. Please separate search terms by a comma. Users may also upload a .csv file containing comma-separated gene stable IDs.', style = "color: #7b8a8b")),
                                   p(tags$em(tags$b('Note: Please hit the Clear button between successive searches.', style = "color: #F39C12"))),
                                   textAreaInput('idtext',
                                                 h6('Gene Stable IDs or Keyword'),
                                                 rows = 5, 
                                                 resize = "vertical"),
                                   
                                   ### Upload list of GeneIDs
                                   uiOutput('genefile_upload'),
                                   
                                   ### Action Button
                                   actionButton('goGW',
                                                'Submit',
                                                #width = '50%',
                                                icon = icon("fas fa-share"),
                                                class = "btn-primary"),
                                   
                                   actionButton('resetGenes', 'Clear',
                                                icon = icon("far fa-trash-alt"))
                               )),
                        
                        column(8,
                               conditionalPanel(condition = "input.goGW != 0",
                                                id = "geneplot_conditionalPanel",
                                                uiOutput("genePlotPanel_GW")
                                                ## Display Selected Gene Expression ----
                                                # panel(
                                                #     heading = tagList(h4(shiny::icon("fas fa-chart-bar"),
                                                #                          "Gene Expression Across Life Stages")),
                                                #     status = "primary",
                                                #     tagList(div(id = "GenePlotDiv",
                                                #                 uiOutput("downloadbuttonsGenes")
                                                #     ))
                                                # )
                               )
                        ),
                        column(2,
                               conditionalPanel(condition = "input.goGW !=0",
                                                id = "geneSelection_conditionalPanel",
                                                uiOutput("geneDisplaySelection_GW")))
                    ),
                    fluidRow(
                        column(2,
                               conditionalPanel(condition = "output.downloadbuttonsGenes || output.contrastDisplaySelection_GW",
                                                id = "lifeStageInputPanel",
                                                panel(
                                                    ## Select Life Stage Comparisons ----
                                                    heading = tagList(h4(shiny::icon("fas fa-sliders-h"), 
                                                                         "Step 2: Pick Life Stage Comparisons")),
                                                    status = "primary",
                                                    # Select Target Life Stage
                                                    h5('A: Single Comparison', class = 'text-danger'),
                                                    
                                                    selectInput("selectTarget_GW",
                                                                h6("Select Target"),
                                                                choices = c('Choose one or more' = ''
                                                                            ,list("FLF",
                                                                                  "PF",
                                                                                  "iL3",
                                                                                  "iL3a",
                                                                                  "ppL1",
                                                                                  "ppL3",
                                                                                  "pfL1")),
                                                                selectize = TRUE,
                                                                multiple = TRUE),
                                                    # Select Contrast Life Stage
                                                    selectInput("selectContrast_GW",
                                                                h6("Select Contrast"),
                                                                choices = c('Choose one or more' = '',
                                                                            list("FLF",
                                                                                 "PF",
                                                                                 "iL3",
                                                                                 "iL3a",
                                                                                 "ppL1",
                                                                                 "ppL3",
                                                                                 "pfL1")),
                                                                selectize = TRUE,
                                                                multiple = TRUE),
                                                    
                                                    tags$hr(style="border-color: #2C3E50;"),
                                                    h5('B: Multiple Comparisons', class = 'text-danger'),
                                                    
                                                    # Text Input for Multiple Contrasts
                                                    textAreaInput('multiContrasts_GW',
                                                                  (h6('Type comma-separated comparisons using format: (Target)-(Contrast)',
                                                                      tags$br(),tags$em('e.g. iL3-PF, (iL3+iL3a)-(PF+FLF)', style = "color: #7b8a8b"))),
                                                                  
                                                                  rows = 5, 
                                                                  resize = "vertical"),
                                                    
                                                    h6("Correct for Multiple Comparisons?"),
                                                    switchInput(
                                                        inputId = "multipleContrastsYN_GW",
                                                        onLabel = "Yes",
                                                        offLabel = "No",
                                                        size = "small",
                                                        onStatus = "success"
                                                    ),
                                                    
                                                    tags$hr(style="border-color: #2C3E50;"),
                                                    
                                                    ### Action Button
                                                    actionButton('goLifeStage_GW',
                                                                 'Process',
                                                                 #width = '50%',
                                                                 icon = icon("fas fa-share"),
                                                                 class = "btn-primary"),
                                                    
                                                    actionButton('resetGW', 'Clear',
                                                                 icon = icon("far fa-trash-alt"))
                                                )
                                                )
                        ),
                        
                        column(8,
                                conditionalPanel(condition = "input.goLifeStage_GW != 0",
                                                 id = "diffPlotPanel",
                                                 panel(
                                                     heading = tagList(h4(shiny::icon("fas fa-mountain"),
                                                                          "Pairwise Differential Gene Expression: Volcano Plot")),
                                                     status = "primary",
                                                     withSpinner(plotOutput('volcano_GW',
                                                                            hover = hoverOpts("plot_hover", 
                                                                                              delay = 100, 
                                                                                              delayType = "debounce")),
                                                                 
                                                                 color = "#2C3E50"),
                                                     uiOutput("hover_info"),
                                                     
                                                     downloadButton("downloadVolcano_GW",
                                                                    "Download Plot as PDF",
                                                                    class = "btn-primary")
                                                 )
                                                 
                                )
                        ),
                        column(2,
                               uiOutput('contrastDisplaySelection_GW')
                        ),
                        column(10,
                               conditionalPanel(condition = "input.goLifeStage_GW != 0",
                                                panel(
                                                    heading = tagList(h4(shiny::icon("fas fa-table"),
                                                                         "Pairwise Differential Gene Expression: Table")),
                                                    status = "primary",
                                                    withSpinner(DTOutput('highlight.df'),
                                                                color = "#2C3E50"),
                                                    uiOutput('downloadbuttonGW')
                                                )
                               )
                        )
                    )     
           ),
           
           # Life Stage Browser ----
           tabPanel(h4("Browse by Life Stage"),
                    value = "LS",
                    fluidRow(
                        column(2,
                               panel(
                                   heading = tagList(h4(shiny::icon("fas fa-sliders-h"), 
                                                        "Step 1: Pick Life Stage Comparisons")),
                                   status = "primary",
                                   h5('A: Single Comparison', class = 'text-danger', style = "margin: 0px 0px 10.5px 0px"),
                                   selectInput("selectTarget_LS",
                                               h6("Select Target"),
                                               choices = c('Pick one or more' = ''
                                                           ,list("FLF",
                                                                 "PF",
                                                                 "iL3",
                                                                 "iL3a",
                                                                 "ppL1",
                                                                 "ppL3",
                                                                 "pfL1")),
                                               selectize = TRUE,
                                               multiple = TRUE),
                                   # Select Contrast Life Stage
                                   selectInput("selectContrast_LS",
                                               h6("Select Contrast"),
                                               choices = c('Pick one or more' = ''
                                                           ,list("FLF",
                                                                 "PF",
                                                                 "iL3",
                                                                 "iL3a",
                                                                 "ppL1",
                                                                 "ppL3",
                                                                 "pfL1")),
                                               selectize = TRUE,
                                               multiple = TRUE),
                                   
                                   tags$hr(style="border-color: black;"),
                                   h5('B: Multiple Comparisons', class = 'text-danger'),
                                   # Text Input for Multiple Contrasts
                                   textAreaInput('multiContrasts_LS',
                                                 (h6('Type comma-separated comparisons using format: (Target)-(Contrast)',
                                                     tags$br(),tags$em('e.g. iL3-PF, (iL3+iL3a)-(PF+FLF)', style = "color: #7b8a8b"))),
                                                 #label = ('Input At Least Two Comma-Separated Pairwise Comparisons'),
                                                 #placeholder = ('e.g. iL3-FLF, iL3-PF, (iL3+iL3a)-(PF+FLF)'),
                                                 rows = 5, 
                                                 resize = "vertical"),
                                   
                                   
                                   
                                   
                                   h6("Correct for Multiple Comparisons?"),
                                   switchInput(
                                       inputId = "multipleContrastsYN_LS",
                                       onLabel = "Yes",
                                       offLabel = "No",
                                       size = "small",
                                       onStatus = "success"
                                   ),
                                   
                                   tags$hr(style="border-color: #2C3E50;"),
                                   
                                   ### Action Button
                                   actionButton('goLS',
                                                'Submit',
                                                #width = '50%',
                                                icon = icon("fas fa-share"),
                                                class = "btn-primary"),
                                   
                                   actionButton('resetLS', 'Clear',
                                                icon = icon("far fa-trash-alt"))
                                   
                               )
                        ),
                        
                        column(8,
                                conditionalPanel(condition = "input.goLS != 0",
                                                 panel(
                                                     heading = tagList(h4(shiny::icon("fas fa-mountain"),
                                                                          "Pairwise Differential Gene Expression: Volcano Plot")),
                                                     status = "primary",
                                                     withSpinner(plotOutput('volcano_LS',
                                                                            hover = hoverOpts("plot_hover_LS",
                                                                                              delay = 100,
                                                                                              delayType = "debounce")),
                                                                 color = "#2C3E50"),
                                                     # withSpinner(plotOutput('volcano_LS'),
                                                     #             color = "#2C3E50"),
                                                     uiOutput("hover_info_LS"),
                                                     
                                                     downloadButton("downloadVolcano_LS",
                                                                    "Download Plot as PDF",
                                                                    class = "btn-primary")
                                                 )
                                                 
                                                 
                                )
                        ),
                        column(2,
                               uiOutput('contrastDisplaySelection_LS')
                        ),
                        
                        column(10,
                               conditionalPanel(condition = "input.goLS != 0",
                                                panel(
                                                    heading = tagList(h4(shiny::icon("fas fa-table"),
                                                                         "Pairwise Differential Gene Expression: Table")),
                                                    status = "primary",
                                                    withSpinner(DTOutput('tbl_LS'),
                                                                color = "#2C3E50"),
                                                    uiOutput('downloadbuttonLS')
                                                )
                               )
                        )
                        
                    ),
                    fluidRow(
                        
                        column(6,
                               conditionalPanel(condition = "input.goLS != 0",
                                                panel(
                                                    heading = tagList(h4(shiny::icon("fas fa-braille"),
                                                                         "Gene Set Enrichment Analysis: Plot")),
                                                    status = "primary",
                                                    withSpinner(plotOutput('GSEAPlot_LS'),
                                                                color = "#2C3E50"),
                                                    downloadButton("downloadGSEAPlot_LS",
                                                                   "Download Plot as PDF",
                                                                   class = "btn-primary")
                                                ))
                               ),
                        column(6,
                               conditionalPanel(condition = "input.goLS != 0",
                                                panel(
                                                    heading = tagList(h4(shiny::icon("fas fa-table"),
                                                                         "Gene Set Enrichment Analysis: Data Table")),
                                                    status = "primary",
                                                    withSpinner(DTOutput('GSEATbl_LS'),
                                                                color = "#2C3E50"),
                                                    uiOutput("downloadGSEAtbl_LS")
                                                )))
                    )
           ),
           
           tabPanel(h4("About"),
                    value = "about",
                    fluidRow(
                        column(12,
                               # About this app ----
                               panel(heading =  tagList(h4(shiny::icon("fas fa-question-circle"),
                                                           "About this App")),
                                     
                                     status = "primary",
                                     
                                     p('This Shiny app enables users to browse a bioinformatics dataset featuring bulk RNA sequencing of seven *S. stercoralis* developmental stages ',
                                       tags$a(
                                           href = 'https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0001854', 
                                           "(Stolzfus et al 2012)"), '.', br()),
                                     tags$h5('Data Preprocessing', class = 'text-danger'),
                                     p('Offline, Kallisto and custom R scripts were used to perform ultra-fast read mapping of raw reads to the *S. stercoalis* reference transcriptome (PRJEB528.WBPS14.mRNA_transcripts, downloaded from ',
                                       tags$a(
                                           href = "https://parasite.wormbase.org/Strongyloides_stercoralis_prjeb528/Info/Index/", 
                                           'WormBase Parasite'),
                                       'on 16 June 2020).',
                                       tags$br(),
                                       'Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 3 samples), and normalized using the trimmed mean of M-values method (TMM, ',
                                       tags$a(
                                           href = "https://genomebiology.biomedcentral.com/articles/10.1186/gb-2010-11-3-r25", 
                                           'Robinson and Oshlack)'),
                                       'to permit between-samples comparisons.', br(),
                                       'The limma package',
                                       tags$a(
                                           href = "https://pubmed.ncbi.nlm.nih.gov/25605792/", 
                                           '(Ritchie et al 2015'),
                                       ', ',
                                       tags$a(
                                           href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5373812/", 
                                           'Phipson et al 2016)'),
                                       'is used to conduct pairwise differential gene expression analyses between life stages. The mean-variance relationship is modeled using a precision weights approach',
                                       tags$a(
                                           href = "https://genomebiology.biomedcentral.com/articles/10.1186/gb-2014-15-2-r29", 
                                           '(Law et al 2014)'),
                                       '.'),
                                     tags$h5('Data Visualization', class = 'text-danger'),
                                     p('For heatmaps of Log2 Counts per Million gene expression values, columns (life stages) were ordered using Spearman clustering of expression in all genes (not just the user-defined subset). Rows were ordered using Pearson clustering of expression of the user-selected gene subset. Only the life stage dendrogram is displayed for clarity.')
                                     
                                     
                                     
                               ),
                               # App Credits ----
                               panel( heading =  tagList(h4(shiny::icon("fas fa-poop"),
                                                            "Who is responsibe for this?")),
                                      status = "primary",
                                      p('This app was created by', 
                                        tags$a(
                                            href = "https://scholar.google.com/citations?user=uSGqqakAAAAJ&hl=en", 
                                            'Astra S. Bryant, PhD'),'for the', 
                                        tags$a(href="http://www.hallemlab.com/",'Hallem Lab'), 'at UCLA.', 
                                        tags$br(),
                                        'The underlying code is avaliable on Github:', 
                                        tags$a(
                                            href = "Stercoralis RNAseq Browser App  Repository", 
                                            'https://github.com/astrasb/Strongyloides_Bioinformatics/tree/master/Stercoralis_RNAseq_Browser'))
                               )
                        )
                    )
           )
           
)



# Header ----
navbarPage(h3(em("Strongyloides"), "RNAseq Browser"),
           windowTitle = "Str-RNAseq Browser",
           theme = shinytheme("flatly"), 
           collapsible = TRUE,
           id = "tab",
           
           
           # Gene-Wise Browser ----
           tabPanel(h4("Browse By Gene"),
                    value = "GW",
                    useShinyjs(),
                    div(id = "GW",
                        
                    fluidRow(
                        
                        column(2,
                               
                               panel(
                                   heading = tagList(h5(shiny::icon("fas fa-sliders-h"),
                                                         "Select Species")),
                                   status = "primary",
                                   selectInput("selectSpecies_GW",
                                               h6("Pick a species"),
                                               choices = list("S. stercoralis",
                                                              "S. ratti")),
                                   actionButton('speciesGW',
                                                'Initialize',
                                                #width = '50%',
                                                icon = icon("fas fa-share"),
                                                class = "btn-primary")
                               ),
                               
                               ## Select Genes to Browse ----
                               conditionalPanel(condition = "input.speciesGW != 0",
                                   uiOutput("genePanelinputs")
                               )),
                        
                        column(8,
                               conditionalPanel(condition = "input.goGW != 0",
                                                id = "geneplot_conditionalPanel",
                                                uiOutput("genePlotPanel_GW")
                                                
                               )
                        ),
                        column(2,
                               conditionalPanel(condition = "input.goGW !=0",
                                                id = "geneSelection_conditionalPanel",
                                                uiOutput("geneDisplaySelection_GW")),
                               
                               conditionalPanel(condition = "input.goGW !=0 && output.geneDisplaySelection_GW",
                                                id = "lifeStageLegend_GW",
                                                uiOutput("Legend_GW"))
                               
                        )
                    ),
                    fluidRow(
                        column(2,
                               conditionalPanel(condition = "(output.downloadbuttonsGenes || output.volcano_GW)",
                                                id = "lifeStageInputPanel",
                                                uiOutput("pairwiseSelector_GW")
                               )
                        ),
                        
                        column(8,
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
                        column(10,
                               conditionalPanel(condition = "output.downloadbuttonsGenes && input.goLifeStage_GW != 0 && output.contrastDisplaySelection_GW && output.volcano_GW",
                                                panel(
                                                    heading = tagList(h5(shiny::icon("fas fa-table"),
                                                                         "Pairwise Differential Gene Expression: Table")),
                                                    status = "primary",
                                                    
                                                    tagList(withSpinner(DTOutput('highlight.df'),
                                                                        color = "#2C3E50",
                                                                        type = 7),
                                                            
                                                            uiOutput("downloadbuttonGW")        
                                                    )
                                                )
                               )
                        )
                    ) 
                    )
           ),
           
           # Life Stage Browser ----
           tabPanel(h4("Browse by Life Stage"),
                    value = "LS",
                    useShinyjs(),
                    div(id = "LS",
                    fluidRow(
                        column(2,
                               panel(
                                   heading = tagList(h5(shiny::icon("fas fa-sliders-h"),
                                                         "Select Species")),
                                   status = "primary",
                                   selectInput("selectSpecies_LS",
                                               h6("Pick a species"),
                                               choices = list("S. stercoralis",
                                                              "S. ratti")),
                                   actionButton('speciesLS',
                                                'Initialize',
                                                #width = '50%',
                                                icon = icon("fas fa-share"),
                                                class = "btn-primary")
                               ),
                               
                              
                               conditionalPanel(condition = 'input.speciesLS != 0',
                               uiOutput("pairwiseSelector_LS")
                        )
                        ),
                        
                        column(8,
                               conditionalPanel(condition = "output.pairwiseSelector_LS && input.goLS != 0",
                                                uiOutput('volcano_LS')               
                               )
                        ),
                        column(2,
                               conditionalPanel(condition = "output.pairwiseSelector_LS && input.goLS !=0",
                                                id = "contrastDisplaySelectionPanel_LS",
                                                uiOutput('contrastDisplaySelection_LS')
                               ),
                               conditionalPanel(condition = "input.goLS !=0 && output.contrastDisplaySelection_LS",
                                                id = "lifeStageLegend_LS",
                                                uiOutput("Legend_LS"))
                        ),
                        
                        column(10,
                               conditionalPanel(condition = "output.pairwiseSelector_LS && input.goLS != 0 && output.volcano_LS",
                                                panel(
                                                    heading = tagList(h5(shiny::icon("fas fa-table"),
                                                                         "Pairwise Differential Gene Expression: Table")),
                                                    status = "primary",
                                                    withSpinner(DTOutput('tbl_LS'),
                                                                color = "#2C3E50",
                                                                type = 7),
                                                    uiOutput('downloadbuttonLS')
                                                )
                               )
                        )
                        
                    ),
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
                                                    withSpinner(DTOutput('GSEATbl_LS'),
                                                                color = "#2C3E50",
                                                                type = 7),
                                                    uiOutput("downloadGSEAtbl_LS")
                                                )))
                    )
                    )
           ),
           
           tabPanel(h4("About"),
                    value = "about",
                    fluidRow(
                        column(12,
                               # About this app ----
                               panel(heading =  tagList(h5(shiny::icon("fas fa-question-circle"),
                                                           "About this App")),
                                     
                                     status = "primary",
                                     
                                     p('This Shiny app enables users to browse a bioinformatics dataset featuring bulk RNA sequencing of seven ', tags$em('S. stercoralis'), ' developmental stages ',
                                       tags$a(
                                           href = 'https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0001854', 
                                           "(Stolzfus et al 2012)."), br()),
                                     tags$h5(tags$em('S. stercoralis'), 'Data Preprocessing', class = 'text-danger'),
                                     p('Offline, Kallisto and custom R scripts were used to perform ultra-fast read mapping of raw reads to the ', tags$em('S. stercoralis'), ' reference transcriptome (PRJEB528.WBPS14.mRNA_transcripts, downloaded from ',
                                       tags$a(
                                           href = "https://parasite.wormbase.org/Strongyloides_stercoralis_prjeb528/Info/Index/", 
                                           'WormBase Parasite'),
                                       'on 16 June 2020).',
                                       tags$br(),
                                       'Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 3 samples). This process excluded 717 genes from the final dataset.', tags$br(),
                                       tags$a(href="SsRNAseq_discardedGene_counts.csv", 
                                              "Download the list of excluded genes and their expression across life stages here.", 
                                              download="SsRNAseq_discardedGene_counts.csv", target="blank"), tags$br(),
                                       
                                       'Finally, CPM values were normalized using the trimmed mean of M-values method (TMM, ',
                                       tags$a(
                                           href = "https://genomebiology.biomedcentral.com/articles/10.1186/gb-2010-11-3-r25", 
                                           'Robinson and Oshlack)'),
                                       'to permit between-samples comparisons.', 
                                       
                                       tags$br(),
                                       tags$a(href="SsRNAseq_discardedGene_counts.csv", 
                                              "Download the database of filtered and TMM-normalized log2 CPM gene expression values.", 
                                              download="SsRNAseq_log2cpm_filtered_norm.csv", target="blank"), tags$br()
                                     ),
                                     
                                     tags$h5('On-demand Differential Expression Analysis', class = 'text-danger'),
                                     p('Here, the limma package',
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
                                           '(Law et al 2014).')),
                                     
                                     tags$h5('Functional Enrichment Analysis', class = 'text-danger'),
                                     p('Here, we perform gene set enrichment analysis using the GSEA function from the clusterProfiler R package. Given a priori defined set of gene XX, the goal of GSEA is to determine whether the members of XX are randomly distributed throughout the ranked gene list (L) or primarily found at the top or bottom. Thus, GSEA depends on the availability of gene sets. Here, we use a Ensembl Compara protein family set list defined in Hunt', tags$em('et al'), '2016. (Note that this uses specific transcript information, which are discarded). The list was preprocessed to remove genes that are not found in the preprocessed list of genes for which we have RNASeq data. For a selected pairwise comparison, the gene list (L) is constructed by rank ordering by LogFC.'),
                                     p('GSEA analysis returns a plot of enriched gene families as well as a table containing normalized gene enrichment scores. These scores represent the degree to which the elements of the gene set are over-represented at the edges of the ranked gene list. Sccores are normalized based on the number of genes within the gene set.'), 
                                     tags$h5('Data Visualization', class = 'text-danger'),
                                     p('For heatmaps of Log2 Counts per Million gene expression values, columns (life stages) were ordered using Spearman clustering of expression in all genes (not just the user-defined subset). Rows were ordered using Pearson clustering of expression of the user-selected gene subset. Only the life stage dendrogram is displayed for clarity.')
                                     
                               ),
                               
                               # App Credits ----
                               panel( heading =  tagList(h5(shiny::icon("fas fa-poop"),
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
                                            href = 'https://github.com/astrasb/Strongyloides_Bioinformatics/tree/master/Strongyloides_RNAseq_Browser', "Strongyloides RNAseq Browser App Repository"))
                               )
                        )
                    )
           )
           
)

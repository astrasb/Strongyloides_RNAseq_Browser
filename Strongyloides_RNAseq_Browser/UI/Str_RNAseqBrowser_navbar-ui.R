

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
                                     
                                     p('This Shiny app enables users to browse publically available', tags$em('Strongyloides'),
                                       'bulk RNAseq datasets, and perform on-demand analyses including differential expression and gene set enrichment.',
                                       'Data from the following species are currently avaliable:', tags$em('Strongyloides stercoralis,'),'and',tags$em('Strongyloides ratti.')),
                                     tags$h5('Data Preprocessing', class = 'text-danger'),
                                     p('All code used in data preprocessing is publically avaliable in a',
                                       tags$a(
                                           href = 'https://github.com/astrasb/Strongyloides_Bioinformatics/tree/master/Pre_processing', 
                                           "subsection of the Strongyloides Bioinformatics Github repository.")),
                                     p(tags$strong(tags$em('S. stercoralis:')), 'Raw reads were downloaded from the European Nucleotide Archive (study accession number',
                                       tags$a(
                                           href = "https://www.ebi.ac.uk/ena/browser/view/PRJEB3116", 
                                           'PRJEB3116'),', data originally described in', tags$a(
                                               href = 'https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0001854', 
                                               "Stolzfus et al 2012"),').',
                                       'Kallisto was used to perform ultra-fast read mapping of raw reads to the ',
                                       tags$em('S. stercoralis'),
                                       ' reference transcriptome (',
                                       tags$a(
                                           href = "https://parasite.wormbase.org/Strongyloides_stercoralis_prjeb528/Info/Index/", 
                                           'PRJEB528.WBPS14.mRNA_transcripts,'),
                                       'downloaded from Wormbase ParaSite on 16 June 2020). Kallisto alignments were imported into the R environment and annotated with information imported via the Wormbase ParaSite BioMaRT. Annotation information includes:', tags$em('C. elegans'), 'homologs/percent homology,', tags$em('S. ratti'),'homologs/percent homology, UniProtKB number, Interpro terms, GO terms, and general Description information. Annotation information is saved as an R object that is loaded into this app.',
                                       
                                       'Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 3 samples). This process excluded 717 genes from the final dataset.',
                                       tags$a(href="SsRNAseq_discardedGene_counts.csv", 
                                              "[Download the list of excluded genes and their non-normalized CPM expression values across life stages here.]", 
                                              download="SsRNAseq_discardedGene_counts.csv", target="blank"),
                                       
                                       'For non-discarded genes, CPM values were normalized using the trimmed mean of M-values method (TMM, ',
                                       tags$a(
                                           href = "https://genomebiology.biomedcentral.com/articles/10.1186/gb-2010-11-3-r25", 
                                           'Robinson and Oshlack, 2010)'),
                                       'to permit between-samples comparisons. The mean-variance relationship was modeled using a precision weights approach, via the limma:voom function',
                                       tags$a(
                                           href = "https://genomebiology.biomedcentral.com/articles/10.1186/gb-2014-15-2-r29", 
                                           '(Law et al 2014).'), 'A variance-stabilized DGEList object was saved and is loaded into this app for downstream browsing and analysis.',
                                       tags$br(),
                                       tags$br(),
                                       
                                       
                                       tags$strong(tags$em('S. ratti:')), 'Raw reads were downloaded from the European Nucleotide Archive (study accession numbers',
                                       tags$a(
                                           href = "https://www.ebi.ac.uk/ena/browser/view/PRJEB1376", 
                                           'PRJEB1376'),'and',
                                       tags$a(
                                           href = "https://www.ebi.ac.uk/ena/browser/view/PRJEB3187", 
                                           'PRJEB3187'),
                                       ', data originally described in', tags$a(
                                           href = 'https://www.nature.com/articles/ng.3495', 
                                           "Hunt et al 2016"),').',
                                       'Raw reads were aligned to the', tags$em('S. ratti'),'reference transcriptome (',
                                       tags$a(
                                           href = "https://parasite.wormbase.org/Strongyloides_ratti_prjeb125/Info/Index", 
                                           'PRJEB125.WBPS14.mRNA_transcripts,'),
                                       'downloaded from Wormbase ParaSite on 17 August 2020), using Kallisto. Kallisto alignments were imported into the R environment and annotated with information imported via the Wormbase ParaSite BioMaRT. Annotation information includes:', tags$em('C. elegans'), 'homologs/percent homology,', tags$em('S. stercoralis'),'homologs/percent homology, UniProtKB number, Interpro terms, GO terms, and general Description information. Annotation information is saved as an R object that is loaded into this app.',
                                       'Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 1 sample). This process excluded 416 genes from the final dataset.', 
                                       tags$a(href="SrRNAseq_discardedGene_counts.csv",
                                              "[Download the list of excluded genes and their non-normalized CPM expression values across life stages here.]",
                                              download="SrRNAseq_discardedGene_counts.csv", target="blank"),
                                       'For non-discarded genes, CPM values were normalized using the trimmed mean of M-values method to permit between-samples comparisons. The mean-variance relationship was modeled using a precision weights approach, via the limma:voom function. This dataset includes technical replicates; after voom modeling, data were condensed by replacing within-experiment technical replicates with their average, using the limma:avearrays function',
                                       tags$a(
                                           href = "http://www.statsci.org/smyth/pubs/normalize.pdf", 
                                           '(Smyth, Michaud, and Scott, 2005).'),
                                       'A variance-stabilized, condensed DGEList object was saved and is loaded into this app for downstream browsing and analysis.', tags$br(),
                                       tags$strong('Note:'), 'samples included in this database were collected in two separate experiments, with FLM and iL3s in one, and FLF and PF in another. Due to the lack of sample overlap between the experiments, we do not correct for batch effects.'),
                                     
                                     
                                     tags$h5('On-demand Differential Expression Analysis', class = 'text-danger'),
                                     p('The limma package is used to conduct pairwise differential gene expression analyses between life stages',
                                       tags$a(
                                           href = "https://pubmed.ncbi.nlm.nih.gov/25605792/", 
                                           '(Ritchie et al 2015'),
                                       ', ',
                                       tags$a(
                                           href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5373812/", 
                                           'Phipson et al 2016).')),
                                      
                                     tags$h5('Functional Enrichment Analysis', class = 'text-danger'),
                                     p('We perform gene set enrichment analysis using the GSEA function from the clusterProfiler R package',
                                       tags$a(
                                           href = "http://dx.doi.org/10.1089/omi.2011.0118", 
                                           '(Yu et al 2012).'),
                                       'The goal of GSEA is to determine whether the members of a predefined set of genes are randomly distributed throughout a ranked gene list. Thus, GSEA depends on the availability of gene sets. Here, we use a Ensembl Compara protein family set list defined in',
                                       tags$a(
                                           href = 'https://www.nature.com/articles/ng.3495', 
                                           "Hunt et al 2016."),
                                       '(Note that the original set list uses specific transcript information, which we discard). The R script used to assemble the gene set list for use in this app is publically available as part of our Strongyloides Bioinformatics repository',
                                       tags$a(
                                           href = "https://github.com/astrasb/Strongyloides_Bioinformatics/blob/master/Pre_processing/generateGeneSet.R", 
                                           '(Pre_processing subfolder > `generateGeneSet.R`).'),
                                       'For a selected pairwise comparison, the ranked gene list is constructed by rank ordering by LogFC.'),
                                     p('The GSEA analysis returns a plot of enriched gene families as well as a table containing normalized gene enrichment scores. These scores represent the degree to which the elements of the gene set are over-represented at the edges of the ranked gene list. Scores are normalized based on the number of genes within the gene set.'), 
                                     tags$h5('Data Visualization', class = 'text-danger'),
                                     p('For heatmaps of Log2 Counts per Million gene expression values, columns (life stages) were ordered using Spearman clustering of expression in all genes (not just the user-defined subset). Rows were ordered using Pearson clustering of expression of the user-selected gene subset.')
                                     
                               ),
                               
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

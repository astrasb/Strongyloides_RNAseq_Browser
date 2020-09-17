# General Information ----
panel(heading =  tagList(h5(shiny::icon("fas fa-question-circle"),
                            "About this App")),
      status = "primary",
      
      p('This Shiny app enables users to browse publically available', tags$em('Strongyloides'),
        'bulk RNAseq datasets, and perform on-demand analyses including differential expression and gene set enrichment.',
        'Data from the following species are currently avaliable:', tags$em('Strongyloides stercoralis,'),'and',tags$em('Strongyloides ratti.')),
      
      # Data Preprocessing ----      
      tags$h5('Data Preprocessing', class = 'text-danger'),
      p('All code used in data preprocessing is publically avaliable in a',
        tags$a(
            href = 'https://github.com/astrasb/Strongyloides_Bioinformatics/tree/master/Pre_processing', 
            "subsection of the Strongyloides Bioinformatics Github repository.")),
      
      ## S. stercoralis ----
      p(tags$strong(tags$em(style = "color: #E74C3C",'S. stercoralis:')), 'Raw reads were downloaded from the European Nucleotide Archive (study accession number',
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
        'downloaded from Wormbase ParaSite on 16 June 2020). Kallisto alignments were imported into the R environment and annotated with information imported via the Wormbase ParaSite BioMaRT.',
        'Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 3 samples). This process excluded 717 genes from the final dataset.',
        tags$a(href="SsRNAseq_discardedGene_counts.csv", 
               "[Download the list of excluded genes and their non-normalized CPM expression values across life stages here.]", 
               download="SsRNAseq_discardedGene_counts.csv", target="blank"),
        
        'For non-discarded genes, CPM values were normalized using the trimmed mean of M-values method (TMM, ',
        tags$a(
            href = "https://genomebiology.biomedcentral.com/articles/10.1186/gb-2010-11-3-r25", 
            'Robinson and Oshlack, 2010)'),
        'to permit between-samples comparisons.',
        tags$a(href="SsRNAseq_log2cpm_filtered_norm.csv",
               "[Download the filtered, normalized Log2CPM values here.]",
               download="SsRNAseq_log2cpm_filtered_norm.csv", target="blank"),
        'The mean-variance relationship was modeled using a precision weights approach, via the limma:voom function',
        tags$a(
            href = "https://genomebiology.biomedcentral.com/articles/10.1186/gb-2014-15-2-r29", 
            '(Law et al 2014).'), 'A variance-stabilized DGEList object was saved and is loaded into this app for downstream browsing and analysis.',
        tags$a(href="../Data/Ss_vDEGList",
               "[Download the vDGEList here.]",
               download="../Data/Ss_vDEGList", target="blank"),
        tags$br(),
        tags$br(),
        
        # S. ratti ----
        tags$strong(tags$em(style = "color: #E74C3C",'S. ratti:')), 'Raw reads were downloaded from the European Nucleotide Archive (study accession numbers',
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
        'downloaded from Wormbase ParaSite on 17 August 2020), using Kallisto. Kallisto alignments were imported into the R environment and annotated with information imported via the Wormbase ParaSite BioMaRT.',
        'Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 1 sample). This process excluded 416 genes from the final dataset.', 
        tags$a(href="SrRNAseq_discardedGene_counts.csv",
               "[Download the list of excluded genes and their non-normalized CPM expression values across life stages here.]",
               download="SrRNAseq_discardedGene_counts.csv", target="blank"),
        'For non-discarded genes, CPM values were normalized using the trimmed mean of M-values method to permit between-samples comparisons.',
        tags$a(href="SrRNAseq_log2cpm_filtered_norm.csv",
               "[Download the filtered, normalized Log2CPM values here.]",
               download="SrRNAseq_log2cpm_filtered_norm.csv", target="blank"),
        'The mean-variance relationship was modeled using a precision weights approach, via the limma:voom function. This dataset includes technical replicates; after voom modeling, data were condensed by replacing within-experiment technical replicates with their average, using the limma:avearrays function',
        tags$a(
            href = "http://www.statsci.org/smyth/pubs/normalize.pdf", 
            '(Smyth, Michaud, and Scott, 2005).'),
        'A variance-stabilized, condensed DGEList object was saved and is loaded into this app for downstream browsing and analysis.',
        tags$a(href="../Data/Sr_vDEGList",
               "[Download the vDGEList here.]",
               download="../Data/Sr_vDEGList", target="blank"), tags$br(),
        tags$strong('Note:'), 'samples included in this database were collected in two separate experiments, with FLM and iL3s in one, and FLF and PF in another. Due to the lack of sample overlap between the experiments, we do not correct for batch effects.',
      
      tags$br(),
      tags$br(),
      
      # S. papillosus ----
      
      tags$strong(tags$em(style = "color: #E74C3C",'S. papillosus:')), 'Raw reads were downloaded from the European Nucleotide Archive (study accession number',
      tags$a(
          href = "https://www.ebi.ac.uk/ena/browser/view/PRJEB14543", 
          'PRJEB14543'),', data originally analyzed by', tags$a(
              href = 'https://www.nature.com/articles/ng.3495', 
              "Hunt et al 2016"),').',
      
      'Raw reads were aligned to the', tags$em('S. papillosus'),'reference transcriptome (',
      tags$a(
          href = "https://parasite.wormbase.org/Strongyloides_papillosus_prjeb525/Info/Index", 
          'PRJEB525.WBPS14.mRNA_transcripts,'),
      'downloaded from Wormbase ParaSite on 17 August 2020), using Kallisto. Kallisto alignments were imported into the R environment and annotated with information imported via the Wormbase ParaSite BioMaRT.',
      
      'Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 2 samples)',
      'This process excluded 3247 genes from the final dataset.', 
      tags$a(href="SpRNAseq_discardedGene_counts.csv",
             "[Download the list of excluded genes and their non-normalized CPM expression values across life stages here.]",
             download="SpRNAseq_discardedGene_counts.csv", target="blank"),
      'For non-discarded genes, CPM values were normalized using the trimmed mean of M-values method to permit between-samples comparisons.',
      tags$a(href="SpRNAseq_log2cpm_filtered_norm.csv",
             "[Download the filtered, normalized Log2CPM values here.]",
             download="SpRNAseq_log2cpm_filtered_norm.csv", target="blank"),
      'The mean-variance relationship was modeled using a precision weights approach, via the limma:voom function, as for other datasets. A variance-stabilized DGEList object was saved and is loaded into this app for downstream browsing and analysis.',
      tags$a(href="../Data/Sp_vDEGList",
             "[Download the vDGEList here.]",
             download="../Data/Sp_vDEGList", target="blank"),
      
      tags$br(),
      tags$br(),
      
      # S. venezuelensis ----
      tags$strong(tags$em(style = "color: #E74C3C", 'S. venezuelensis:')), 'Raw reads were downloaded from the European Nucleotide Archive (study accession number',
      tags$a(
          href = "https://www.ebi.ac.uk/ena/browser/view/PRJDB3457", 
          'PRJDB3457'),', data originally published by', tags$a(
              href = 'https://www.nature.com/articles/s41598-018-23514-z', 
              "Hunt et al 2018"),').',
      
      'Raw reads were aligned to the', tags$em('S. venezuelensis'),'reference transcriptome (',
      tags$a(
          href = "https://parasite.wormbase.org/Strongyloides_venezuelensis_PRJEB530/Info/Index", 
          'PRJEB530.WBPS14.mRNA_transcripts,'),
      'downloaded from Wormbase ParaSite on 17 August 2020), using Kallisto. Kallisto alignments were imported into the R environment and annotated with information imported via the Wormbase ParaSite BioMaRT.',
      
      'Samples included in study PRJDB3457 were prepared using different libary construction methods (amplified vs non-amplified), sequencing run batches, and machines (',
      tags$a(
          href = 'https://www.nature.com/articles/s41598-018-23514-z', 
          "Hunt et al 2018"),
      '). Dividing the experiments based on sequencing instrument produces two batches, both of which contain data from Free-living females and thereotically permit batch correction. However, following limma-based batch correction there were still substantial differences between FLF groups from the two batches. We therefore take the conservative approach of treating these two batches separately.',
      tags$br(),
      'Thus, we define two functional groups for processing and analysis:'),
      tags$ol(
          tags$li(('Group FLF_PF:'), 'Free-living females and parasitic females (samples DRR106346 - DRR106357; aka SAMD00096905-SAMD00096910; SRA Study: DRP002629). This set includes 3 biological replicates and two technical replicates per life stage.'),
          tags$li(('Group iL3_extended:'), 'Egg, L1, iL3s, activated iL3s (1 and 5 day), iL3_lung, Young_FLF, FLF (samples DRR029282, DRR029433 - DRR029445; SRA Study: ). This set includes 1 biological replicated and two technical replicates per life stage, except for activated iL3s, which have a single technical replicate at 1 and 5 days.')
      ),
      p(tags$strong('Note:'), 'Currently, only Group FLF_PF is included in the', tags$em('Strongyloides'),
      'RNAseq Browser.',
      'Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 3 samples)',
      'This process excluded 4613 genes from the final dataset.', 
      tags$a(href="SvRNAseq_discardedGene_counts.csv",
             "[Download the list of excluded genes and their non-normalized CPM expression values across life stages here.]",
             download="SvRNAseq_discardedGene_counts.csv", target="blank"),
      'For non-discarded genes, CPM values were normalized using the trimmed mean of M-values method to permit between-samples comparisons.',
      tags$a(href="SpRNAseq_log2cpm_filtered_norm.csv",
             "[Download the filtered, normalized Log2CPM values here.]",
             download="SpRNAseq_log2cpm_filtered_norm.csv", target="blank"),
      'The mean-variance relationship was modeled using a precision weights approach, via the limma:voom function, as for other datasets. This dataset includes technical replicates; after voom modeling, data were condensed by replacing within-experiment technical replicates with their average, using the limma:avearrays function',
      tags$a(
          href = "http://www.statsci.org/smyth/pubs/normalize.pdf", 
          '(Smyth, Michaud, and Scott, 2005).'),
      'A variance-stabilized DGEList object was saved and is loaded into this app for downstream browsing and analysis.',
      tags$a(href="../Data/Sv_vDEGList",
             "[Download the vDGEList here.]",
             download="../Data/Sv_vDEGList", target="blank")),
      
      # Gene Annotation ----
      
      tags$h5('Gene Annotation', class = 'text-danger'),
      p('Annotation information includes:', tags$em('C. elegans'), 'homologs/percent homology,', 'homologs/percent homology, UniProtKB number, Interpro terms, GO terms, and general Description information.', tags$a(
          href = 'https://www.nature.com/articles/ng.3495', 
          "Hunt et al 2016"),
      'establishes two distinct subclades from the four sequenced', tags$em('Strongyloides'),'species: ',
      tags$em('S. stercoralis & S. ratti'), 'and', tags$em('S. papillosus & S. venezuelensis.'), 
      'Thus, we also included annotation information for the appropriate in-group, and a reference member of the out-group (either', tags$em('S. stercoralis or S. papillosus.'), 'Annotation information is saved as an R object that is loaded into this app.'),
      
      # On-demand Differential Expression Analysis ----
      tags$h5('On-demand Differential Expression Analysis', class = 'text-danger'),
      p('The limma package is used to conduct pairwise differential gene expression analyses between life stages',
        tags$a(
            href = "https://pubmed.ncbi.nlm.nih.gov/25605792/", 
            '(Ritchie et al 2015'),
        ', ',
        tags$a(
            href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5373812/", 
            'Phipson et al 2016).')),
      
      # Functional Enrichment Analysis ----
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
      
)
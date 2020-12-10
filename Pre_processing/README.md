# Data Pre-processing 
This folder contains RMarkdown files for each species; these files contain code for the alignment of raw reads, data filtering and normalization, voom variance-normalization of count data, and collection of gene annotation information. RMarkdown files generate outputs that act as essential inputs to the *Strongyloides* RNAseq Browser App. RMarkdown files are knitted into PDF files; see those PDF files for plots illustrating the effect of filtering and normallization on raw data inputs.  

Our goal is to pre-process previously published RNAseq datasets downloaded from online repositories, for subsequent analysis by the Shiny web app. Use the sections below to view details for the pre-processing of each *Strongyloides spp.* dataset.  

## Table of Contents  
1. [*S. stercoralis*](#s-stercoralis)
2. [*S. ratti*](#s-ratti)
3. [*S. papillosus*](#s-papillosus)
4. [*S. venezuelensis*](#s-venezuelensis)

## *S. stercoralis*  
The *S. stercoralis* dataset included in this repository was originaly published by [Stolzfus *et al* 2012](https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0001854).  

### Data Sources and Details 
Raw reads were downloaded from the European Nucleotide Archive - study accession number [PRJEB3116](https://www.ebi.ac.uk/ena/browser/view/PRJEB3116). This dataset consists of 21 samples, representing 7 life stages with 3 biological replicates for each life stage:

  * Free-living adult females (FLF)
  * Parasitic adult females (PF)
  * Infective third-stage larvae (iL3)
  * Activated iL3s (iL3a)
  * Post-parasitic 1st stage larvae (ppL1)
  * Post-parasitic 3rd stage larvae (ppL3)
  * Post-free-living 1st stage larvae (pfL1)

### Kallisto Alignment and Gene Annotation  
Kallisto was used to perform ultra-fast read mapping of raw reads to the *S. stercoalis* reference transcriptome (PRJEB528.WBPS14.mRNA_transcripts, downloaded from [WormBase Parasite](https://parasite.wormbase.org/Strongyloides_stercoralis_prjeb528/Info/Index/) on 16 June 2020). Kallisto alignments are imported into the R environment using `Tximport`. Counts are generated from abundance files using the `lengthScaledTPM` option; an R object containing this data is saved into each species' subfolder in the primary Data folder. In subsequent chunks, that file is loaded, and analysis progresses. The point of this is so that folks attempting to rerun this analysis do not need to have abundance files loaded on their local machines (and we do not have to upload abundance files to github).  
Count data is then annotated with information imported via the Wormbase ParaSite BioMaRT. Annotation information includes:

  * *C. elegans* homologs/percent homology
  * UniProtKB number
  * Interpro terms
  * GO terms
  * general Description information
  
[Hunt *et al* 2016](https://www.nature.com/articles/ng.3495) establishes two distinct subclades from the four sequenced *Strongyloides* species: *S. venezuelensis-S. papillosus* and *S. ratti-S. stercoralis*. Thus, we also include annotation information for the appropriate in-group and a reference member of the out-group, here:

  * In-group: *S. ratti* homologs/percent homology
  * Out-group: *S. papillosus* homologs/percent homology

### Filtering and Normalization Steps
Raw reads are quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 3 samples). Non-discarded gene values are normalized using the trimmed mean of M-values method [(TMM, Robinson and Oshlack)](https://genomebiology.biomedcentral.com/articles/10.1186/gb-2010-11-3-r25) to permit between-samples comparisons. The mean-variance relationship was modeled using a precision weights approach [(Law *et al* 2014)](https://genomebiology.biomedcentral.com/articles/10.1186/gb-2014-15-2-r29).  

### Pre-processing Outputs
Finally, we save data and annotations for use by offline analysis files and the Shiny application. We can separate these saving actions into two groups:

1. Data saved for downstream offline analyses, including the `SsRNAseq_data_preprocessed` file which saves filtered, normalized (but not voom adjusted) log2CPM values, gene annotation information, and sample information.
2. Files that are required inputs to the *Strongyloides* RNAseq Browser App, including:
    i) a gene annotation R object (`Ss_geneAnnotations`)
    ii)  the variance-stabilized vDGEList, saved as an R object (`Ss_vDGEList`)
    iii) a matrix of discarded genes and their raw counts (`SsRNAseq_discardedGene_counts.csv`) - this data is downloadable from within the Browser App
    iv) a matrix of variance-stabilized gene expression data, extracted from the vDGEList (`SsRNAseq_log2cpm_filtered_norm_voom.csv`) - this data is downloadable from within the Browser App
    
These files are saved in the Outputs folder; in order to make them accessible to a local version of the Shiny browser they need to be copied to appropriate subfolders within the App folder - the www sub folder (for .csv files) or the Data subfolder (for R objects). Stable copies are already located within those folders and do not need to be replaced unless the pre-processing steps change.  

## *S. ratti*  
The *S. ratti* data included in this repository was originally published by [Hunt *et al* 2016](https://www.nature.com/articles/ng.3495).  

### Data Sources and Details  
Raw reads were downloaded from the European Nucleotide Archive - study accession numbers [PRJEB1376](https://www.ebi.ac.uk/ena/browser/view/PRJEB1376) and [PRJEB3187](https://www.ebi.ac.uk/ena/browser/view/PRJEB3187). This dataset consists of 14 samples, representing 4 life stages with a mixed number of biological and techincal replicates:

  * Free-living adult females (FLF): 3 biological replicates with 3 techincal replicates for each biological replicate.
  * Parasitic adult females (PF): 3 biological replicates with 3 techincal replicates for each biological replicate.
  * Infective third-stage larvae (iL3): 1 biological replicate
  * Free-living adult males (FLM): 1 biological replicate

Note: samples included in this database were collected in two separate experiments, with FLM and iL3s in one, and FLF and PF in another. Due to the lack of sample overlap between the experiments, we do not correct for batch effects.  

### Kallisto Alignment and Gene Annotation  
Kallisto was used to perform ultra-fast read mapping of raw reads to the *S. ratti* reference transcriptome (PRJEB125.WBPS14.mRNA_transcripts, downloaded from [WormBase Parasite](https://parasite.wormbase.org/Strongyloides_ratti_prjeb125/Info/Index) on 17 August 2020). Kallisto alignments are imported into the R environment using `Tximport`. Counts are generated from abundance files using the `lengthScaledTPM` option; an R object containing this data is saved into each species' subfolder in the primary Data folder. In subsequent chunks, that file is loaded, and analysis progresses. The point of this is so that folks attempting to rerun this analysis do not need to have abundance files loaded on their local machines (and we do not have to upload abundance files to github).  
Count data is then annotated with information imported via the Wormbase ParaSite BioMaRT. Annotation information includes:

  * *C. elegans* homologs/percent homology
  * UniProtKB number
  * Interpro terms
  * GO terms
  * general Description information
  
[Hunt *et al* 2016](https://www.nature.com/articles/ng.3495) establishes two distinct subclades from the four sequenced *Strongyloides* species: *S. venezuelensis-S. papillosus* and *S. ratti-S. stercoralis*. Thus, we also include annotation information for the appropriate in-group and a reference member of the out-group, here:

  * In-group: *S. stercoralis* homologs/percent homology
  * Out-group: *S. papillosus* homologs/percent homology  
  
### Filtering and Normalization Steps  
Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 1 sample). Non-discarded gene values are normalized using the trimmed mean of M-values method [(TMM, Robinson and Oshlack)](https://genomebiology.biomedcentral.com/articles/10.1186/gb-2010-11-3-r25) to permit between-samples comparisons. The mean-variance relationship was modeled using a precision weights approach [(Law *et al* 2014)](https://genomebiology.biomedcentral.com/articles/10.1186/gb-2014-15-2-r29). This dataset includes technical replicates; after voom modeling, data are condensed by replacing within-experiment technical replicates with their average, using the `limma:avearrays` function [(Smyth, Michaud, and Scott, 2005)](http://www.statsci.org/smyth/pubs/normalize.pdf).  

### Pre-processing Outputs
Finally, we save data and annotations for use by offline analysis files and the Shiny application. We can separate these saving actions into two groups:

1. Data saved for downstream offline analyses, including the `SrRNAseq_data_preprocessed` file which saves filtered, normalized (but not voom adjusted) log2CPM values, gene annotation information, and sample information.
2. Files that are required inputs to the *Strongyloides* RNAseq Browser App, including:  
    i) a gene annotation R object (`Sr_geneAnnotations`)
    ii)  the variance-stabilized vDGEList, saved as an R object (`Sr_vDGEList`)
    iii) a matrix of discarded genes and their raw counts (`SrRNAseq_discardedGene_counts.csv`) - this data is downloadable from within the Browser App
    iv) a matrix of variance-stabilized gene expression data, extracted from the vDGEList (`SrRNAseq_log2cpm_filtered_norm_voom.csv`) - this data is downloadable from within the Browser App
    
These files are saved in the Outputs folder; in order to make them accessible to a local version of the Shiny browser they need to be moved to appropriate subfolders within the App folder - the www sub folder (for .csv files) or the Data subfolder (for R objects). Stable copies are already located within those folders and do not need to be replaced unless the pre-processing steps change.  

## *S. papillosus*
The *S. papillosus* data included in this repository was originally analyzed by [Hunt *et al* 2016](https://www.nature.com/articles/ng.3495).  

### Data Sources and Details  
Raw reads were downloaded from the European Nucleotide Archive - study accession number [PRJEB14543](https://www.ebi.ac.uk/ena/browser/view/PRJEB14543). This dataset consists of 10 samples, representing 6 life stages with 1-2 biological replicates per life stage:

  * Free-living adult females (FLF)
  * Parasitic adult females (PF)
  * Infective third-stage larvae (iL3)
  * Free-living adult males (FLM)
  * Mixed population of free-living 1st and 2nd stage larvae (flL1/L2)
  * Mixed population of parasitic 1st and 2nd stage larvae (pL1/L2)

### Kallisto Alignment and Gene Annotation  
Raw reads are aligned to the *S. papillosus* reference transcriptome (PRJEB525.WBPS14.mRNA_transcripts, downloaded from [WormBase Parasite](https://parasite.wormbase.org/Strongyloides_papillosus_prjeb525/Info/Index) on 17 August 2020), using Kallisto. Kallisto alignments are imported into the R environment using `Tximport`. Counts are generated from abundance files using the `lengthScaledTPM` option; an R object containing this data is saved into each species' subfolder in the primary Data folder. In subsequent chunks, that file is loaded, and analysis progresses. The point of this is so that folks attempting to rerun this analysis do not need to have abundance files loaded on their local machines (and we do not have to upload abundance files to github).  
Count data is then annotated with information imported via the Wormbase ParaSite BioMaRT. Annotation information includes:

  * *C. elegans* homologs/percent homology
  * UniProtKB number
  * Interpro terms
  * GO terms
  * general Description information
  
[Hunt *et al* 2016](https://www.nature.com/articles/ng.3495) establishes two distinct subclades from the four sequenced *Strongyloides* species: *S. venezuelensis-S. papillosus* and *S. ratti-S. stercoralis*. Thus, we also include annotation information for the appropriate in-group and a reference member of the out-group, here:

  * In-group: *S. venezuelensis* homologs/percent homology
  * Out-group: *S. stercoralis* homologs/percent homology  
  
### Filtering and Normalization Steps  
Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 1 sample). Non-discarded gene values are normalized using the trimmed mean of M-values method [(TMM, Robinson and Oshlack)](https://genomebiology.biomedcentral.com/articles/10.1186/gb-2010-11-3-r25) to permit between-samples comparisons. The mean-variance relationship was modeled using a precision weights approach [(Law *et al* 2014)](https://genomebiology.biomedcentral.com/articles/10.1186/gb-2014-15-2-r29).  

### Pre-processing Outputs
Finally, we save data and annotations for use by offline analysis files and the Shiny application. We can separate these saving actions into two groups:

1. Data saved for downstream offline analyses, including the `SpRNAseq_data_preprocessed` file which saves filtered, normalized (but not voom adjusted) log2CPM values, gene annotation information, and sample information.
2. Files that are required inputs to the *Strongyloides* RNAseq Browser App, including:  
    i) a gene annotation R object (`Sp_geneAnnotations`)
    ii)  the variance-stabilized vDGEList, saved as an R object (`Sp_vDGEList`)
    iii) a matrix of discarded genes and their raw counts (`SpRNAseq_discardedGene_counts.csv`) - this data is downloadable from within the Browser App
    iv) a matrix of variance-stabilized gene expression data, extracted from the vDGEList (`SpRNAseq_log2cpm_filtered_norm_voom.csv`) - this data is downloadable from within the Browser App
    
These files are saved in the Outputs folder; in order to make them accessible to a local version of the Shiny browser they need to be moved to appropriate subfolders within the App folder - the www sub folder (for .csv files) or the Data subfolder (for R objects). Stable copies are already located within those folders and do not need to be replaced unless the pre-processing steps change.  

## *S. venezuelensis*
The *S. venezuelensis* data included in this repository was originally published by [Hunt *et al* 2018](https://www.nature.com/articles/s41598-018-23514-z).  

### Data Sources and Details  
Raw reads were downloaded from the European Nucleotide Archive - study accession number [PRJDB3457](https://www.ebi.ac.uk/ena/browser/view/PRJDB3457). 

Samples included in study PRJDB3457 were prepared using different libary construction methods (amplified vs non-amplified), sequencing run batches, and machines [(Hunt *et al* 2018)](https://www.nature.com/articles/s41598-018-23514-z). Dividing the experiments based on sequencing instrument produces two batches, both of which contain data from Free-living females and thereotically permit batch correction. However, following limma-based batch correction there were still substantial differences between FLF groups from the two batches. We therefore take the conservative approach of treating these two batches separately.   
Thus, we define three functional groups for processing and analysis:

  1. Group FLF_PF: This set includes 2 life stages with 3 biological replicates and two technical replicates per life stage:  
    i) Free-living females (FLF)  
    ii) Parasitic adult females (PF)   
  2. Group iL3_extended:   
    i) Egg: 1 biological replicate and two technical replicates  
    ii) 1st stage larvae (L1): 1 biological replicate and two technical replicates  
    iii) Infective third-stage larvae (iL3): 1 biological replicate and two technical replicates  
    iv) activated iL3s (iL3a) - 1 day: 1 biological replicate  
    v) activated iL3s (iL3a) - 5 day: 1 biological replicate  
    vi) iL3s removed from the lungs (iL3_lung): 1 biological replicate and two technical replicates  
    vii) Young free-living adult females (Young_FLF): 1 biological replicate and two technical replicates  
    viii) Free-living adult females (FLF): 1 biological replicate and two technical replicates    

Note: Currently, only Group FLF_PF is included in the *Strongyloides* RNAseq Browser. However, the Offline Analysis RMarkdown file does include both functional groups, and a PCA analysis of the full dataset after unsuccessful batch correction.

### Kallisto Alignment and Gene Annotation  
Raw reads are aligned to the *S. venezuelensis* reference transcriptome (PRJEB530.WBPS14.mRNA_transcripts, downloaded from [WormBase Parasite](https://parasite.wormbase.org/Strongyloides_venezuelensis_PRJEB530/Info/Index) on 17 August 2020), using Kallisto. Kallisto alignments are imported into the R environment using `Tximport`. Counts are generated from abundance files using the `lengthScaledTPM` option; an R object containing this data is saved into each species' subfolder in the primary Data folder. In subsequent chunks, that file is loaded, and analysis progresses. The point of this is so that folks attempting to rerun this analysis do not need to have abundance files loaded on their local machines (and we do not have to upload abundance files to github).  
Count data is then annotated with information imported via the Wormbase ParaSite BioMaRT. Annotation information includes:

  * *C. elegans* homologs/percent homology
  * UniProtKB number
  * Interpro terms
  * GO terms
  * general Description information
  
[Hunt *et al* 2016](https://www.nature.com/articles/ng.3495) establishes two distinct subclades from the four sequenced *Strongyloides* species: *S. venezuelensis-S. papillosus* and *S. ratti-S. stercoralis*. Thus, we also include annotation information for the appropriate in-group and a reference member of the out-group, here:

  * In-group: *S. papillosus* homologs/percent homology
  * Out-group: *S. stercoralis* homologs/percent homology  
  
### Filtering and Normalization Steps  
Raw reads were quantified as counts per million using the EdgeR package, then filtered to remove transcripts with low counts (less than 1 count-per-million in at least 3 sample (Group FLF_PF), 1 sample (Group iL3_extended), or 1 sample (Group AllSamples)). Non-discarded gene values are normalized using the trimmed mean of M-values method [(TMM, Robinson and Oshlack)](https://genomebiology.biomedcentral.com/articles/10.1186/gb-2010-11-3-r25) to permit between-samples comparisons. The mean-variance relationship was modeled using a precision weights approach [(Law *et al* 2014)](https://genomebiology.biomedcentral.com/articles/10.1186/gb-2014-15-2-r29). This dataset includes technical replicates; after voom modeling, data are condensed by replacing within-experiment technical replicates with their average, using the `limma:avearrays` function [(Smyth, Michaud, and Scott, 2005)](http://www.statsci.org/smyth/pubs/normalize.pdf).   

### Pre-processing Outputs
Finally, we save data and annotations for use by offline analysis files and the Shiny application. For Group FLF_PF we can separate these saving actions into two groups:

1. Data saved for downstream offline analyses, including the `SvRNAseq_data_preprocessed` file which saves filtered, normalized (but not voom adjusted) log2CPM values, gene annotation information, and sample information.
2. Files that are required inputs to the *Strongyloides* RNAseq Browser App, including:  
    i) a gene annotation R object (`Sv_geneAnnotations`)
    ii)  the variance-stabilized vDGEList, saved as an R object (`Sv_vDGEList`)
    iii) a matrix of discarded genes and their raw counts (`SvRNAseq_discardedGene_counts.csv`) - this data is downloadable from within the Browser App
    iv) a matrix of variance-stabilized gene expression data, extracted from the vDGEList (`SvRNAseq_log2cpm_filtered_norm_voom.csv`) - this data is downloadable from within the Browser App

For Group iL3_extended and Group AllSamples, we save only the data required for downstream offline analyses, as these datasets are not included in the *Strongyloides* RNAseq Browser.  
    
All files are saved in the Outputs folder; in order to make files accessible to a local version of the Shiny browser they need to be moved to appropriate subfolders within the App folder - the www sub folder (for .csv files) or the Data subfolder (for R objects). Stable copies are already located within those folders and do not need to be replaced unless the pre-processing steps change.  

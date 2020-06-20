# Introduction to the Step 2 script ----
# Goals of this script:
# 1 - Filter and normalize data
# 2 - use ggplot2 to visualize the impact of filtering and normalization on the data.

# Notes:
# recall that abundance data are TPM, while the counts are read counts mapping to each gene or transcript

# Load packages -----
library(tidyverse) # already know about this from Step 1 script
library(edgeR) # well known package for differential expression analysis, but we only use for the DGEList object and for normalization methods
library(matrixStats) # let's us easily calculate stats on rows or columns of a data matrix
library(cowplot) # allows you to combine multiple plots in one figure
library(ggthemes)
source("~/Documents/RStudio/General/theme_Publication.R")


# Examine data up to this point ----
#myTPM <- Txi_gene$abundance
#myCounts <- Txi_gene$counts
#colSums(myTPM)
#colSums(myCounts)

# Generate and plot summary stats for the data ----
myTPM.stats <- transform(Txi_gene$abundance, 
                         SD=rowSds(Txi_gene$abundance), 
                         AVG=rowMeans(Txi_gene$abundance),
                         MED=rowMedians(Txi_gene$abundance))

# produce a scatter plot of the transformed data
p1 <- ggplot(myTPM.stats) + 
    aes(x = SD, y = MED) +
    geom_point(shape=16, size=2) +
    geom_smooth(method=lm) +
    #geom_hex(show.legend = FALSE) +
    labs(y="Median", x = "Standard deviation",
         title="Transcripts per million (TPM)",
         subtitle="unfiltered, non-normalized data",
         caption="S. stercoralis Stoltzfus RNAseq Dataset") +
    theme_bw()


# make a Digital Gene Expression list using the raw counts and plot ----
myDGEList <- DGEList(Txi_gene$counts, samples = targets$sample, group = targets$group)

#save the DEGList objects
save(myDGEList, file = "./Outputs/SsDGEList")


# calculate and plot log2 counts per million ----

# capture sample labels from the study design file
sampleLabels <- targets$sample

# Generate life stage IDs
ids <- rep(cbind(targets$group), 
           times = nrow(Tx.Ss)) %>%
    as_factor()

# use the 'cpm' function from EdgeR to get log2 counts per million
# then coerce into a tibble
# add sample names to the dataframe
# tidy up the dataframe into a tibble
log2.cpm.df.pivot <-cpm(myDGEList, log=TRUE) %>%
    as_tibble(rownames = "geneID") %>%
    setNames(nm = c("geneID", sampleLabels)) %>%
    pivot_longer(cols = -geneID, 
                 names_to = "samples", 
                 values_to = "expression") %>% 
    add_column(life_stage = ids)


# plot the pivoted data
p2 <- ggplot(log2.cpm.df.pivot) +
    aes(x=samples, y=expression, fill=life_stage) +
    geom_violin(trim = FALSE, show.legend = T) +
    stat_summary(fun = "median", 
                 geom = "point", 
                 shape = 20, 
                 size = 2, 
                 color = "black", 
                 show.legend = FALSE) +
    labs(y="log2 expression", x = "sample",
         title="Log2 Counts per Million (CPM)",
         subtitle="unfiltered, non-normalized",
         caption=paste0("produced on ", Sys.time())) +
    theme_bw() +
    coord_flip()

# Filter the data ----
# Take a look at how many genes or transcripts have no read counts in all of the samples
#table(rowSums(myDGEList$counts==0)==nrow(targets))

# now set some cut-off to get rid of genes/transcripts with low counts
# again using rowSums to tally up the 'TRUE' results of a simple evaluation
# how many genes had more than 1 CPM (TRUE) in at least n samples
# The cutoff "n" should be adjusted for the number of samples in the smallest group of comparison.
keepers <- cpm(myDGEList) %>%
    rowSums(.>1)>=3

# now use base R's simple subsetting method to filter the DGEList based on the logical produced above
myDGEList.filtered <- myDGEList[keepers,]
#dim(myDGEList.filtered)

ids.filtered <- rep(cbind(targets$group), 
                    times = nrow(myDGEList.filtered)) %>%
    as_factor()

log2.cpm.filtered.df.pivot <- cpm(myDGEList.filtered, log=TRUE) %>%
    as_tibble(rownames = "geneID") %>%
    setNames(nm = c("geneID", sampleLabels)) %>%
    pivot_longer(cols = -geneID,
                 names_to = "samples",
                 values_to = "expression") %>%
    add_column(life_stage = ids.filtered)

p3 <- ggplot(log2.cpm.filtered.df.pivot) +
    aes(x=samples, y=expression, fill=life_stage) +
    geom_violin(trim = FALSE, show.legend = T) +
    stat_summary(fun = "median", 
                 geom = "point", 
                 shape = 20, 
                 size = 2, 
                 color = "black", 
                 show.legend = FALSE) +
    labs(y="log2 expression", x = "sample",
         title="Log2 Counts per Million (CPM)",
         subtitle="filtered, non-normalized",
         caption=paste0("produced on ", Sys.time())) +
    theme_bw() +
    coord_flip()

# Normalize the data using a between samples normalization ----
# Source for TMM sample normalization here: https://genomebiology.biomedcentral.com/articles/10.1186/gb-2010-11-3-r25
myDGEList.filtered.norm <- calcNormFactors(myDGEList.filtered, method = "TMM")

log2.cpm.filtered.norm.df.pivot <- cpm(myDGEList.filtered.norm, log=TRUE) %>%
    as_tibble(rownames = "geneID") %>%
    setNames(nm = c("geneID", sampleLabels)) %>%
    pivot_longer(cols = -geneID,
                 names_to = "samples",
                 values_to = "expression") %>%
    add_column(life_stage = ids.filtered)

p4 <- ggplot(log2.cpm.filtered.norm.df.pivot) +
    aes(x=samples, y=expression, fill=life_stage) +
    geom_violin(trim = FALSE, show.legend = T) +
    stat_summary(fun = "median", 
                 geom = "point", 
                 shape = 20, 
                 size = 2, 
                 color = "black", 
                 show.legend = FALSE) +
    labs(y="log2 expression", x = "sample",
         title="Log2 Counts per Million (CPM)",
         subtitle="filtered, TMM normalized",
         caption=paste0("produced on ", Sys.time())) +
    theme_bw() +
    coord_flip()

# Plot all the plots on a grid
p5 <- plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), label_size = 12)
ggsave("Stoltzfus RNAseq Counts.pdf", 
       plot = p5, 
       device = "pdf",
       height = 8.5,
       width = 11,
       path = './Outputs/')

print("Data Wrangling Complete")

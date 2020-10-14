### On-demand Differential Gene Expression

The limma package ( [Ritchie *et al*
2015](https://pubmed.ncbi.nlm.nih.gov/25605792/), [Phipson *et al*
2016](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5373812/)) is used to
conduct pairwise differential gene expression (DEG) analyses between
life stages. For all species, DEG analyses are conducted on
variance-stabilized, filtered, normalized Log2CPM values. We use
empirical Bayes smoothing of gene-wise standard deviations to [provide
increased statistical
power](https://www.degruyter.com/doi/10.2202/1544-6115.1027). All
reported p-values are adjusted for multiple comparisons using the
Benjamini-Hochberg method. If users are performing multiple closely
related pairwise comparisons they may select the option to correct for
multiple contrasts using the global method (using the slider input
provided). For each gene, we detemine whether expression is upregulated,
downregulated, or not significantly different based on a cutoff of
p&lt;0.05.

### Functional Enrichment Analysis

We perform gene set enrichment analysis using the GSEA function from the
`clusterProfiler` R package [(Yu et al
2012)](http://dx.doi.org/10.1089/omi.2011.0118). The goal of GSEA is to
determine whether the members of a predefined set of genes are randomly
distributed throughout a ranked gene list. Thus, GSEA depends on the
availability of gene sets. Here, we use a Ensembl Compara protein family
set list defined in [(Hunt *et al*
2016)](https://www.nature.com/articles/ng.3495). Note that the original
set list uses specific transcript information, which we discard. The R
script used to assemble the gene set list for use in this app is
publically available as part of this repository (Pre\_processing
subfolder &gt; `generateGeneSet.R`).

For the example pairwise comparisons, the ranked gene list is
constructed by rank ordering by LogFC. The GSEA analysis returns a plot
of enriched gene families as well as a table containing normalized gene
enrichment scores. These scores represent the degree to which the
elements of the gene set are over-represented at the edges of the ranked
gene list. Scores are normalized based on the number of genes within the
gene set.

### Data Visualization

For heatmaps of Log2 Counts per Million gene expression values, columns
(life stages) are ordered using Spearman clustering of the user-defined subset. Rows are ordered using Pearson
clustering of expression of the user-selected gene subset.

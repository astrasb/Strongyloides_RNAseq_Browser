## Calculate Differential Gene Expression for a specified pairwise comparison.
## Requires a Bayes-smoothed linear model
## Produces a Top Table of differentially expressed genes. 

calc_DEG_tbl <- function (ebFit, coef) {
    myTopHits.df <- limma::topTable(ebFit, adjust ="BH", 
                                    coef=coef, number=40000, 
                                    sort.by="logFC") %>%
        as_tibble(rownames = "geneID") %>%
        dplyr::rename(tStatistic = t, LogOdds = B, BH.adj.P.Val = adj.P.Val) %>%
        dplyr::relocate(UniProtKB, Description, InterPro, GO_term, Str_geneID, Str_percent_homology, Ce_geneID, Ce_percent_homology, .after = LogOdds) %>%
        dplyr::relocate(ends_with("WBgeneID"), .before = Str_geneID)
    
    myTopHits.df
    }

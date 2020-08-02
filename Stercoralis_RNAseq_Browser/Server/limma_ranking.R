# Run Limma based Differential Expression Analysis
# Use Empirical Bayes Statistics to rank genes in order of evidence for differential expression
# Adjust for Multiple Comparisons if necessary

contrast.matrix <- makeContrasts(contrasts = vals$comparison,
                                 levels = v.DEGList.filtered.norm$design)

fits <- contrasts.fit(fit, contrast.matrix)
vals$ebFit <- limma::eBayes(fits)

# Adjust for multiple comparisons using method = global.
# Generate a corrected diffGenes value that accounts for multiple comparisons
if (vals$multipleCorrection == T) {
    results <- decideTests(vals$ebFit, 
                                method="global", 
                                adjust.method="BH", 
                                p.value = adj.P.thresh)
}else{
    results <- decideTests(vals$ebFit, 
                                method="separate", 
                                adjust.method="BH", 
                                p.value = adj.P.thresh)    
}

recode01<- function(x){
    case_when(x == 1 ~ "Up",
              x == -1 ~ "Down",
              x == 0 ~ "NotSig")
}
results <- results %>%
    as_tibble(rownames = "geneID")

diffDesc<-results %>%
    dplyr::mutate(across(-geneID, unclass)) %>%
    dplyr::mutate(across(where(is.double), recode01))

vals$list.myTopHits.df <- sapply(vals$comparison, function(y){
    calc_DEG_tbl(vals$ebFit, y)}, 
    simplify = FALSE, 
    USE.NAMES = TRUE)

#### Filter dataset looking for the genes on the list
if (isTruthy(vals$genelist)){
    vals$list.highlight.df <- sapply(vals$comparison, function(y){
        vals$list.myTopHits.df[[y]] %>%
            dplyr::filter(geneID %in% vals$genelist[[1]]) %>%
            dplyr::select(geneID, 
                          logFC, 
                          BH.adj.P.Val:percent_homology)},
        simplify = FALSE, 
        USE.NAMES = TRUE)
} else {
    vals$list.highlight.df <- sapply(vals$comparison, function(y){
        vals$list.myTopHits.df[[y]] %>%
            dplyr::select(geneID, 
                          logFC, 
                          BH.adj.P.Val:percent_homology)},
        simplify = FALSE, 
        USE.NAMES = TRUE)
}



# Get log2CPM values and threshold information for genes of interest
if (isTruthy(vals$genelist)){
    vals$list.highlight.tbl <- sapply(seq_along(vals$comparison), function(y){
        tS<- vals$targetStage[y,][vals$targetStage[y,]!=""]
        cS<- vals$contrastStage[y,][vals$contrastStage[y,]!=""]
    
        diffGenes.df %>%
            dplyr::filter(geneID %in% vals$genelist[[1]]) %>%
            dplyr::select(geneID, starts_with(paste0(tS,"-")), 
                          starts_with(paste0(cS,"-"))) %>%
            left_join(vals$list.highlight.df[[y]], by = "geneID")%>%
            left_join(dplyr::select(diffDesc,geneID,vals$comparison[y]), by = "geneID")%>%
            dplyr::rename(DEG_Desc=vals$comparison[y])%>%
            dplyr::relocate(DEG_Desc)},
    simplify = FALSE)
} else {
    vals$list.highlight.tbl <- sapply(seq_along(vals$comparison), function(y){
        tS<- vals$targetStage[y,][vals$targetStage[y,]!=""]
        cS<- vals$contrastStage[y,][vals$contrastStage[y,]!=""]
        
        diffGenes.df %>%
            dplyr::select(geneID, starts_with(paste0(tS,"-")), 
                          starts_with(paste0(cS,"-"))) %>%
            left_join(vals$list.highlight.df[[y]], by = "geneID") %>%
            left_join(dplyr::select(diffDesc,geneID,vals$comparison[y]), by = "geneID") %>%
            dplyr::rename(DEG_Desc=vals$comparison[y])%>%
            dplyr::relocate(DEG_Desc)},
        simplify = FALSE)
}
names(vals$list.highlight.tbl) <- vals$comparison

vals$list.highlight.tbl <- sapply(vals$comparison, function(y){
    vals$list.highlight.tbl[[y]] %>%
        dplyr::mutate(DEG_Desc = case_when(DEG_Desc == "Up" ~ paste0("Up in ", str_split(y,'-',simplify = T)[1,1]),
                                           DEG_Desc == "Down" ~ paste0("Down in ", str_split(y,'-',simplify = T)[1,1]),
                                           DEG_Desc == "NotSig" ~ "NotSig")) 
    },
    simplify = FALSE, 
    USE.NAMES = TRUE)

sapply(vals$comparison, function(y) {
    vals$list.highlight.tbl[[y]]$BH.adj.P.Val <-formatC(vals$list.highlight.tbl[[y]]$BH.adj.P.Val, 
                                                        digits = 3, 
                                                        format = "E") 
})


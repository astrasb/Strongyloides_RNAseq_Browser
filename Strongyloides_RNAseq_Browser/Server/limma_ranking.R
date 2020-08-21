# Run Limma based Differential Expression Analysis
# Use Empirical Bayes Statistics to rank genes in order of evidence for differential expression
# Adjust for Multiple Comparisons if necessary
limma_ranking <- function(comparison, targetStage, contrastStage, multipleCorrection, genelist, vals, fit, v.DEGList.filtered.norm, adj.P.thresh, diffGenes.df){
    if (targetStage == contrastStage) {
        stop("Pick two different life stages.")
    }
    
    contrast.matrix <- makeContrasts(contrasts = comparison,
                                     levels = v.DEGList.filtered.norm$design)
    
    fits <- contrasts.fit(fit, contrast.matrix)
    ebFit <- limma::eBayes(fits)
    
    # Adjust for multiple comparisons using method = global.
    # Generate a corrected diffGenes value that accounts for multiple comparisons
    if (multipleCorrection == T) {
        results <- decideTests(ebFit, 
                               method="global", 
                               adjust.method="BH", 
                               p.value = adj.P.thresh)
    }else{
        results <- decideTests(ebFit, 
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
    
    vals$list.myTopHits.df <- sapply(comparison, function(y){
        calc_DEG_tbl(ebFit, y)}, 
        simplify = FALSE, 
        USE.NAMES = TRUE)
    
    #### Filter dataset looking for the genes on the list
    if (isTruthy(genelist)){
        list.highlight.df <- sapply(comparison, function(y){
            vals$list.myTopHits.df[[y]] %>%
                dplyr::filter(geneID %in% genelist[[1]]) %>%
                dplyr::select(geneID, 
                              logFC, 
                              BH.adj.P.Val:percent_homology)},
            simplify = FALSE, 
            USE.NAMES = TRUE)
    } else {
        list.highlight.df <- sapply(comparison, function(y){
            vals$list.myTopHits.df[[y]] %>%
                dplyr::select(geneID, 
                              logFC, 
                              BH.adj.P.Val:percent_homology)},
            simplify = FALSE, 
            USE.NAMES = TRUE)
    }
    
    
    
    if (isTruthy(genelist)){
        diffGenes.df <- diffGenes.df %>%
            dplyr::filter(geneID %in% genelist[[1]])
    } 
    # Get log2CPM values and threshold information for genes of interest
    vals$list.highlight.tbl <- sapply(seq_along(comparison), function(y){
        tS<- targetStage[y,][targetStage[y,]!=""]
        cS<- contrastStage[y,][contrastStage[y,]!=""]
        
        concat_name <- function(x) {
            ifelse(x == "target", 
                   paste(tS, collapse = "+"), 
                   paste(cS, collapse = "+"))
        }
        
        groupAvgs <- diffGenes.df %>%
            dplyr::select(geneID, starts_with(paste0(tS,"-")), 
                          starts_with(paste0(cS,"-"))) %>%
            pivot_longer(cols = -geneID, names_to = c("group","sample"), values_to = "CPM",
                         names_sep = "-") %>%
            dplyr::mutate(contrastID = if_else(group %in% tS,"target", "contrast")) %>%
            group_by(geneID, contrastID) %>%
            dplyr::select(-sample) %>%
            summarize(mean = mean(CPM), .groups = "drop_last") %>%
            pivot_wider(names_from = contrastID, values_from = mean) %>%
            dplyr::relocate(contrast, .after = target) %>%
            dplyr::rename_with(concat_name, -geneID) %>%
            dplyr::rename_with(.cols =-geneID, .fn = ~ paste0("avg_(",.x,")"))
        
        diffGenes.df %>%
            dplyr::select(geneID, starts_with(paste0(tS,"-")), 
                          starts_with(paste0(cS,"-"))) %>%
            left_join(groupAvgs, by = "geneID") %>%
            left_join(list.highlight.df[[y]],., by = "geneID") %>%
            left_join(dplyr::select(diffDesc,geneID,comparison[y]), by = "geneID") %>%
            dplyr::rename(DEG_Desc=comparison[y]) %>%
            dplyr::relocate(DEG_Desc) %>%
            dplyr::relocate(logFC:percent_homology, .after = last_col())
        
    },
    simplify = FALSE)
    
    comparison <- gsub("/[0-9]*","", comparison)
    names(vals$list.highlight.tbl) <- comparison
    
    vals$list.highlight.tbl <- sapply(comparison, function(y){
        vals$list.highlight.tbl[[y]] %>%
            dplyr::mutate(DEG_Desc = case_when(DEG_Desc == "Up" ~ paste0("Up in ", str_split(y,'-',simplify = T)[1,1]),
                                               DEG_Desc == "Down" ~ paste0("Down in ", str_split(y,'-',simplify = T)[1,1]),
                                               DEG_Desc == "NotSig" ~ "NotSig")) 
    },
    simplify = FALSE, 
    USE.NAMES = TRUE)
    
    
}
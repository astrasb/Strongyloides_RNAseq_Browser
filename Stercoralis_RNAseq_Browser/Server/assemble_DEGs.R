# create interactive tables to display the thresholded DEGs, ranked ----

    
    diffGenes.TopHits <- DEGs.only[[comparison]]
    
    DEG.datatable <- diffGenes.TopHits %>%
        dplyr::select(geneID, starts_with(paste0(targetStage,"-")), 
                      paste0("avg_",targetStage),
                      starts_with(paste0(contrastStage,"-")),
                      paste0("avg_", contrastStage),
                      logFC, BH.adj.P.Val:percent_homology)
    
    DEG.datatable <- DEG.datatable %>%
        DT::datatable(extensions = c('KeyTable', "FixedHeader"),
                      rownames = FALSE,
                      caption = htmltools::tags$caption(
                          style = 'caption-side: top; text-align: left;',
                          htmltools::tags$b('Differentially Expressed Genes in', htmltools::tags$em('S. stercoralis'), 
                                            targetStage, ' vs ', contrastStage),
                          htmltools::tags$br(),
                          "Threshold: p < ",
                          adj.P.thresh, "; log-fold change > ",
                          lfc.thresh,
                          htmltools::tags$br(),
                          'Values = log2 counts per million'),
                      options = list(keys = TRUE,
                                     autoWidth = TRUE,
                                     scrollX = TRUE,
                                     order = list(9, 'desc'),
                                     searchHighlight = TRUE, 
                                     pageLength = 10, 
                                     lengthMenu = c("10", "25", "50", "100")))
    
    DEG.datatable <- DEG.datatable %>%
        DT::formatRound(columns=c(2:9), digits=2) %>%
        DT::formatRound(columns = c(10,12), digits = 3)
    
    DEG.datatable


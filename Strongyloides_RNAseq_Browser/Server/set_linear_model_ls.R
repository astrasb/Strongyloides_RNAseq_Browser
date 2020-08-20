## GW: Set Contrast Matrix and Fit the Linear Model ----
set_linear_model_LS <- eventReactive(input$goLS,{
    limma_ranking(vals$limmacontrast_LS, 
                  vals$targetStage_LS, 
                  vals$contrastStage_LS, 
                  vals$multipleCorrection_LS, 
                  NA, 
                  vals, fit, 
                  v.DEGList.filtered.norm, 
                  adj.P.thresh, 
                  diffGenes.df)
    
    vals$list.highlight.tbl_LS <- vals$list.highlight.tbl
})
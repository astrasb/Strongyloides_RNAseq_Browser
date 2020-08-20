## GW: Generate/Reset Gene File Upload ----
output$genefile_upload <- renderUI({
    input$resetGenes
    fileInput('loadfile',
              h6('Gene Stable ID List (.csv)'),
              multiple = FALSE)
})

## GW: Clear Genes ----
observeEvent(input$resetGenes,{
    vals$genelist <- NULL
    vals$HeatmapRowOrder <- NULL
    updateTextAreaInput(session,"idtext",value = "")
    updatePickerInput(session, "displayedGene", choices = "", selected = "")
    removeUI(selector = "#CPMPlotlydiv_parent")
    removeUI(selector = "#geneDisplaySelectionPanel")
})

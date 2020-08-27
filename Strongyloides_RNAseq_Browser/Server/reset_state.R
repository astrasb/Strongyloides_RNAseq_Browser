## GW: Generate/Reset Gene File Upload ----
output$genefile_upload <- renderUI({
    input$resetGenes
    input$speciesGW
    input$tab
    fileInput('loadfile',
              h6('Gene Stable ID List (.csv)'),
              multiple = FALSE)
})

## GW: Generate Main Gene Input Panel ----
output$genePanelinputs <- renderUI({
    tagList(
        panel(
            id = "GeneInputBox",
            heading = tagList(h5(shiny::icon("fas fa-dna"), "Step 1: Input Genes / Keywords")),
            status = "primary",
            ### GeneID (text box)
            h5('Pick Genes', class = 'text-danger', style = "margin: 0px 0px 5px 0px"),
            p(tags$em('Users may type gene stable IDs or keywords that will be matched against Wormbase Parasite Gene Descriptions, known C. elegans homologs, InterPro terms, and an Ensembl Compara database of gene families. Please separate search terms by a comma. Users may also upload a .csv file containing comma-separated gene stable IDs.', style = "color: #7b8a8b")),
            p(tags$em(tags$b('Note: Please hit the Clear button between successive searches.', style = "color: #F39C12"))),
            textAreaInput('idtext',
                          h6('Gene Stable IDs or Keyword'),
                          rows = 5, 
                          resize = "vertical"),
            
            ### Upload list of GeneIDs
            uiOutput('genefile_upload'),
            
            ### Action Button
            actionButton('goGW',
                         'Submit',
                         #width = '50%',
                         icon = icon("fas fa-share"),
                         class = "btn-primary"),
            
            actionButton('resetGenes', 'Clear',
                         icon = icon("far fa-trash-alt"))
        )
    )
})

## GW: Clear Genes with Reset Button ----
observeEvent(input$resetGenes, {
    updatePickerInput(session, "displayedComparison_GW", choices = "", selected = "")
    vals$genelist <- NULL
    vals$HeatmapRowOrder <- NULL
    updateTextAreaInput(session,"idtext",value = "")
    updatePickerInput(session, "displayedGene", choices = "", selected = "")
    removeUI(selector = "#CPMPlotlydiv_parent")
    removeUI(selector = "#geneDisplaySelectionPanel")
    updateSelectInput(session, "selectContrast_GW", selected = "")
    updateSelectInput(session, "selectTarget_GW", selected = "")
    updateTextAreaInput(session,"multiContrasts_GW",value = "")
    
    
    vals$comparison_GW <- NULL
    vals$targetStage_GW <- NULL 
    vals$contrastStage_GW <- NULL
    vals$limmacontrast_GW <- NULL
    vals$target.contrast.options <- NULL
    vals$target.contrast.options <- vals$v.DEGList.filtered.norm$targets$group
})

## Reset Elements on Tab Change ----   
     observeEvent(input$tab, {
         vals$v.DEGList.filtered.norm <- NULL
         shinyjs::reset("GW")
         shinyjs::reset("LS")
         vals$genelist <- NULL
         vals$HeatmapRowOrder <- NULL
         updateTextAreaInput(session,"idtext",value = "")
         updatePickerInput(session, "displayedGene", choices = "", selected = "")
         removeUI(selector = "#CPMPlotlydiv_parent")
         removeUI(selector = "#geneDisplaySelectionPanel")
         updateSelectInput(session, "selectContrast_GW", selected = "")
         updateSelectInput(session, "selectTarget_GW", selected = "")
         updateTextAreaInput(session,"multiContrasts_GW",value = "")
         removeUI(selector = "#contrastDisplaySelectionPanel_GW")
         vals$comparison_GW <- NULL
         updateSelectInput(session, "selectContrast_LS", selected = "")
         updateSelectInput(session, "selectTarget_LS", selected = "")
         updateTextAreaInput(session,"multiContrasts_LS",value = "")
         vals$target.contrast.options <- NULL
     })

## GW: Generate Panel for Gene-wise Plots ----
output$genePlotPanel_GW <- renderUI({
    parse_ids()
    isolate({
        tagList(div(id = "CPMPlotlydiv_parent",
                    panel(
                        heading = tagList(h4(shiny::icon("fas fa-chart-bar"),
                                             "Gene Expression Across Life Stages")),
                        status = "primary",
                        
                        tagList(div(id = "GenePlotDiv",
                                    uiOutput("downloadbuttonsGenes")
                        ))
                    )
        ))
        
    })
    
})

## GW: Generate Responsive Selection for Gene to Display ----
output$geneDisplaySelection_GW <- renderUI({
    parse_ids()
    if (length(vals$genelist$geneID)>1) {
        choices <- c("All Genes", vals$genelist$geneID)
        selected <- "All Genes"
    } else choices <- vals$genelist$geneID
    tagList(div(id = "geneDisplaySelectionPanel",
                panel(
                    heading = tagList(h4(shiny::icon("fas fa-filter"),
                                         "Pick Gene to Display")),
                    status = "default",
                    pickerInput("displayedGene",
                                NULL, 
                                choices,
                                options = list(style = 'btn btn-primary'))
                )
    ))
    
    
})
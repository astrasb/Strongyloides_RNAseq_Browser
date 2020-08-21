## LS: Extract the Differentially Expressed Genes ----
pull_DEGs_LS <- reactive({
    req(vals$comparison_LS)
    req(vals$list.highlight.tbl_LS)
    
    if (isTruthy(input$displayedComparison_LS)){
        vals$displayedComparison_LS <- match(input$displayedComparison_LS,
                                             vals$comparison_LS, nomatch = 1)
    } else {vals$displayedComparison_LS <- 1}
    #### Volcano Plots
    vplot <- ggplot(vals$list.highlight.tbl_LS[[vals$displayedComparison_LS]]) +
        aes(y=-log10(BH.adj.P.Val), x=logFC) +
        geom_point(size=2) +
        geom_hline(yintercept = -log10(adj.P.thresh), 
                   linetype="longdash", 
                   colour="grey", 
                   size=1) + 
        geom_vline(xintercept = lfc.thresh, 
                   linetype="longdash", 
                   colour="#BE684D", 
                   size=1) +
        geom_vline(xintercept = -lfc.thresh, 
                   linetype="longdash", 
                   colour="#2C467A", 
                   size=1) +
        labs(title = paste0('Pairwise Comparison: ',
                            gsub('-',
                                 ' vs ',
                                 vals$comparison_LS[vals$displayedComparison_LS])),
             subtitle = paste0("grey line: p = ",
                               adj.P.thresh, "; colored lines: log-fold change = ", lfc.thresh),
             color = "GeneIDs") +
        theme_Publication() +
        theme(aspect.ratio=1/3)
    vplot
})

## LS: Volcano Plot, Generate UI ----
output$volcano_LS <- renderPlot({
    req(input$goLS)
    set_linear_model_LS()
    pull_DEGs_LS()
})

## LS: Save Volcano Plot ----
output$downloadVolcano_LS <- downloadHandler(
    filename = function(){
        paste('VolcanoPlot_',vals$comparison_LS[vals$displayedComparison_LS], '_',Sys.Date(),'.pdf', sep='')
    },
    content = function(file){
        pull_DEGs_LS()
        ggsave(file,width = 11, height = 8, units = "in", device = "pdf", useDingbats=FALSE)
    }
)

## LS: Volcano Hover Info ----
output$hover_info_LS <- renderUI({
    req(vals$displayedComparison_LS)
    pointer.df <- vals$list.highlight.tbl_LS[[vals$displayedComparison_LS]] %>%
        dplyr::mutate(log10.adj.P.Val = -log10(BH.adj.P.Val))
    
    hover <- input$plot_hover_LS
    point <- nearPoints(pointer.df, hover,
                        xvar = "logFC",
                        yvar = "log10.adj.P.Val",
                        threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$coords_img$y - top_pct * (hover$range$bottom - hover$range$top)
    
    left_px <- (hover$coords_img$x + left_pct)/ hover$img_css_ratio$x 
    #top_px <- (hover$coords_img$y - top_pct) / hover$imge_css_ratio$y
    
    #left_px <- hover$coords_img$x + hover$x
    #top_px <- hover$coords_img$y + hover$y
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    #"right:", hover$x + 10, "px; top:", hover$y + 10, "px;")
                    #"left:", hover$range$left, "px; bottom:", hover$range$bottom , "px;")
                    "left:", left_px + 50, "px; bottom:", top_px + 5, "px;")
    
    
    
    # actual tooltip created as wellPanel
    wellPanel(
        style = style,
        p(HTML(paste0("<b> GeneID: </b>",
                      point$geneID,
                      "<br/>",
                      "<b> Log FC: </b>",
                      round(point$logFC,digits = 2),
                      "<br/>",
                      "<b> p-value: </b>",
                      format(point$BH.adj.P.Val, digits = 3, scientific = TRUE))))
    )
})

## LS: Data Table of Differentially Expressed Genes ----
assemble_DEGs_LS <- reactive({
    req(vals$displayedComparison_LS)
    tS<- vals$targetStage_LS[vals$displayedComparison_LS,
                             ][vals$targetStage_LS[vals$displayedComparison_LS,
                                                   ]!=""]
    cS<- vals$contrastStage_LS[vals$displayedComparison_LS,
                               ][vals$contrastStage_LS[vals$displayedComparison_LS,
                                                       ]!=""]
    
    n_num_cols <- length(tS)*3 + length(cS)*3 + 5
    
    LS.datatable <- vals$list.highlight.tbl_LS[[vals$displayedComparison_LS]] %>%
        DT::datatable(rownames = FALSE,
                      caption = htmltools::tags$caption(
                          style = 'caption-side: top; text-align: left; color: black',
                          htmltools::tags$b('Differentially Expressed Genes in', 
                                            htmltools::tags$em('S. stercoralis'), 
                                            gsub('-',' vs ',vals$comparison_LS[vals$displayedComparison_LS])),
                          htmltools::tags$br(),
                          "Threshold: p < ",
                          adj.P.thresh, "; log-fold change > ",
                          lfc.thresh,
                          htmltools::tags$br(),
                          'Values = log2 counts per million'),
                      options = list(autoWidth = TRUE,
                                     scrollX = TRUE,
                                     scrollY = '300px',
                                     scrollCollapse = TRUE,
                                     order = list(n_num_cols-1, 
                                                  'desc'),
                                     searchHighlight = TRUE, 
                                     pageLength = 25, 
                                     lengthMenu = c("5",
                                                    "10",
                                                    "25",
                                                    "50",
                                                    "100"),
                                     initComplete = htmlwidgets::JS(
                                         "function(settings, json) {",
                                         paste0("$(this.api().table().container()).css({'font-size': '", "10pt", "'});"),
                                         "}"),
                                     columnDefs = list(
                                         list(
                                             targets = ((n_num_cols + 
                                                             1)),
                                             render = JS(
                                                 "function(data, row) {",
                                                 "data.toExponential(1);",
                                                 "}")
                                         ),
                                         list(
                                             targets = ((n_num_cols + 
                                                             4):(n_num_cols + 
                                                                     5)),
                                             render = JS(
                                                 "function(data, type, row, meta) {",
                                                 "return type === 'display' && data.length > 20 ?",
                                                 "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                 "}")
                                         ),
                                         list(targets = "_all",
                                              class="dt-right")
                                     )
                                     
                      )) 
    LS.datatable <- LS.datatable %>%
        DT::formatRound(columns=c(3:n_num_cols), 
                        digits=3)
    
    LS.datatable <- LS.datatable %>%
        DT::formatRound(columns=c(n_num_cols+2, 
                                  n_num_cols+9,
                                  n_num_cols+11), 
                        digits=2)
    
    LS.datatable <- LS.datatable %>%
        DT::formatSignif(columns=c(n_num_cols+1), 
                         digits=3)
    
    LS.datatable
})




output$tbl_LS <- renderDT ({
    req(input$goLS)
    DEG.datatable_LS<-assemble_DEGs_LS()
    DEG.datatable_LS
})

## LS: Save Excel Tables with DEG Tables ----
output$downloadbuttonLS <- renderUI({
    req(input$goLS)
    req(vals$comparison_LS)
    
    output$generate_excel_report_LS <- generate_excel_report(vals$comparison_LS, 
                                                             vals$list.highlight.tbl_LS)
    downloadButton("generate_excel_report_LS",
                   "Download DEG Tables",
                   class = "btn-primary")
})
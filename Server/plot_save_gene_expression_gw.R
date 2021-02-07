## GW: Plots of Gene Expression ----
generateGenePlot <- reactive({
    req(vals$genelist)
    req(input$displayedGene)
    req(vals$genelist.Log2CPM)
    req(vals$v.DEGList.filtered.norm)
    
    # Set gene to display
    if (input$displayedGene == "All Genes" | input$displayedGene == "Data Table"){
        vals$gene_of_interest <- vals$genelist$geneID
    } else vals$gene_of_interest <- input$displayedGene
    
    if (length(vals$gene_of_interest) == 1 && input$displayedGene != "Data Table") {
        setProgress(0.25)
        # Plot Log2CPM values for an individual gene
        gene_vals <- vals$genelist.Log2CPM %>%
            dplyr::filter(geneID == vals$gene_of_interest)
        setProgress(0.5)
        
        p<- suppressWarnings(ggplot(gene_vals) + 
                                 aes(x = life_stage, y = log2CPM, fill = life_stage) +
                                 geom_boxplot(show.legend = F, alpha = 0.7) +
                                 labs(y="log2 CPM expression", x = "Life Stage",
                                      title=paste("Log2 Counts per Million (CPM) Expression:",
                                                  vals$gene_of_interest),
                                      subtitle="filtered, normalized, variance-stabilized") +
                                 theme_Publication() + 
                                 theme(aspect.ratio=2/3) )
        setProgress(0.75)
        p
    } else if (input$displayedGene == "Data Table") {
        excluded.genes <- dplyr::anti_join(vals$submitted.genelist, 
                                           vals$genelist,
                                           by = "geneID") %>%
            left_join(vals$annotations, by = "geneID") # Add gene annotations
        setProgress(0.2)
        gene_vals <- vals$genelist.Log2CPM %>%
            dplyr::filter(geneID %in% vals$gene_of_interest) %>%
            dplyr::summarize(mean = mean(log2CPM), .groups = "drop_last") %>%
            pivot_wider(names_from = life_stage,
                        id_cols = geneID,
                        values_from = mean) %>%
            left_join(vals$annotations, by = "geneID") %>%
            dplyr::relocate(UniProtKB, Description, InterPro, GO_term,
                            In.subclade_geneID, In.subclade_percent_homology,
                            Out.subclade_geneID, Out.subclade_percent_homology,
                            Ce_geneID, Ce_percent_homology, .after = last_col())  %>%
            dplyr::relocate(ends_with("WBgeneID"), .before = In.subclade_geneID)%>%
            {suppressMessages(dplyr::full_join(.,excluded.genes))} 
        
        
        n_num_cols <- ncol(gene_vals)
        n_num_values <- nlevels(vals$v.DEGList.filtered.norm$targets$group)
        setProgress(0.4)
        gene_vals.datatable <- gene_vals %>%
            DT::datatable(rownames = FALSE,
                          options = list(autoWidth = TRUE,
                                         scrollX = TRUE,
                                         scrollY = '300px',
                                         scrollCollapse = TRUE,
                                         searchHighlight = TRUE, 
                                         pageLength = 10, 
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
                                                 targets = ((n_num_cols -
                                                                 8):(n_num_cols-7)),
                                                 render = JS(
                                                     "function(data, type, row, meta) {",
                                                     "return type === 'display' && data.length > 20 ?",
                                                     "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                     "}")
                                             ),
                                             list(targets = "_all",
                                                  class="dt-right")
                                         ),
                                         rowCallback = JS(c(
                                             "function(row, data){",
                                             "  for(var i=0; i<data.length; i++){",
                                             "    if(data[i] === null){",
                                             "      $('td:eq('+i+')', row).html('NA')",
                                             "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
                                             "    }",
                                             "  }",
                                             "}"
                                         )
                                         )
                                         
                          )
            )
        setProgress(0.8)
        gene_vals.datatable <-  gene_vals.datatable %>%
            DT::formatRound(columns=c(2:(n_num_values+1)), 
                            digits=3)
        
        gene_vals.datatable <-  gene_vals.datatable %>%
            DT::formatRound(columns=c(n_num_cols, 
                                      n_num_cols-2,
                                      n_num_cols-4), 
                            digits=2)
        
    } else {
        # Make a heatmap for all the genes using the Log2CPM values
        myheatcolors <- RdBu(75)
        
        diffGenes <- vals$diffGenes.df %>%
            dplyr::select(!geneID) %>%
            as.matrix()
        rownames(diffGenes) <- rownames(vals$v.DEGList.filtered.norm$E)
        subset.diffGenes<- diffGenes[vals$gene_of_interest,]
        
        setProgress(0.2)
        clustColumns <- hclust(as.dist(1-cor(subset.diffGenes, method="spearman")), method="complete")
        
        colnames(subset.diffGenes) <- paste0(vals$v.DEGList.filtered.norm$targets$group,
                                             "...",
                                             substr(vals$v.DEGList.filtered.norm$targets$samples, 
                                                    nchar(
                                                        as.character(vals$v.DEGList.filtered.norm$targets$samples[1]))-2, nchar(
                                                            as.character(vals$v.DEGList.filtered.norm$targets$samples[1])))
        )
        
        setProgress(0.4)
        
        clustRows <- hclust(as.dist(1-cor(t(subset.diffGenes), 
                                          method="pearson")), 
                            method="complete") 
        par(cex.main=1.2)
        vals$HeatmapRowOrder <- order.dendrogram(ladderize(as.dendrogram(clustRows)))
        vals$HeatmapColOrder <- order.dendrogram(ladderize(seriate_dendrogram(as.dendrogram(clustColumns),
                                                                              as.dist(1-cor(diffGenes, method="spearman")))))
        setProgress(0.6)
        
        hovertext <- as.data.frame(subset.diffGenes) %>%
            round(digits = 2)
        
        colnames(hovertext) <- vals$v.DEGList.filtered.norm$targets$samples
        hovertext[] <- lapply(seq_along(hovertext), function(x){
            paste0("GeneID: ", rownames(hovertext), "<br>",
                   "Log2CPM: ", hovertext[,x], "<br>",
                   "Life Stage: ", vals$v.DEGList.filtered.norm$targets$group[x],
                   "<br>",
                   "Sample: ", colnames(hovertext)[x])
        })
        
        showticklabels <- if(length(vals$gene_of_interest)<20){c(TRUE,TRUE)} else {c(TRUE,FALSE)}
        setProgress(0.8)
        p <- heatmaply(subset.diffGenes,
                       colors = rev(myheatcolors),
                       Rowv= ladderize(as.dendrogram(clustRows)),
                       Colv=ladderize(as.dendrogram(clustColumns)),
                       show_dendrogram = c(TRUE, TRUE),
                       showticklabels = showticklabels,
                       scale='row', #rows are scaled to have mean zero and standard deviation one. 
                       plot_method = "plotly",
                       branches_lwd = 0.2,
                       key.title = "Row-scaled Z Score",
                       cexRow=1.2, cexCol=1.2,
                       margins = c(100, 50, 10, 0),
                       colorbar_len = 0.5,
                       colorbar_ypos = 0.5,
                       colorbar_xpos = 1,
                       custom_hovertext = hovertext)
        setProgress(1)
        p
    }
})


## GW: Switch what type of GW Plot is produced ----
output$CPM <- renderPlot({
    req(input$displayedGene != "All Genes")
    req(input$displayedGene != "Data Table")
    withProgress(generateGenePlot(), message = "Loading Plot")
})

output$CPM.datatable <- renderDT({
    req(input$displayedGene == "Data Table")
    withProgress(generateGenePlot(), message = "Loading Data Table")
})

output$CPMPlotly <- renderPlotly({
    req(input$displayedGene == "All Genes")
    vals$genelist
    withProgress(generateGenePlot(), message = "Loading Heatmap")
})

observe({
    req(vals$genelist)
    req(input$displayedGene)
    vals$genelist
    
    if(input$displayedGene == "All Genes"){
        removeUI(
            selector = "#CPMPlotlydiv"
        )
        removeUI(
            selector = "#CPMdiv"
        )
        
        removeUI(
            selector = "#CPMTablediv"
        )
        
        insertUI(
            selector = '#GenePlotDiv',
            where = "beforeBegin",
            ui = tagList(div(id = "CPMPlotlydiv",
                             h5("Log2 Counts per Million (CPM) Expression Across Life Stages"),
                             plotlyOutput('CPMPlotly')
            ))
        )
        
        
        
    }else if(input$displayedGene == "Data Table"){
        removeUI(
            selector = "#CPMTablediv"
        )
        insertUI(
            selector = '#GenePlotDiv',
            where = "beforeBegin",
            ui = tagList(div(id = "CPMTablediv",
                             h5("Log2 Counts per Million (CPM) Expression Across Life Stages"),
                             DTOutput('CPM.datatable')
            ))
        )
        removeUI(
            selector = "#CPMdiv"
        )
        
        removeUI(
            selector = "#CPMPlotlydiv"
        )
        
    }else{
        removeUI(
            selector = "#CPMdiv"
        )
        insertUI(
            selector = '#GenePlotDiv',
            where = "beforeBegin",
            ui = tagList(div(id = "CPMdiv",
                             plotOutput('CPM')
            ))
        )
        removeUI(
            selector = "#CPMPlotlydiv"
        )
        
        removeUI(
            selector = "#CPMTablediv"
        )
    }
})

## GW: Save Gene Plots ----
output$downloadGenePlot <- downloadHandler(
    
    filename = function(){
        paste('GeneExpression_',input$displayedGene, '_',Sys.Date(),'.pdf', sep='')
    },
    content = function(file){
        withProgress ({
            
            if (input$displayedGene == "All Genes" | input$displayedGene == "Data Table") {
                setProgress(0.2)
                # Make a heatmap for all the genes using the Log2CPM values
                myheatcolors <- RdBu(75)
                
                diffGenes <- vals$diffGenes.df %>%
                    dplyr::select(!geneID) 
                colnames(diffGenes) <- vals$v.DEGList.filtered.norm$target$group
                diffGenes <- diffGenes %>%
                    as.matrix()
                
                rownames(diffGenes) <- rownames(vals$v.DEGList.filtered.norm$E)
                subset.diffGenes<- diffGenes[vals$gene_of_interest,]
                
                clustColumns <- hclust(as.dist(1-cor(subset.diffGenes, method="spearman")), method="complete")
                
                colnames(subset.diffGenes) <- paste0(vals$v.DEGList.filtered.norm$targets$group,
                                                     "...",
                                                     substr(vals$v.DEGList.filtered.norm$targets$samples, 
                                                            nchar(
                                                                as.character(vals$v.DEGList.filtered.norm$targets$samples[1]))-2, nchar(
                                                                    as.character(vals$v.DEGList.filtered.norm$targets$samples[1])))
                )
                
                setProgress(0.4)
                
                clustRows <- hclust(as.dist(1-cor(t(subset.diffGenes), 
                                                  method="pearson")), 
                                    method="complete") 
                
                par(cex.main=1.2)
                setProgress(0.6)
                
                showticklabels <- if(length(vals$gene_of_interest)<20){c(TRUE,TRUE)} else {c(TRUE,FALSE)}
                
                p<-ggheatmap_local(subset.diffGenes,
                                   colors = rev(myheatcolors),
                                   Rowv= ladderize(as.dendrogram(clustRows)),
                                   Colv=ladderize(as.dendrogram(clustColumns)),
                                   show_dendrogram = c(TRUE, TRUE),
                                   key.title = "Row Z Score",
                                   branches_lwd = 0.5,
                                   showticklabels = showticklabels,
                                   scale='row',
                                   cexRow=1.2, cexCol=1.2,
                                   main = "Log2 Counts per Million (CPM) Expression Across Life Stages")
                setProgress(0.8)
                ggsave(file,
                       plot = p,
                       width = 11,
                       height = 5,
                       device = "pdf",
                       useDingbats=FALSE)
                setProgress(1)
            } else {
                p<-generateGenePlot()
                
                ggsave(file,
                       plot = p,
                       width = 7,
                       height = 7,
                       device = "pdf",
                       useDingbats=FALSE)
            }
        },
        message = "Saving Plots")
    }
    
)

## GW: Save Excel Table with Gene Expression Data ----
output$downloadbuttonsGenes <- renderUI({
    req(vals$genelist)
    req(vals$genelist.Log2CPM)
    if (nrow(vals$genelist)>1) {req(vals$HeatmapRowOrder)}
    isolate({
        
        vals$genelist.Log2CPM$sampleID <- rep(as.character(vals$v.DEGList.filtered.norm$targets$samples),
                                              times =  nrow(vals$genelist))
        
        genelist.expression <-  vals$genelist.Log2CPM %>%
            left_join(vals$genelist, .,by = "geneID") %>%
            pivot_wider(id_cols = geneID,
                        names_from = c(life_stage,sampleID),
                        names_sep = "-",
                        values_from = log2CPM)
        
        if (isTruthy(vals$HeatmapRowOrder)){
            row.order <- tibble(OldOrder = vals$HeatmapRowOrder,
                                NewOrder = seq_along(vals$HeatmapRowOrder)) %>%
                dplyr::arrange(OldOrder)
            
            col.order <- tibble(OldOrder = vals$HeatmapColOrder,
                                NewOrder = seq_along(vals$HeatmapColOrder)) %>%
                dplyr::arrange(desc(NewOrder))
            
            genelist.expression <- genelist.expression %>%
                add_column(NewOrder = row.order$NewOrder, .before = "geneID") %>%
                dplyr::arrange(NewOrder) %>%
                dplyr::select(!NewOrder) %>%
                .[c(1, col.order$OldOrder+1)]
        }
        
        genelist.expression <- genelist.expression %>%
            list("User-selected Genes" = . )
        
        # Add back on genes that were submitted by the user but don't appear in the list of genes for which there is available data.
        excluded.genes <- dplyr::anti_join(vals$submitted.genelist,
                                           vals$genelist,
                                           by = "geneID")
        
        genelist.expression <-lapply(genelist.expression, function (x) {
            dplyr::full_join(x,excluded.genes, by = "geneID")
        })
        
        # Add gene annotations
        genelist.expression <-lapply(genelist.expression, function (x) {
            vals$annotations %>%
                dplyr::relocate(UniProtKB, Description, InterPro, GO_term,
                                In.subclade_geneID, In.subclade_percent_homology,
                                Out.subclade_geneID, Out.subclade_percent_homology,
                                Ce_geneID, Ce_percent_homology, .after = geneID) %>%
                dplyr::relocate(ends_with("WBgeneID"), .before = In.subclade_geneID)%>%
                dplyr::left_join(x,., by = "geneID")
            
        })
    })
    
    output$heatmap_data_download <- generate_excel_report(c("User-selected Genes"),
                                                          genelist.expression,
                                                          name = paste(input$selectSpecies_GW, "RNA-seq Gene Expression"),
                                                          filename_prefix = "Gene_Expression_Data_",
                                                          subtitle_prefix = "Log2CPM Expression:")
    
    tagList(
        downloadButton("downloadGenePlot",
                       "Download Plot as PDF",
                       class = "btn-primary"),
        
        downloadButton("heatmap_data_download",
                       "Download Expression Data",
                       class = "btn-primary")
    )
    
})
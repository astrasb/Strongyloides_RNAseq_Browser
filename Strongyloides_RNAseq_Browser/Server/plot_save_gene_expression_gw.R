## GW: Plots of Gene Expression ----
generateGenePlot <- reactive({
    req(vals$genelist)
    req(input$displayedGene)
    req(vals$genelist.Log2CPM)
    
    # Set gene to display
    if (input$displayedGene == "All Genes"){
        vals$gene_of_interest <- vals$genelist$geneID
    } else vals$gene_of_interest <- input$displayedGene
    
    
    if (length(vals$gene_of_interest) == 1) {
        # Plot Log2CPM values for an individual gene
        gene_vals <- vals$genelist.Log2CPM %>%
            dplyr::filter(geneID == vals$gene_of_interest)
        
        p<- ggplot(gene_vals) + 
            aes(x = life_stage, y = log2CPM, fill = life_stage) +
            geom_boxplot(show.legend = F, alpha = 0.7) +
            labs(y="log2 CPM expression", x = "Life Stage",
                 title=paste("Log2 Counts per Million (CPM):",
                             vals$gene_of_interest),
                 subtitle="filtered, normalized, variance-stabilized") +
            theme_Publication() + 
            theme(aspect.ratio=2/3) 
        p 
    } else {
        # Make a heatmap for all the genes using the Log2CPM values
        myheatcolors <- RdBu(75)
        diffGenes <- diffGenes.df %>%
            dplyr::select(!geneID) %>%
            as.matrix()
        rownames(diffGenes) <- rownames(v.DEGList.filtered.norm$E)
        clustColumns <- hclust(as.dist(1-cor(diffGenes, method="spearman")), method="complete")
        subset.diffGenes<- diffGenes[vals$gene_of_interest,]
        colnames(subset.diffGenes) <- paste0(v.DEGList.filtered.norm$targets$group, 
                                             ".", 
                                             rep(1:3,7))
        clustRows <- hclust(as.dist(1-cor(t(subset.diffGenes), 
                                          method="pearson")), 
                            method="complete") 
        par(cex.main=1.2)
        vals$HeatmapRowOrder <- order.dendrogram(ladderize(as.dendrogram(clustRows)))
        vals$HeatmapColOrder <- order.dendrogram(ladderize(seriate_dendrogram(as.dendrogram(clustColumns),
                                                                              as.dist(1-cor(diffGenes, method="spearman")))))
        
        hovertext <- as.data.frame(subset.diffGenes) %>%
            round(digits = 2)
        colnames(hovertext) <- v.DEGList.filtered.norm$targets$samples
        hovertext[] <- lapply(seq_along(hovertext), function(x){
            paste0("GeneID: ", rownames(hovertext), "<br>",
                   "Log2CPM: ", hovertext[,x], "<br>",
                   "Life Stage: ", v.DEGList.filtered.norm$targets$group[x],
                   "<br>",
                   "Sample: ", colnames(hovertext)[x])
        })
        
        showticklabels <- if(length(vals$gene_of_interest)<20){c(TRUE,TRUE)} else {c(TRUE,FALSE)}
        p <- heatmaply(subset.diffGenes,
                       colors = rev(myheatcolors),
                       Rowv= ladderize(as.dendrogram(clustRows)),
                       Colv=ladderize(as.dendrogram(clustColumns)),
                       show_dendrogram = c(TRUE, TRUE),
                       showticklabels = showticklabels,
                       scale='row', #rows are scaled to have mean zero and standard deviation one. 
                       plot_method = "plotly",
                       branches_lwd = 0.2,
                       key.title = "Row Z Score",
                       cexRow=1.2, cexCol=1.2,
                       xlab = "Note: column clustering performed across entire RNAseq dataset",
                       margins = c(100, 50, 10, 0),
                       colorbar_len = 0.5,
                       colorbar_ypos = 0.5,
                       colorbar_xpos = 1,
                       custom_hovertext = hovertext)
    }
})


## GW: Switch what type of GW Plot is produced ----
output$CPM <- renderPlot({
    req(input$displayedGene != "All Genes")
    generateGenePlot()
})

output$CPMPlotly <- renderPlotly({
    req(input$displayedGene == "All Genes")
    generateGenePlot()
})

observeEvent(input$displayedGene,{
    req(vals$genelist)
    if(input$displayedGene == "All Genes"){
        removeUI(
            selector = "#CPMPlotlydiv"
        )
        insertUI(
            selector = '#GenePlotDiv',
            where = "beforeBegin",
            ui = tagList(div(id = "CPMPlotlydiv",
                             h5("Log2 Counts Per Million (CPM) Expression Across Life Stages"),
                             withSpinner(plotlyOutput('CPMPlotly'),
                                         color = "#2C3E50")
            ))
        )
        removeUI(
            selector = "#CPMdiv"
        )
        
    }else{
        removeUI(
            selector = "#CPMdiv"
        )
        insertUI(
            selector = '#GenePlotDiv',
            where = "beforeBegin",
            ui = tagList(div(id = "CPMdiv",
                             withSpinner(plotOutput('CPM'),
                                         color = "#2C3E50")
            ))
        )
        removeUI(
            selector = "#CPMPlotlydiv"
        )
    }
},ignoreNULL = FALSE)

## GW: Save Gene Plots ----
output$downloadGenePlot <- downloadHandler(
    filename = function(){
        paste('GeneExpression_',input$displayedGene, '_',Sys.Date(),'.pdf', sep='')
    },
    content = function(file){
        withProgress ({
            
            if (input$displayedGene == "All Genes") {
                
                # Make a heatmap for all the genes using the Log2CPM values
                myheatcolors <- RdBu(75)
                
                diffGenes <- diffGenes.df %>%
                    dplyr::select(!geneID) %>%
                    as.matrix()
                rownames(diffGenes) <- rownames(v.DEGList.filtered.norm$E)
                colnames(diffGenes) <- as.character(v.DEGList.filtered.norm$targets$group)
                clustColumns <- hclust(as.dist(1-cor(diffGenes, method="spearman")), method="complete")
                subset.diffGenes<- diffGenes[vals$gene_of_interest,]
                colnames(subset.diffGenes) <- paste0(v.DEGList.filtered.norm$targets$group, 
                                                     "_", 
                                                     v.DEGList.filtered.norm$targets$samples)
                clustRows <- hclust(as.dist(1-cor(t(subset.diffGenes), 
                                                  method="pearson")), 
                                    method="complete") 
                par(cex.main=1.2)
                
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
                                   #margin = c(50,1000),
                                   main = "Log2 Counts Per Million (CPM) Expression Across Life Stages")
                ggsave(file, 
                       plot = p,
                       width = 11,
                       height = 5, 
                       device = "pdf", 
                       useDingbats=FALSE)
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
    req(vals$HeatmapRowOrder)
    isolate({
        
        vals$genelist.Log2CPM$sampleID <- rep(as.character(v.DEGList.filtered.norm$targets$samples), 
                                              times =  nrow(vals$genelist))
        
        row.order <- tibble(OldOrder = vals$HeatmapRowOrder,
                            NewOrder = seq_along(vals$HeatmapRowOrder)) %>%
            dplyr::arrange(OldOrder)
        
        col.order <- tibble(OldOrder = vals$HeatmapColOrder,
                            NewOrder = seq_along(vals$HeatmapColOrder)) %>%
            dplyr::arrange(desc(NewOrder))
        
        genelist.expression <-  vals$genelist.Log2CPM %>%
            left_join(vals$genelist, .,by = "geneID") %>%
            pivot_wider(id_cols = geneID,
                        names_from = c(life_stage,sampleID),
                        names_sep = "-",
                        values_from = log2CPM) %>%
            add_column(NewOrder = row.order$NewOrder, .before = "geneID") %>%
            dplyr::arrange(NewOrder) %>%
            dplyr::select(!NewOrder) %>%
            .[c(1, col.order$OldOrder+1)] %>%
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
            annotations %>%
            dplyr::relocate(UniProtKB, Description, InterPro, GO_term, Sr_geneID, Sr_WBgeneID, Sr_percent_homology, Ce_geneID, Ce_percent_homology, .after = geneID) %>%
            dplyr::left_join(x,., by = "geneID") 
                  
        })
    })
    
    output$heatmap_data_download <- generate_excel_report(c("User-selected Genes"), 
                                                          genelist.expression,
                                                          name = "S. stercoralis RNAseq Gene Expression",
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
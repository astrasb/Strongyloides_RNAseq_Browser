# This script loads species-specific data for the Strongyloides RNAseq Browser

# Load species data in Gene-wise tab ----
observeEvent(input$speciesGW, {
    input$selectSpecies_GW
    vals$genelist <- NULL
    vals$HeatmapRowOrder <- NULL
    vals$target.contrast.options <- NULL
    updateTextAreaInput(session,"idtext",value = "")
    updatePickerInput(session, "displayedGene", choices = "", selected = "")
    removeUI(selector = "#CPMPlotlydiv_parent")
    removeUI(selector = "#geneDisplaySelectionPanel")
    updateSelectInput(session, "selectContrast_GW", selected = "")
    updateSelectInput(session, "selectTarget_GW", selected = "")
    updateTextAreaInput(session,"multiContrasts_GW",value = "")
    vals$comparison_GW <- NULL
    
    species <- switch(input$selectSpecies_GW,
                      `S. stercoralis` = 'Ss',
                      `S. ratti` = 'Sr')
    
    withProgress({
        
        # Import a variance-stabilized DEGList created by voom transformation command.
        # Outputs: E = normalized CPMexpression values on the log2 scale
        load(file = paste0("./Data/",species,"_vDEGList"))
        vals$v.DEGList.filtered.norm <- v.DEGList.filtered.norm
        
        setProgress(value = .25)
        
        vals$target.contrast.options <- vals$v.DEGList.filtered.norm$targets$group
        # Import a tidy dataframe containing gene annotations for all genes in the genome (including those that are excluded from this database.)
        load(file = paste0("./Data/",species,"_geneAnnotations"))
        vals$annotations <- as_tibble(annotations, rownames = "geneID")
        
        setProgress(value = .5)
        
        # Parse vDEGList into a tibble containing Log2CPM information
        vals$Log2CPM<-v.DEGList.filtered.norm$E %>%
            as_tibble(rownames = "geneID")%>%
            setNames(nm = c("geneID", 
                            as.character(v.DEGList.filtered.norm$targets$group))) %>%
            pivot_longer(cols = -geneID,
                         names_to = "life_stage", 
                         values_to = "log2CPM") %>%
            group_by(geneID, life_stage)
        
        vals$diffGenes.df <- v.DEGList.filtered.norm$E %>%
            as_tibble(rownames = "geneID", .name_repair = "unique")
        
        setProgress(value = .75)
        
        ## Fit a linear model to the data ----
        vals$fit <- lmFit(v.DEGList.filtered.norm, v.DEGList.filtered.norm$design)
        
        setProgress(value = 1)
        
    }, message = "Loading Species Database")
    
    
})

# Load Species data in life stage tab
observeEvent(input$speciesLS, {
    input$selectSpecies_LS
    
    updateSelectInput(session, "selectContrast_LS", selected = "")
    updateSelectInput(session, "selectTarget_LS", selected = "")
    updateTextAreaInput(session,"multiContrasts_LS",value = "")
    
    species <- switch(input$selectSpecies_LS,
                      `S. stercoralis` = 'Ss',
                      `S. ratti` = 'Sr')
    
    withProgress({
        # Import a variance-stabilized DEGList created by voom transformation command.
        # Outputs: E = normalized CPMexpression values on the log2 scale
        load(file = paste0("./Data/",species,"_vDEGList"))
        vals$v.DEGList.filtered.norm <- v.DEGList.filtered.norm
        
        setProgress(value = .25)
        
        # Import a tidy dataframe containing gene annotations for all genes in the genome (including those that are excluded from this database.)
        load(file = paste0("./Data/",species,"_geneAnnotations"))
        vals$annotations <- as_tibble(annotations, rownames = "geneID")
        
        setProgress(value = .5)
        
        # Parse vDEGList into a tibble containing Log2CPM information
        vals$Log2CPM<-v.DEGList.filtered.norm$E %>%
            as_tibble(rownames = "geneID")%>%
            setNames(nm = c("geneID", 
                            as.character(v.DEGList.filtered.norm$targets$group))) %>%
            pivot_longer(cols = -geneID,
                         names_to = "life_stage", 
                         values_to = "log2CPM") %>%
            group_by(geneID, life_stage)
        
        vals$diffGenes.df <- v.DEGList.filtered.norm$E %>%
            as_tibble(rownames = "geneID", .name_repair = "unique")
        
        setProgress(value = .75)
        
        ## Fit a linear model to the data ----
        vals$fit <- lmFit(v.DEGList.filtered.norm, v.DEGList.filtered.norm$design)
        
        setProgress(value = 1)
    }, message = "Loading Species Database")
    
    
})






# This script loads species-specific data for the Strongyloides RNAseq Browser

# GW: Load species data in Gene-wise tab ----
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
                      `S. ratti` = 'Sr',
                      `S. papillosus` = "Sp",
                      `S. venezuelensis` = "Sv")
    
    withProgress({
        
        # Import a variance-stabilized DEGList created by voom transformation command.
        # Outputs: E = normalized CPMexpression values on the log2 scale
        load(file = paste0("./Data/",species,"_vDGEList"))
        vals$v.DEGList.filtered.norm <- v.DEGList.filtered.norm
        
        setProgress(value = .25)
        
        vals$target.contrast.options <- vals$v.DEGList.filtered.norm$targets$group
        # Import a tidy dataframe containing gene annotations for all genes in the genome (including those that are excluded from this database.)
        load(file = paste0("./Data/",species,"_geneAnnotations"))
        vals$annotations <- as_tibble(annotations, rownames = "geneID")
        
        setProgress(value = .5)
        
        # Parse vDGEList into a tibble containing Log2CPM information
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
        
        ## Fit a linear model to the data
        vals$fit <- lmFit(v.DEGList.filtered.norm, v.DEGList.filtered.norm$design)
        
        setProgress(value = 1)
        
    }, message = "Loading Species Database")
    
    
})

# GW: Load experiment information ----
StudyInfo.filename.GW <- reactive({
    req(input$selectSpecies_GW)
    
    species <- switch(input$selectSpecies_GW,
                      `S. stercoralis` = 'Ss',
                      `S. ratti` = 'Sr',
                      `S. papillosus` = "Sp",
                      `S. venezuelensis` = "Sv")
    
    Info.type <- switch(input$which.Experimental.Info.GW,
                        `Study Design` = '_studyDesign.txt',
                        `Log2CPM Gene Counts` = 'RNAseq_log2cpm_filtered_norm.csv',
                        `vDGEList` = "_vDGEList",
                        `Discarded Gene Counts` = "RNAseq_discardedGene_counts.csv")
    
    file.location <- switch(input$which.Experimental.Info.GW,
                            `Study Design` = './www/',
                            `Log2CPM Gene Counts` = './www/',
                            `vDGEList` = "./Data/",
                            `Discarded Gene Counts` = "./www/"
    )
    Info.file <- paste0(file.location,species, Info.type)
    Info.file
    
})

output$StudyInfo.panel.GW <- renderUI({
    output$StudyInfo.file.GW <- downloadHandler(
        filename = function() {
            Info.file <- StudyInfo.filename.GW()
            file.location <- switch(input$which.Experimental.Info.GW,
                                    `Study Design` = './www/',
                                    `Log2CPM Gene Counts` = './www/',
                                    `vDGEList` = "./Data/",
                                    `Discarded Gene Counts` = "./www/"
            )
            str_remove(Info.file, file.location)
        },
        content = function(file){
            Info.file <- StudyInfo.filename.GW()
            file.copy(Info.file, file)
        }
    )
    
    downloadButton("StudyInfo.file.GW","Download",
                   class = "btn-default")
})



# LS: Load species data in life stage tab ----
observeEvent(input$speciesLS, {
    input$selectSpecies_LS
    
    updateSelectInput(session, "selectContrast_LS", selected = "")
    updateSelectInput(session, "selectTarget_LS", selected = "")
    updateTextAreaInput(session,"multiContrasts_LS",value = "")
    
    species <- switch(input$selectSpecies_LS,
                      `S. stercoralis` = 'Ss',
                      `S. ratti` = 'Sr',
                      `S. papillosus` = "Sp",
                      `S. venezuelensis` = "Sv")
    
    withProgress({
        # Import a variance-stabilized DEGList created by voom transformation command.
        # Outputs: E = normalized CPMexpression values on the log2 scale
        load(file = paste0("./Data/",species,"_vDGEList"))
        vals$v.DEGList.filtered.norm <- v.DEGList.filtered.norm
        
        setProgress(value = .25)
        
        # Import a tidy dataframe containing gene annotations for all genes in the genome (including those that are excluded from this database.)
        load(file = paste0("./Data/",species,"_geneAnnotations"))
        vals$annotations <- as_tibble(annotations, rownames = "geneID")
        
        setProgress(value = .5)
        
        # Parse vDGEList into a tibble containing Log2CPM information
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
        
        ## Fit a linear model to the data
        vals$fit <- lmFit(v.DEGList.filtered.norm, v.DEGList.filtered.norm$design)
        
        setProgress(value = 1)
    }, message = "Loading Species Database")
    
    
})

# LS: Load experiment information ----
StudyInfo.filename.LS <- reactive({
    req(input$selectSpecies_LS)
    
    species <- switch(input$selectSpecies_LS,
                      `S. stercoralis` = 'Ss',
                      `S. ratti` = 'Sr',
                      `S. papillosus` = "Sp",
                      `S. venezuelensis` = "Sv")
    
    Info.type <- switch(input$which.Experimental.Info.LS,
                        `Study Design` = '_studyDesign.txt',
                        `Log2CPM Gene Counts` = 'RNAseq_log2cpm_filtered_norm.csv',
                        `vDGEList` = "_vDGEList",
                        `Discarded Gene Counts` = "RNAseq_discardedGene_counts.csv")
    
    file.location <- switch(input$which.Experimental.Info.LS,
                            `Study Design` = './www/',
                            `Log2CPM Gene Counts` = './www/',
                            `vDGEList` = "./Data/",
                            `Discarded Gene Counts` = "./www/"
    )
    Info.file <- paste0(file.location,species, Info.type)
    Info.file
    
})

output$StudyInfo.panel.LS <- renderUI({
    output$StudyInfo.file.LS <- downloadHandler(
        filename = function() {
            Info.file <- StudyInfo.filename.LS()
            file.location <- switch(input$which.Experimental.Info.LS,
                                    `Study Design` = './www/',
                                    `Log2CPM Gene Counts` = './www/',
                                    `vDGEList` = "./Data/",
                                    `Discarded Gene Counts` = "./www/"
            )
            str_remove(Info.file, file.location)
        },
        content = function(file){
            Info.file <- StudyInfo.filename.LS()
            file.copy(Info.file, file)
        }
    )
    
    downloadButton("StudyInfo.file.LS","Download",
                   class = "btn-default")
})




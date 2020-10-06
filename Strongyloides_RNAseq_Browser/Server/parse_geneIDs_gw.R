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
            p(tags$em(tags$b('Note: Please hit the Clear button if switching between typing and uploading inputs.', style = "color: #F39C12"))),
            textAreaInput('idtext',
                          h6('Gene Stable IDs or Keyword'),
                          rows = 5, 
                          resize = "vertical"),
            
            ### Upload list of GeneIDs
            uiOutput('genefile_upload'),
            
            ### Action Button
            actionButton('goGW',
                         'Submit',
                         icon = icon("fas fa-share"),
                         class = "btn-primary"),
            
            actionButton('resetGenes', 'Clear',
                         icon = icon("far fa-trash-alt"))
        )
    )
})

## GW: Parse Gene Inputs ----
parse_ids <- eventReactive(input$goGW,{
    vals$genelist <- NULL
    vals$HeatmapRowOrder <- NULL
    validate(
        need(isTruthy(vals$v.DEGList.filtered.norm), 
             "Please re-select a species for analysis")
    )
    
    validate(
        need({isTruthy(input$loadfile) | isTruthy(input$idtext)}, "Please input gene ids")
    )
    isolate({
        if (isTruthy(input$idtext)){
            if (any(grepl('SSTP|SRAE|SVE|SPAL', input$idtext, ignore.case = TRUE))){
                # Text input matches SSTP|SRAE|SVE|SPAL values
                genelist <- input$idtext %>%
                    gsub("\\s+", "", .) %>% #remove any number of whitespace
                    str_split(pattern = ",") %>%
                    unlist() %>%
                    as_tibble_col(column_name = "geneID")
            } else {
                
                # Assume the values here are keywords, search Description terms
                terms <- input$idtext %>%
                    str_split(pattern = ",") %>%
                    unlist()
                
                geneindex<-sapply(terms, function(y) {
                    grepl(gsub("^\\s+|\\s+$","",y), #remove any number of whitespace from start of end
                          vals$v.DEGList.filtered.norm$genes$Description,
                          ignore.case = TRUE)
                }) %>%
                    rowSums() %>%
                    as.logical()
                
                genelist <- vals$v.DEGList.filtered.norm$genes %>%
                    rownames_to_column(var = "geneID") %>%
                    dplyr::select(geneID)
                ensComp<- ensComp %>%
                    left_join(genelist, ., by = "geneID") %>%
                    dplyr::relocate(gs_name, geneID)
                
                geneindex.ensembl<-sapply(terms, function(y) {
                    gsub("^\\s+|\\s+$","",y) %>%
                        paste0("\\<",.,"\\>") %>%
                        grepl(., 
                              ensComp$gs_name,
                              ignore.case = TRUE)
                }) %>%
                    rowSums() %>%
                    as.logical()
                
                geneindex.Cehomologs<-sapply(terms, function(y) {
                    gsub("^\\s+|\\s+$","",y) %>%
                        paste0("\\<",.,"\\>") %>%
                        grepl(., 
                              vals$v.DEGList.filtered.norm$genes$Ce_geneID,
                              ignore.case = TRUE)
                }) %>%
                    rowSums() %>%
                    as.logical()
                
                geneindex.InterPro<-sapply(terms, function(y) {
                    gsub("^\\s+|\\s+$","",y) %>%
                        paste0("\\<",.,"\\>") %>%
                        grepl(., 
                              vals$v.DEGList.filtered.norm$genes$InterPro,
                              ignore.case = TRUE)
                }) %>%
                    rowSums() %>%
                    as.logical()
                
                geneindex <- geneindex | geneindex.ensembl | geneindex.Cehomologs | geneindex.InterPro
                
                genelist <- vals$v.DEGList.filtered.norm$genes %>%
                    rownames_to_column(var = "geneID") %>%
                    dplyr::select(geneID)
                genelist <- genelist$geneID[geneindex] %>%
                    as_tibble_col(column_name = "geneID")
            }
            
        } else if (isTruthy(input$loadfile)){
            file <- input$loadfile
            ext <- tools::file_ext(file$datapath)
            validate(need(ext == "csv", "Please upload a csv file"))
            suppressWarnings(
                genelist <- read.csv(file$datapath, 
                                     header = FALSE, 
                                     colClasses = "character", 
                                     strip.white = T) %>%
                    as_tibble() %>%
                    pivot_longer(cols = everything(), 
                                 values_to = "geneID") %>%
                    dplyr::select(geneID)
            )
        } 
        
        if (nrow(genelist) == 0){
            disable("goLifeStage_GW")
        } else {enable("goLifeStage_GW")}
        
        # Produces error message if genelist is empty
        validate(
            need(nrow(genelist) != 0, "No genes found, please try a new search")
        )
       
        # Save record of original genelist before filtering, removing rows that contain the word 'gene'
        vals$submitted.genelist <- genelist %>%
            dplyr::filter(!grepl("gene", geneID, ignore.case = T))
        
        # Remove genes from the list that aren't part of vals$Log2CPM
        # Ideally, this would trigger a notification to the user.
        genelist <- genelist %>%
            dplyr::filter(geneID %in% vals$Log2CPM$geneID)
        
        vals$genelist <- genelist
        vals$genelist.Log2CPM <- vals$Log2CPM %>%
            dplyr::filter(geneID %in% genelist$geneID)
    })
})
## GW: Parse Gene Inputs ----
parse_ids <- eventReactive(input$goGW,{
    vals$genelist <- NULL
    vals$HeatmapRowOrder <- NULL
    validate(
        need({isTruthy(input$loadfile) | isTruthy(input$idtext)}, "Please input gene ids")
    )
    
    isolate({
        if (isTruthy(input$idtext)){
            if (any(grepl('SSTP', input$idtext, ignore.case = TRUE))){
                # Text input matches SSTP values
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
                          v.DEGList.filtered.norm$genes$Description,
                          ignore.case = TRUE)
                }) %>%
                    rowSums() %>%
                    as.logical()
                
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
                              v.DEGList.filtered.norm$genes$Ce_geneID,
                              ignore.case = TRUE)
                }) %>%
                    rowSums() %>%
                    as.logical()
                
                geneindex.InterPro<-sapply(terms, function(y) {
                    gsub("^\\s+|\\s+$","",y) %>%
                        paste0("\\<",.,"\\>") %>%
                        grepl(., 
                              v.DEGList.filtered.norm$genes$InterPro,
                              ignore.case = TRUE)
                }) %>%
                    rowSums() %>%
                    as.logical()
                
                geneindex <- geneindex | geneindex.ensembl | geneindex.Cehomologs | geneindex.InterPro
                
                genelist <- v.DEGList.filtered.norm$genes %>%
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
        
        # Remove genes from the list that aren't part of Log2CPM
        # Ideally, this would trigger a notification to the user.
        genelist <- genelist %>%
            dplyr::filter(geneID %in% Log2CPM$geneID)
        
        vals$genelist <- genelist
        vals$genelist.Log2CPM <- Log2CPM %>%
            dplyr::filter(geneID %in% genelist$geneID)
    })
})
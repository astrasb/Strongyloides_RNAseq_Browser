## GW: Clear Comparison Selections
observeEvent(input$resetGW,{
    updateSelectInput(session, "selectContrast_GW", selected = "")
    updateSelectInput(session, "selectTarget_GW", selected = "")
    updateTextAreaInput(session,"multiContrasts_GW",value = "")
    
})

## Parse the inputs ----
parse_contrasts_GW <- eventReactive(input$goLifeStage_GW,{
    if (isTruthy(input$multiContrasts_GW)) {
        comparison <- input$multiContrasts_GW %>%
            gsub(" ", "", ., fixed = TRUE) %>%
            str_split(pattern = "[,;]") %>%
            unlist()
        targetStage<- comparison %>%
            str_split(pattern="-", simplify = T) %>%
            .[,1] %>%
            gsub("(", "", ., fixed = TRUE) %>%
            gsub(")", "", ., fixed = TRUE) %>%
            str_split(pattern = "\\+", simplify = T)
        
        contrastStage<-comparison %>%
            str_split(pattern="-", simplify = T) %>%
            .[,2] %>%
            gsub("(", "", ., fixed = TRUE) %>%
            gsub(")", "", ., fixed = TRUE)  %>%
            str_split(pattern = "\\+", simplify = T)
        
        comparison<- sapply(seq_along(comparison),function(x){
            tS <- as.vector(targetStage[x,]) %>%
                .[. != ""] 
            cS <- as.vector(contrastStage[x,]) %>%
                .[. != ""] 
            paste(paste0(tS, 
                         collapse = "+") %>%
                      paste0("(",.,")/",length(tS)),
                  paste0(cS, 
                         collapse = "+") %>%
                      paste0("(",.,")/",length(cS)),
                  sep = "-")
            
        })
        if (input$multipleContrastsYN_GW == T) {
            vals$multipleCorrection_GW <- T
        } else vals$multipleCorrection_GW <- F
    } else if (length(input$selectTarget_GW) > 1 | 
               length(input$selectContrast_GW) > 1) {
        targetStage <- rbind(input$selectTarget_GW)
        contrastStage <- rbind(input$selectContrast_GW)
        comparison <- paste(paste0(input$selectTarget_GW, 
                                   collapse = "+") %>%
                                paste0("(",.,")/",length(input$selectTarget_GW)), 
                            paste0(input$selectContrast_GW, 
                                   collapse = "+") %>%
                                paste0("(",.,")/",length(input$selectContrast_GW)), 
                            sep = "-")
        vals$multipleCorrection_GW <- F
    } else {
        targetStage <- rbind(input$selectTarget_GW)
        contrastStage <- rbind(input$selectContrast_GW)
        comparison <- paste(input$selectTarget_GW, 
                            input$selectContrast_GW, 
                            sep = "-")
        vals$multipleCorrection_GW <- F
    }
    
    vals$targetStage_GW <- targetStage 
    vals$contrastStage_GW <- contrastStage
    vals$limmacontrast_GW <- comparison
    vals$comparison_GW <- gsub("/[0-9]*","", comparison)
    
})

## GW: Generate Responsive Selection for Life Stage to Display ----
output$contrastDisplaySelection_GW <- renderUI({
    comparison <- parse_contrasts_GW()
    panel(
        heading = tagList(h4(shiny::icon("fas fa-filter"),
                             "Pick Contrast to Display")),
        status = "default",
        pickerInput("displayedComparison_GW",
                    NULL, 
                    comparison,
                    selected = comparison[[1]],
                    options = list(style = 'btn btn-primary'))
    )
})
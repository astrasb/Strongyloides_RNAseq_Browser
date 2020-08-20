## LS: Clear Comparison Selections ----
observeEvent(input$resetLS,{
    updateSelectInput(session, "selectContrast_LS", selected = "")
    updateSelectInput(session, "selectTarget_LS", selected = "")
    updateTextAreaInput(session,"multiContrasts_LS",value = "")
    
})

## LS: Pairwise comparisons Across Life Stages ----
parse_contrasts_LS<-eventReactive(input$goLS,{
    if (isTruthy(input$multiContrasts_LS)) {
        comparison <- input$multiContrasts_LS %>%
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
            gsub(")", "", ., fixed = TRUE) %>%
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
        if (input$multipleContrastsYN_LS == T) {
            vals$multipleCorrection_LS <- T
        } else vals$multipleCorrection_LS <- F
    } else if (length(input$selectTarget_LS) > 1 | 
               length(input$selectContrast_LS) > 1){
        targetStage <- rbind(input$selectTarget_LS)
        contrastStage <- rbind(input$selectContrast_LS)
        comparison <- paste(paste0(input$selectTarget_LS, 
                                   collapse = "+") %>%
                                paste0("(",.,")/",length(input$selectTarget_LS)), 
                            paste0(input$selectContrast_LS, 
                                   collapse = "+") %>%
                                paste0("(",.,")/",length(input$selectContrast_LS)), 
                            sep = "-")
        vals$multipleCorrection_LS <- F
    } else {
        targetStage <- rbind(input$selectTarget_LS)
        contrastStage <- rbind(input$selectContrast_LS)
        comparison <- paste(input$selectTarget_LS, 
                            input$selectContrast_LS, 
                            sep = "-")
        vals$multipleCorrection_LS <- F
    }
    
    vals$targetStage_LS <- targetStage 
    vals$contrastStage_LS <- contrastStage
    vals$limmacontrast_LS <- comparison
    vals$comparison_LS <- gsub("/[0-9]*","", comparison)
    browser()
    
})

## LS: Generate Responsive Selection for Life Stage to Display ----
output$contrastDisplaySelection_LS <- renderUI({
    comparison <- parse_contrasts_LS()
    
    panel(
        heading = tagList(h4(shiny::icon("fas fa-filter"),
                             "Pick Contrast to Display")),
        status = "default",
        pickerInput("displayedComparison_LS",
                    NULL, 
                    comparison,
                    options = list(style = 'btn btn-primary'))
    )
})

## LS: Generate Legend Explaining the Life Stages ----
output$lifeStageLegend_LS <- renderTable({
    lifestage_legend.df <- lifestage_legend %>%
        dplyr::filter(group %in% unique(v.DEGList.filtered.norm$targets$group)) %>%
        dplyr::rename(`Abbr.` = group, `Life Stage` = developmental_stage) %>%
        dplyr::arrange(`Abbr.`)
}, striped = T,
spacing = "xs", align = "l", bordered = T)

output$Legend_LS <- renderUI({
    req(vals$comparison_LS)
    panel(
        heading = tagList(h4(shiny::icon("fas fa-book-open"),
                             "Life Stage Legend")),
        status = "default",
        tableOutput("lifeStageLegend_LS")
    )
})
## GW: Clear Comparison Selections ----
observeEvent({input$resetGW
    input$tab},{
        updateSelectInput(session, "selectContrast_GW", selected = "")
        updateSelectInput(session, "selectTarget_GW", selected = "")
        updateTextAreaInput(session,"multiContrasts_GW",value = "")
        
    })

## GW: Generate Comparison Selection Boxes ----
output$pairwiseSelector_GW<- renderUI({
    req(vals$v.DEGList.filtered.norm)
    
    list(
        panel(
            # Select Life Stage Comparisons 
            heading = tagList(h5(shiny::icon("fas fa-sliders-h"), 
                                 "Step 2: Pick Life Stage Comparisons")),
            status = "primary",
            # Select Target Life Stage
            h5('A: Single Comparison', class = 'text-danger'),
            
            selectInput("selectTarget_GW",
                        h6("Select Target"),
                        choices = c('Choose one or more' = ''
                                    ,as.list(levels(vals$target.contrast.options))),
                        selectize = TRUE,
                        multiple = TRUE),
            # Select Contrast Life Stage
            selectInput("selectContrast_GW",
                        h6("Select Contrast"),
                        choices = c('Choose one or more' = ''
                                    ,as.list(levels(vals$target.contrast.options))),
                        selectize = TRUE,
                        multiple = TRUE),
            
            
            tags$hr(style="border-color: #2C3E50;"),
            h5('B: Multiple Comparisons', class = 'text-danger'),
            
            # Text Input for Multiple Contrasts
            textAreaInput('multiContrasts_GW',
                          (h6('Type comma-separated comparisons using format: (Target)-(Contrast)',
                              tags$br(),tags$em('e.g. iL3-PF, (iL3+iL3a)-(PF+FLF)', style = "color: #7b8a8b"))),
                          
                          rows = 5, 
                          resize = "vertical"),
            
            h6("Correct for Multiple Comparisons?"),
            switchInput(
                inputId = "multipleContrastsYN_GW",
                onLabel = "Yes",
                offLabel = "No",
                size = "small",
                onStatus = "success"
            ),
            
            tags$hr(style="border-color: #2C3E50;"),
            
            ### Action Button
            actionButton('goLifeStage_GW',
                         'Process',
                         icon = icon("fas fa-share"),
                         class = "btn-primary"),
            
            actionButton('resetGW', 'Clear',
                         icon = icon("far fa-trash-alt"))
        )
        
    )
})

## GW: Parse the inputs ----
parse_contrasts_GW <- eventReactive(input$goLifeStage_GW,{
    
    # Produces error message if target and contrasts are not different
    validate(
        need(input$selectTarget_GW != input$selectContrast_GW, "Target and Contrast selections are identical. Please select new options.")
    )
    
    req(isTruthy(input$multiContrasts_GW) || (isTruthy(input$selectTarget_GW) && isTruthy(input$selectContrast_GW)))
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
    req(input$goLifeStage_GW, vals$genelist)
    
    comparison <- parse_contrasts_GW()
    isolate({
        panel(
            heading = tagList(h5(shiny::icon("fas fa-filter"),
                                 "Pick Contrast to Display")),
            status = "default",
            pickerInput("displayedComparison_GW",
                        NULL, 
                        comparison,
                        selected = comparison[[1]],
                        options = list(style = 'btn btn-primary'))
        )
    })
})
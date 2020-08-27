## LS: Clear Comparison Selections ----
observeEvent({input$resetLS
    input$tab},{
        updateSelectInput(session, "selectContrast_LS", selected = "")
        updateSelectInput(session, "selectTarget_LS", selected = "")
        updateTextAreaInput(session,"multiContrasts_LS",value = "")
        
    })

## LS: Generate Comparison Selection Boxes
output$pairwiseSelector_LS<- renderUI({
    req(vals$v.DEGList.filtered.norm)
    
    list(
        panel(
            heading = tagList(h5(shiny::icon("fas fa-sliders-h"), 
                                 "Step 1: Pick Life Stage Comparisons")),
            status = "primary",
            h5('A: Single Comparison', class = 'text-danger', style = "margin: 0px 0px 10.5px 0px"),
            
            selectInput("selectTarget_LS",
                        h6("Select Target"),
                        choices = c('Choose one or more' = ''
                                    ,as.list(levels(vals$v.DEGList.filtered.norm$targets$group))),
                        selectize = TRUE,
                        multiple = TRUE),
            # Select Contrast Life Stage
            selectInput("selectContrast_LS",
                        h6("Select Contrast"),
                        choices = c('Choose one or more' = ''
                                    ,as.list(levels(vals$v.DEGList.filtered.norm$targets$group))),
                        selectize = TRUE,
                        multiple = TRUE),
            tags$hr(style="border-color: black;"),
            h5('B: Multiple Comparisons', class = 'text-danger'),
            # Text Input for Multiple Contrasts
            textAreaInput('multiContrasts_LS',
                          (h6('Type comma-separated comparisons using format: (Target)-(Contrast)',
                              tags$br(),tags$em('e.g. iL3-PF, (iL3+iL3a)-(PF+FLF)', style = "color: #7b8a8b"))),
                          rows = 5, 
                          resize = "vertical"),
            
            h6("Correct for Multiple Comparisons?"),
            switchInput(
                inputId = "multipleContrastsYN_LS",
                onLabel = "Yes",
                offLabel = "No",
                size = "small",
                onStatus = "success"
            ),
            
            tags$hr(style="border-color: #2C3E50;"),
            
            ### Action Button
            actionButton('goLS',
                         'Submit',
                         icon = icon("fas fa-share"),
                         class = "btn-primary"),
            
            actionButton('resetLS', 'Clear',
                         icon = icon("far fa-trash-alt"))
            
        )
    )
})

## LS: Pairwise comparisons Across Life Stages ----
parse_contrasts_LS<-eventReactive(input$goLS,{
    
    # Produces error message if target and contrasts are not different
    validate(
        need(input$selectTarget_LS != input$selectContrast_LS, "Target and Contrast selections are identical. Please select new options.")
    )
    
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
    
})

## LS: Generate Responsive Selection for Life Stage to Display ----
output$contrastDisplaySelection_LS <- renderUI({
    req(vals$v.DEGList.filtered.norm)
    input$resetLS
    input$speciesLS
    
    comparison <- parse_contrasts_LS()
    isolate({
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
})

## LS: Generate Legend Explaining the Life Stages ----
output$lifeStageLegend_LS <- renderTable({
    lifestage_legend.df <- lifestage_legend %>%
        dplyr::filter(group %in% unique(vals$v.DEGList.filtered.norm$targets$group)) %>%
        dplyr::rename(`Abbr.` = group, `Life Stage` = developmental_stage) %>%
        dplyr::arrange(`Abbr.`)
}, striped = T,
spacing = "xs", align = "l", bordered = T)

output$Legend_LS <- renderUI({
    req(vals$comparison_LS)
    req(vals$v.DEGList.filtered.norm)
    panel(
        heading = tagList(h5(shiny::icon("fas fa-book-open"),
                             "Life Stage Legend")),
        status = "default",
        tableOutput("lifeStageLegend_LS")
    )
})
generate_excel_report <- function(comparison, tbl,
                                  name = "RNAseq Differential Gene Expression",
                                  filename_prefix = "Stercoralis_RNAseq_Data_",
                                  subtitle_prefix = "Contrast:",
                                  multiplecorrection = "",
                                  filteredacross = "",
                                  DEGpattern = "",
                                  proportionexport = ""){
  
  temp <- downloadHandler(
    
    filename = function(){
      paste(filename_prefix,Sys.Date(),".xlsx",sep = "")
    },
    
    content = function(file){
      
      withProgress({
        removeModal() 
        
        # Generate Workbook ----
        to_download <<- createWorkbook()
        
        setProgress(.25)
        
        # Write Data to Workbook ----
        
        sapply(seq_along(comparison), function(y){
          
          # Create Tab
          addWorksheet(wb = to_download, sheetName = paste0("Sheet_",y))
          
          ## Write Header to Sheet
          writeData(
            to_download,
            sheet = y,
            x = c(
              name,
              paste(subtitle_prefix, comparison[[y]]),
              multiplecorrection,
              DEGpattern,
              proportionexport,
              filteredacross,
              paste0("Report generated on ", format(Sys.Date(), "%B %d, %Y"))
            )
          )
          
          ## Write Data to Sheet
          writeData(
            to_download,
            sheet = y,
            x = tbl[[y]],
            startRow = 9,
            startCol = 1,
            headerStyle = createStyle(
              textDecoration = "Bold",
              halign = "center",
              border = "bottom"
            ),
            keepNA = TRUE,
            na.string = "NA"
          )
          
          # Implement Styling ----
          ## Styling the title row
          addStyle(
            to_download,
            sheet = y,
            rows = 1,
            cols = 1:10,
            style = createStyle(
              fontSize = "14",
              textDecoration = "bold"
            )
          )
          
        })
        setProgress(.75)
        saveWorkbook(to_download, file)
        setProgress(1)
      },
      message = "Generating Excel Report"
      )
    }
  )
}
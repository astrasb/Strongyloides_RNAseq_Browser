generate_excel_report <- function(comparison, tbl,
                                  name = "S. stercoralis RNAseq Differential Gene Expression",
                                  subtitle_prefix = "Contrast:"){
   
temp <- downloadHandler(
  
    filename = function(){
        paste("Stercoralis_RNAseq_Data_",Sys.Date(),".xlsx",sep = "")
    },
    
    content = function(file){
        removeModal() 
        
        # Workbook
        to_download <<- createWorkbook()
        
        
        # Write Data
        
        sapply(seq_along(comparison), function(y){
            
            # Create Tab
            addWorksheet(wb = to_download, sheetName = comparison[[y]])
            
            ## Sheet header
            writeData(
                to_download,
                sheet = y,
                x = c(
                    name,
                    paste(subtitle_prefix, comparison[[y]]),
                    paste0("Report generated on ", format(Sys.Date(), "%B %d, %Y"))
                )
            )
            
            ## Sheet Data
            writeData(
                to_download,
                sheet = y,
                x = tbl[[y]],
                startRow = 5,
                startCol = 1,
                headerStyle = createStyle(
                    textDecoration = "Bold",
                    halign = "center",
                    border = "bottom"
                )
            )
            
            # Styling
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
            
        withProgress(
            saveWorkbook(to_download, file),
            message = "Generating Excel Report")
    }
)

}
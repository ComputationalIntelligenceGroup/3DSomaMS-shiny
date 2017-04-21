#'
#'
dl_table_update_df <- function (rv, input, output, session ){
 
  selected <- input$dl_table_colsel
  if( (nrow(rv$soma$characterization) > 0) && (length(selected)>1) ){
    
    # Filter and select
    df <- rv$soma$characterization[ !rv$soma$characterization$deleted, ]
    df <- df_applyfilters(df, rv$soma$characterization.filter)[,selected]
    toEscape <- which(colnames(df) != "from")
    colnames(df) <- prettyColnames(colnames(df))
    output$dl_table_dt <- renderDataTable(df, 
                                          escape = toEscape,
                                          options = list( pageLength = 10, lengthChange = FALSE,
                                                               info = FALSE, searching = FALSE, searchable = NULL))
    # Set upload button
    output$dl_table_save_ui <- renderUI(downloadButton("dl_table_save", label="Export as CSV"))
    
    closeAlert(session, "load_table_nochar")
  }
  else{
    if( (nrow(rv$soma$characterization) == 0) )
    createAlert(session,"dl_table_alert","load_table_nochar", title = "No data available",
                content = "No somas have been characterized yet. Go to Data loading -> preprocess to characterize somas or import a CSV table",
                style = "warning", dismiss = F, append = F )
    
    output$dl_table_dt <- renderDataTable(NULL, 
                                          options = list( pageLength = 10, lengthChange = FALSE,
                                                          info = FALSE, searching = FALSE, searchable = NULL))
    return (NULL)
  }
}

#'  
#' Control export button
dl_table_export <- function ( rv, output ){
  
  # Output may not exist yet
  tryCatch(
    # Export rv$soma$characterization table
    output$dl_table_save <- downloadHandler(
      
      filename = "characterization_table.csv",
      content = function(file){ write.csv(rv$soma$characterization, file=file, row.names = F)}
    ))
}

#'
#' Import button toggle
dl_table_importButtonEnabler <- function(input,session){
  updateButton(session, "dl_table_load", disabled = is.null(input$dl_table_file))
}

#'
#' Import CSV file
dl_table_importCSV <- function ( rv, input, output, session ){
  
  # Get file
  file <- input$dl_table_file
  withWarns <- F
  
  # Read csv
  toAdd <- NULL
  tryCatch(
    toAdd <- read.csv(file$datapath,stringsAsFactors=F),
    error = function(e){ createAlert(sesison, "dl_table_alert", "dl_table_imperr", title= "Error", content = e, style = "error", append = T)},
    warning = function(w){ createAlert(sesison, "dl_table_alert", NULL, title= "Warning", content = w, style = "warning", append = T); withWarns <- T}
  )
  if(is.null(toAdd))
    return (NULL)
  
  #Check for empty file
  if( nrow(toAdd) == 0 ){
    createAlert(sesison, "dl_table_alert", "dl_table_imperr", title= "Error", content = "Empty file", style = "error", append = T)
    return (NULL)
  }
  
  # Check that colnames match
  if( any(colnames(toAdd)!=colnames(rv$soma$characterization) ) ){
    createAlert(sesison, "dl_table_alert", "dl_table_imperr", title= "Error", content = "Column names do not match", style = "error", append = T)
    return (NULL)
  }
  
  # Check for dup name- type within the file
  if( nrow(unique(toAdd[,c("name","from")])) < nrow(toAdd) ){
    createAlert(sesison, "dl_table_alert", "dl_table_imperr", title= "Error", content = "Duplicated entries in the CSV file", style = "error", append = T)
    return (NULL)
  }
  
  # Check for dup name - type in table
  merged <- merge( rv$soma$characterization, toAdd, by=c("name","from") )
  if(nrow(merged)>0){
    createAlert(sesison, "dl_table_alert", NULL, title= "Warning", content = "Duplicated entries will be ignored", style = "warning", append = T)
    for( i in 1:nrow(merged)){
      toAdd <- toAdd[ toAdd$name != merged$name[i] & toAdd$from != merged$from[i],]
    }
    withWarns <- T
  }
  
  # NOW we can merge
  rv$soma$characterization <- rbind.data.frame( rv$soma$characterization, toAdd)
  
  createAlert(session, "dl_table_alert", NULL, title= "CSV Import finished", content = sprintf("%d new entries added to the table",nrow(toAdd)), 
              style = ifelse(withWarns,"warning","success"), append = T)
  
  updateButton(session, "dl_table_load", disabled = T)
}
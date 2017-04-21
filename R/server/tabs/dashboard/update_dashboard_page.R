
dash_update_vbox <- function(rv,output){

  write( "[CALL] UPDATE SUMMARY DASH VBOX" , file=stderr())
  
  # Get total values
  original <- sum( is.na(rv$soma$summary.data$simulation_id) & ( ! rv$soma$summary.data$deleted ) )
  model <- rv$soma$model
  simulation <- rv$soma$simulation
  
  total_value <- function(x){ 
    
    # Get deleted 
    del <- sapply(x,function(y)y$deleted)
    if(!is.logical(del)) del<-0
    else del <- sum(del)
    
    # Return length - deleted
    return( length(x) - del )
  }
  
  # Create vbox total neurs
  create_vbox(output, "dash_vbox_neur", original, "Loaded neurons", color ="blue", icon("files-o") )
  create_vbox(output, "dash_vbox_mod", total_value(model), "Models", color ="green", icon("stats", lib="glyphicon"))
  create_vbox(output, "dash_vbox_sim", total_value(simulation), "Simulations", color ="orange", icon("gears"))
  
}


dash_update_summary <- function(rv, output){
  df <- df_applyfilters(rv$soma$summary.data, rv$soma$summary.data.filter )[,1:7]
  colnames( df ) <- c("Name", "Package", "Simulation", "Repaired", "Segmented", "Repaired & Segmented", "Characterized")
  output$dash_summary <- renderDataTable(df, 
                                         escape = c(1,2),
                                         options = list( pageLength = 10, lengthChange = FALSE,
                                                             info = FALSE, searching = FALSE, searchable = NULL))
}
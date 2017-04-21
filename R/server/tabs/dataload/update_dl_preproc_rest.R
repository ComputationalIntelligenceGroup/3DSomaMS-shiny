#' Preproc general controls

# Disable and enables dl_pproc_run button
update_dl_preproc_button <- function(session, input){
    updateButton(session, "dl_pproc_run", disabled = ( !( length(input$dl_pproc_op) > 0 ) ) )
}

# Run Rep / seg etc processes
run_dl_preproc_processes <- function(rv, session, input, output){
  
  log <- c("[INFO] Starting process...")
  output$dl_pproc_log <- renderText( paste0(log,collapse = "\n" ) )
  errors <- F
  
  # Get options
  processes <- isolate(input$dl_pproc_op)
  
  # Get files
  
  # Extract soma name and type
  somas <- extract_somalist_form_selector( isolate(input$dl_pproc_selector) )
  
  # Remove "root" somas
  toRemove <- NULL
  for(i in 1:length(somas) ) if( somas[[i]]$type == "root" ) toRemove <- c(toRemove,-i)
  if(!is.null(toRemove))
    somas <- somas[toRemove]
  
  totalPocs <- (length(somas) * length(processes))+1
  
  createAlert(session, "dl_pproc_alert", alertId = "dl_pproc_infoalert", dismiss = F, append = F,
              title = "Please wait...", content = "Somas are being processed. Each soma can take up to 5 minutes")
  
  withProgress(message = 'Running:', value = 0, {
    
  # Apply for each element in the list
  sapply( somas, function(x, rv, procs){
    
      soma <- x
    
      soma.real <- rv$files$real[[soma$name]]
      
      # Fail if we cannot find the soma
      if( is.null(soma.real)  ){
        log <<- c(log, sprintf("[%s] [ERROR] Soma entry not found." ,soma$name))
        errors <- T
        return (NULL)
      }
      
      # Get data (raw, repaired, segmented, rep_and_seg )
      data <- switch(soma$type,
                     raw = soma.real$original$data,
                     repaired = soma.real$repaired$data,
                     segmented = soma.real$segmented$data,
                     rep_and_seg = soma.real$rep_and_seg$data)
      
      # Repair process
      if( "Repair" %in% procs ){
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/(totalPocs), detail = sprintf("%s %s", "Repair", soma$name))
        
        # We can only repair original somas
        if( soma$type != "raw"){
          log <<- c(log, sprintf("[%s] [WARNING] Reparation can only be applied to unprocessed somas (SKIP)." ,soma$name))
          errors <- T
        }
        else if( !is.null(soma.real$repaired) ){
            log <<- c(log, sprintf("[%s] [WARNING] Soma already repaired." ,soma$name))
            errors <- T
        }
        else{
            repairedName <- NULL
            # Execute repair operation
            tryCatch(
                   # Also remove spaces from name since meshlab argument parsing is soooooooooooo bad
                   repairedName <-repair.soma3d( soma.real$original$data, name = gsub(" ","_",soma$name) ),
                  warning = function(w){ log <<- c(log, sprintf("[%s] [WARNING] %s." ,soma$name, w)); errors <- T },
                  error = function(e) { log <<- c(log, sprintf("[%s] [ERROR] %s." ,soma$name, e)); errors <- T }
            )
                
            # If we have a valid path
            if(!is.null(repairedName)){
              storage_rv_add_reparation( repairedName, soma$name, rv )
              log <<- c(log, sprintf("[%s] Reparation complete" ,soma$name))
              
              # Now data is the repaired file
              data <- repairedName
              soma$type <- "repaired"
            }
            else{
              # Stop process
              return (NULL)
            }
        }
      }
      
      # Segmentation
      if ( "Segment" %in% procs){
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/(totalPocs), detail = sprintf("%s %s", "Segment", soma$name))
        
        if( soma$type != "raw" && soma$type != "repaired"){
          log <<- c(log, sprintf("[%s] [WARNING] Segmentation can only be applied to unprocessed or repaired somas." ,soma$name))
          errors <- T
        }
        else if( ( soma$type == "raw" ) && ( !is.null(soma.real$segmented) ) ){
          log <<- c(log, sprintf("[%s] [WARNING] Soma already segmented" ,soma$name))
          errors <- T
        }
        else if( ( soma$type == "repaired" ) && ( !is.null(soma.real$rep_and_seg) ) ){
          log <<- c(log, sprintf("[%s] [WARNING] Soma already segmented" ,soma$name))
          errors <- T
        }
        else{
          
          segmented <- NULL
          # Execute repair operation
          tryCatch(
            # Also remove spaces from name since meshlab argument parsing is soooooooooooo bad
            segmented <-segment.soma3d( data, name = gsub(" ","_",soma$name) ),
            warning = function(w){ log <<- c(log, sprintf("[%s] [WARNING] %s." ,soma$name, w)) ; errors <- T },
            error = function(e) { log <<- c(log, sprintf("[%s] [ERROR] %s." ,soma$name, e)) ; errors <- T }
          )
          
          if(!is.null(segmented)){
            log <<- c(log, sprintf("[%s] Segmentation complete" ,soma$name))
            
            # Now data is the repaired file
            data <- segmented
            if(soma$type == "raw" ){ storage_rv_add_segmentation( data, soma$name,rv) ; soma$type <- "segmented"} 
            else if(soma$type == "repaired" ){ storage_rv_add_repandseg( data, soma$name,rv) ; soma$type <- "rep_and_seg" } 
            
          }
        }
      }
      
      # Characterization
      if ("Characterize" %in% procs){
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/(totalPocs), detail = sprintf("%s %s", "Characterize", soma$name))
        
        if( any( (rv$soma$characterization$name == soma$name) & (rv$soma$characterization$from == soma$type ) ) ){
          log <<- c(log, sprintf("[%s] [WARNING] Soma already characterized" ,soma$name))
          errors <- T
        }
        else{
          # Execute characterization operation
          characterization <- NULL
          tryCatch(
            # Also remove spaces from name since meshlab argument parsing is soooooooooooo bad
            characterization <- characterize.soma3d( data, name = soma$name ),
            # warning = function(w){ log <<- c(log, sprintf("[%s] [WARNING] %s." ,soma$name, w)) },
            error = function(e) { log <<- c(log, sprintf("[%s] [ERROR] %s." ,soma$name, e)) ; errors <- T }
          )
          
          if(!is.null(characterization)){
            log <<- c(log, sprintf("[%s] Characterization complete" ,soma$name))
            
            # Now data is the repaired file
            storage_rv_add_characterization( characterization, soma$name , soma$type, rv)
          }
        }
      }
    
  },rv,processes)
  
  incProgress(1/(totalPocs+1), detail = "Cleaning up..")
    
  }) # With progresss

  # Dump log
  output$dl_pproc_log <- renderText( paste0(log,collapse = "\n" ) )
  createAlert(session, "dl_pproc_alert", "end_pproc_alert",
              dismiss = T, append = F,
              title = "Process finished",
              style = ifelse(errors,"warning","success"),
              content = paste("Process finished", ifelse(errors,"with errors, please check process log on the right side","successfully")))
  
  return (NULL)
}
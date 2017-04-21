
dl_update_summary_vbox <- function(rv, output){
  
  write( "[CALL] UPDATE SUMMARY DL VBOX" , file=stderr())
 
  # Get everything from summary table
  sum.data <- rv$soma$summary.data
  
  repaired  <- sum( (sum.data$repaired) & !(sum.data$deleted) )
  segmented <- sum( (sum.data$segmented) & !(sum.data$deleted) )
  repandseg <- sum( (sum.data$rep_and_seg) & !(sum.data$deleted) )
  charact <- sum( (sum.data$characterized) & !(sum.data$deleted) )
  
  # Create vbox total neurs
  create_vbox(output, "dl_vbox_repaired", repaired, "Repaired", icon=icon("wrench"), color ="blue", width = 3 )
  create_vbox(output, "dl_vbox_oproc", segmented, "Segmented (but not repaired)", icon=icon("cut"), color ="green",  width = 3)
  create_vbox(output, "dl_vbox_repproc", repandseg, "Repaired and Segmented", icon=icon("thumbs-o-up"), color ="orange",  width = 3)
  create_vbox(output, "dl_vbox_char", charact , "Characterized", icon=icon("th"), color ="purple",  width = 3)
}

dl_update_summary_table <- function(rv, output){
  
  # Summary df
  summary.df <- data.frame(
    Name = character(0),
    Package = character(0),
    "Original faces" = numeric(0),
    "Original vertices" = numeric(0),
    "Rep and seg faces" = numeric (0),
    "Rep and seg vertices" = numeric(0),
    stringsAsFactors = F
  )
  
  # For each "real" file create a new row
  sapply(names(rv$files$real), function(name){
    package <- rv$files$real[[name]]$package
    original.faces <- nrow(rv$files$real[[name]]$original$data$faces)
    original.vertices <- nrow(rv$files$real[[name]]$original$data$vertices)
    if(!is.null(rv$files$real[[name]]$rep_and_seg)){
      ras.faces <- nrow(rv$files$real[[name]]$rep_and_seg$data$faces)
      ras.vertices <- nrow(rv$files$real[[name]]$rep_and_seg$data$vertices)
    }
    else{
      ras.faces <- NA
      ras.vertices <- NA
    }
    
    # Add row
    summary.df[nrow(summary.df)+1,] <<- c(name,package,original.faces,original.vertices,ras.faces,ras.vertices)
  })
  
  colnames(summary.df) <- c("Name","Package","Original faces", "Original vertices", "Rep. & Seg. faces", "Rep. & Seg. vertices")
  output$dl_summary <- renderDataTable(summary.df,
                                       options = list( pageLength = 10, lengthChange = FALSE,
                                                       info = FALSE, searching = FALSE, searchable = NULL))
  
}
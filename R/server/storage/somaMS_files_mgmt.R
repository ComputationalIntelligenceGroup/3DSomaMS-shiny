
storage_rv_add_file <- function( file, rv, package ){
  
  ## If error (duplicated, etc return string with error ) else return NULL
  
  # Check for dups
  name <- file$data$name
  
  if( !is.null( isolate( rv$files$real[[name]] ) ) ){
    ## Dup filename
    return( sprintf(" Duplicated filename %s ", name) )
  }
  
  ## Read VRML or PLY
  if( tolower(file$data$ext) == "ply" ){
    # add extension
    file.rename(file$data$datapath, paste0(file$data$datapath,".ply"))
    vcg_df <- vcgImport( paste0(file$data$datapath,".ply") )
    df <- list(vertices = t(vcg_df$vb[1:3,]), faces = t(vcg_df$it) )
  }
  else
    df <- read_VRML( file$data$datapath)

  if( is.null(df) ){
    ## Parsing error
    return( sprintf(" Errors while parsing %s ", name) )
  }
  
  ## Add to files.real
  rv$files$real[[name]] <- list()
  rv$files$real[[name]]$original <- list(from = file$data$datapath,
                                         ext = file$data$ext,
                                         data = df,
                                         metadata = file$metadata$datapath)
  rv$files$real[[name]]$package <- package
  
  ## Add to data.summary
  rv$soma$summary.data[nrow( isolate(rv$soma$summary.data) ) +1, ] <-
                                  list( name = name,
                                        package = package,
                                        simulation_id = NA,
                                        repaired = F,
                                        segmented = F,
                                        rep_and_seg = F,
                                        characterized = F,
                                        char_uptodate = F,
                                        deleted = F) 
  
  ## Return
  return ( NULL )
}

storage_rv_add_reparation <- function( reparation_path, name , rv ){
  
  # Get root
  base <- isolate(rv$files$real[[name]])
  if(is.null(base)) 
    stop(" Missing file name")
  
  #TODO: ADD METADATA!
  
  # Add to $repaired
  rv$files$real[[name]]$repaired <- list( from = "raw",
                                          data = reparation_path,
                                          data.path = reparation_path,
                                          metadata  =NULL )
  
  # Update summary data
  rv$soma$summary.data$repaired[ isolate(rv$soma$summary.data$name) == name] <- T
  
  return ( NULL )
}

storage_rv_add_segmentation <- function( segmented, name , rv ){
  
  # Get root
  base <- isolate(rv$files$real[[name]])
  if(is.null(base)) 
    stop(" Missing file name")
  
  #TODO: ADD METADATA!
  
  # Add to $repaired
  rv$files$real[[name]]$segmented <- list( from = "raw",
                                          data = segmented,
                                          metadata  =NULL )
  
  # Update summary data
  rv$soma$summary.data$segmented[ isolate(rv$soma$summary.data$name) == name] <- T
  
  return ( NULL )
}


storage_rv_add_repandseg <- function( segmented, name , rv ){
  
  # Get root
  base <- isolate(rv$files$real[[name]])
  if(is.null(base)) 
    stop(" Missing file name")
  
  #TODO: ADD METADATA!
  
  # Add to $repaired
  rv$files$real[[name]]$rep_and_seg <- list( from = "repaired",
                                           data = segmented,
                                           metadata  =NULL )
  
  # Update summary data
  rv$soma$summary.data$rep_and_seg[ isolate(rv$soma$summary.data$name) == name] <- T
  
  return ( NULL )
}

storage_rv_add_characterization <- function ( row , name, from ,rv){
  
  # Get root
  base <- isolate(rv$files$real[[name]])
  if(is.null(base)) 
    stop(" Missing file name")
  
  # Get package
  package <- rv$soma$summary.data$package[ isolate(rv$soma$summary.data$name) == name ]
  simulation_id <- rv$soma$summary.data$simulation_id[ isolate(rv$soma$summary.data$name) == name ]
  
  ## Add to characterization
  rv$soma$characterization[nrow( isolate(rv$soma$characterization) ) +1, ] <- c(name = name, 
                                                                                from = from, 
                                                                                package = package, 
                                                                                simulation_id = simulation_id,
                                                                                deleted = F, 
                                                                                row[1,])

  # Update summary data
  rv$soma$summary.data$characterized[ isolate(rv$soma$summary.data$name) == name] <- T
  
  return ( NULL )
}

storage_rv_add_model <- function ( name, bn, cluster, params ,rv){
  
  rv$soma$model[[name]] <- list(
    bn = bn,
    cluster = cluster,
    params = params
  )
  
}

storage_rv_add_simulation <- function(rv, name, model, nsomas, cluster){
  rv$soma$simulation[[name]] <- list( model = model, nsomas = nsomas, cluster = cluster )
}

storage_rv_add_simulatedSoma <- function(rv, name, simid, mesh){
  
  # Add to files.simulated
  rv$files$simulated[[name]]<- list(     data = mesh,
                                         metadata = NULL,
                                         simulation_id = simid)
  
  # Add to summary
  rv$soma$summary.data[nrow( isolate(rv$soma$summary.data) ) +1, ] <-
    list( name = name,
          package = NA,
          simulation_id = simid,
          repaired = F,
          segmented = F,
          rep_and_seg = F,
          characterized = T,
          char_uptodate = F,
          deleted = F) 
}

storage_rv_add_simulation_chartable <- function(rv, names, simulation_id, chartable ){
  
  # New columns for chartable
  chartable_updated <- cbind(name = names, from = "sim", package = NA, simulation_id = simulation_id, deleted = F, chartable )
  
  # Update
  rv$soma$characterization<- rbind(isolate(rv$soma$characterization),chartable_updated)
}
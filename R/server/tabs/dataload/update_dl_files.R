manage_file_upload <- function(rv, input, output, session){

  write( "[CALL] MANAGE FILE UPLOAD" , file=stderr())
  
  files <- isolate(input$dl_file)
  package <- isolate(input$dl_package)
  
  # Check null values
  # if files is null... nothing to do here
  if(is.null(files)) return (NULL)
  
  # Use files as list instead of df
  files <- unlist(apply(files, 1, list), recursive = FALSE)
  files <- lapply(files, as.list)
  
  # Package null
  if (!is.null(package) && package == "" ) package <- NA
  
  # Remove any previous alert
  createAlert(session,"dl_files_alert",alertId="dl_waitmsg",title="Please wait",
              content="The uploading process can take few minutes ...",append = F, dismiss = F)
  
  # Check file formats
  files <- dl_check.filetypes(files, session)
  
  # Extract zip files
  files <- dl_decompress_files(files, session)
  
  # Create pairs containing only: name & datapath
  files <- dl_onlyFileNames(files)
  
  # Create metadata pairs
  files <- dl_metadataPairs(files, session)
  
  # Call read_VRML or read_PLY function  and add to files.original (done this way so it can be executed in bg, also new tasks should be added)
  dl_readFiles_add( files, rv, session, output, package )
  
  closeAlert(session,"dl_waitmsg")
  
  # Disable button (avoid double-upload)
  updateButton(session,"dl_upstart",disabled = T)
  
  return( NULL )
}

dl_update_button <- function(session){
  updateButton(session,"dl_upstart",disabled = F)
}

#dl_create_file_alert <- function(output, title=NULL, content = NULL, style = c("info","success","warning","danger"), dismiss = T, append = T){
#  output$dl_files_alert
#  
#}

# Check that given files are actually valid
dl_check.filetypes <- function(files, session){
  
   # Check types 
   #res <- sapply(files, function(x) x$type %in% infiles.accepted.types )
   accepted.files <- files
  # rejected.files <- list()
   
   # Create alerts for rejected types
  # if(length(rejected.files) > 0){
  #   lapply(rejected.files, function(x){
  #     createAlert(session,"dl_files_alert",content = sprintf("File %s rejected. File type %s is not accepted", x$name, x$type ))
  #   })
   #}
   
   return (accepted.files)
}

# Decompress all zipped files
dl_decompress_files <- function(files, session){
  
  res <- sapply(files, function(x) x$type %in% infiles.zip.types )
  zipped.files <- files[res]
  unzipped.files <- files[!res]
  
  # Try to unzip zip files
  tmp.files <- lapply(zipped.files, function(x,session){
    v <- tryCatch({
        x$unzipped <- unzip(x, overwrite = F, exdir = tempdir())
        return(x)
      },
      error = function(e){ 
        createAlert(session,"dl_files_alert",content = sprintf("Error while unzipping file %s", x$name, x$type )) 
        return (NULL)
      })
  })
  
  # append and return
  return( c(tmp.files, unzipped.files) )
}

# Extracts file name, extension and datapath for given files
dl_onlyFileNames <- function(files){
  
  vals <- lapply( files, function(x){
    if(is.null(x$unzipped))
      return(list( datapath = x$datapath, name = basename(file_path_sans_ext(x$name)), ext = file_ext(x$name) ))
    else{
      ## Repeat the process for each one
      return( sapply( x$unzipped, function(x){ list( datapath = x, name = basename(file_path_sans_ext(x)), ext = file_ext(x))} ))
    }
  })
}

# Pairs data and metadata files
dl_metadataPairs <- function (files, session) {
  
  paired <- list()
  lapply( files , function(x){
    # Check list el
    if(is.null(paired[[x$name]])){
      paired[[x$name]] <<- list()
    }
    
    # Metadata files
    if( tolower(x$ext) == "xml") paired[[x$name]]$metadata <<- x
    else{
      if( is.null( paired[[x$name]]$data) ) paired[[x$name]]$data <<- x
      else{
        # Create alert
        createAlert(session,"dl_files_alert",style="warning", content = sprintf("Duplicated files with name %s. %s is discarded", x$name, x$datapath)) 
      }
    }
  })
  
  # Check for metadata files wo data files
  paired <- lapply( paired, function(x){
    if( is.null(x$data) ){
      createAlert(session,"dl_files_alert",style="error", content = sprintf("No data file for metadata file %s", x$metadata$name))
      return( NULL )
    } else{
      return ( x )
    }
  })
  
  return ( paired )
}

dl_readFiles_add <- function (files, rv, session, output, package){
  
  nfiles <- length(files)
  loaded <- 0
  color <- "blue"
  nerror <- 0
  
  # First step: create new "task" (get number of files )
  #h_task_menu.add( rv, "upload", text = "Uploading files", color = color, value = 0 )
  withProgress(message = 'Processing:', value = 0, {
    
  # Second step: Iterate
  lapply( files, function(x){
    
    # Increment the progress bar, and update the detail text.
    incProgress(1/(nfiles+1), detail = x$data$name )
    
    # Per file: Add to files.original and to summary.data (check for errors)
    ret<-storage_rv_add_file(x,rv,package)
    
    if( ! is.null(ret) ){
      createAlert(session,"dl_files_alert",style="error", content = ret )
      color <- "yellow"
      nerror <- nerror + 1
    }
    loaded <- loaded + 1
    
    # Update task
    #h_task_menu.add( rv, "upload", text = "Uploading files", color = color, value = ceiling(100*loaded/nfiles) )
    
    # watcher.h_task_menu(rv, output) # Force update
  })
    
  incProgress(1/(nfiles+1), detail = "Cleaning up")
  }) # With progress
  

  
  # Check for errors
  if(color == "blue"){
    #h_task_menu.add( rv, "upload", text = "Upload completed successfully", color = "green", value = 100 )
    createAlert(session,"dl_files_alert",style="success", content = sprintf("Upload finished successfully. %d new somas added. ", length(files)) )
  }
  else
  {
    #h_task_menu.add( rv, "upload", text = "Upload completed with errors", color = "red", value = 100 )
    createAlert(session,"dl_files_alert",style="warning", content = sprintf("Upload finished with errors.  %d conflincting files. ", nerror) )
  }
  
  # watcher.h_task_menu(rv, output) # Force update
  
  # Update log (What to be shown?)
  
  return ( NULL )
}
#'
#' Selector
#'
update_dl_download_selector <- function( rv , output, session ){
  
  write( "[CALL] UPDATE DL SELECTOR" , file=stderr())
  
  # Create hcbox
  if( nrow(rv$soma$summary.data) > 0 ){
    
    structure <- build_hcbox_selector( rv$soma$summary.data, NULL,
                                       includePkgName = rv$config$selectors.packageName,
                                       removeEmpty = rv$config$selectors.removeEmpty,
                                       withSimulations = T,
                                       byPackage = F)
    
    output$dl_download_selector_ui <- renderUI({
      hCBoxInput("dl_download_selector", 
                               structure,
                               label = NULL,
                               skipTopLevels =  T )
      
    })
    closeAlert(session,"load_download_nochar")
    
  }
  else{
    output$dl_pproc_selector_ui <- renderUI({ tags$p("No data has been loaded yet") })
    createAlert(session,"dl_download_alert","load_download_nochar", title = "No data available",
                content = "No soma have been loaded yet. Go to Data loading -> load data to load somas",
                style = "warning", dismiss = F, append = F )
  }
}

#'  
#' Control export button
dl_download_create <- function ( rv, input, output, session ){
  
  # Output may not exist yet
  tryCatch(
    # Export rv$soma$characterization table
    output$dl_download_save <- downloadHandler(
      filename = "somas.zip",
      contentType = "application/zip",
      content = function(fname){ 
        
        somas <- extract_somalist_form_selector( input$dl_download_selector)
        
        # Remove "root" somas
        toRemove <- NULL
        for (i in 1:length(somas) ) if ( somas[[i]]$type == "root" ) toRemove <- c(toRemove,-i)
        if (!is.null(toRemove))
          somas <- somas[toRemove]
        
        # Create tempdir
        baseDir <- file.path(tempdir(),"somaMS_files")
        if (dir.exists(baseDir)) unlink(baseDir, recursive = T )
        dir.create( baseDir )
        
        # Create metadata
        meta <- somaMSMetaheader()
        pkgsMeta <- list()
        simsMeta <- list()
        
        
        # For each selected soma
        sapply( somas, function(soma,rv){
          
          createAlert(session, "dl_download_alert", alertId = NULL, dismiss = F, append = F,
                      style = "info", title = "Please wait...", content = sprintf("Processing %s (%s)",soma$name,soma$type))
          
          # Get data
          if (soma$type == "sim"){
            soma.real <- rv$files$simulated[[soma$name]]
            package <- file.path("simulations",soma.real$simulation_id)
            simid <- soma.real$simulation_id
            sim <- isolate(rv$soma$simulation[[soma.real$simulation_id]])
            
            ## Add sim
            if ( is.null(simsMeta[[simid]]) ) {
              simsMeta[[simid]] <<- createPackageNode(meta, simid, sim = T)
              createSimulationNode(parent = simsMeta[[simid]], name = package, model = sim$model, nsims = sim$nsomas, ncluster = sim$cluster )
            }
            
            createSimsomaNode(simsMeta[[simid]], name = soma$name, simulationId = simid, file = paste0(soma$name,".ply") )
          }
          else
          {
            soma.real <- rv$files$real[[soma$name]]
            package <- soma.real$package
            
            ## Add soma
            if ( is.null( pkgsMeta[[package]] ) ) pkgsMeta[[package]] <<- createPackageNode(meta, package, sim = F)
            
            createSomaNode(pkgsMeta[[package]], name = soma$name,repaired =  ( (soma$type == "repaired") || (soma$type == "rep_and_seg") ),
                           segmented = ( (soma$type == "segmented") || (soma$type == "rep_and_seg") ),
                           file = paste(soma$name, soma.real$ext , sep = ".") )
          }
          
          # Create package dir
          packageBaseDir <- file.path(baseDir,package )
          if(!dir.exists( packageBaseDir ) ) dir.create(packageBaseDir , showWarnings = F, recursive = T)
          
          # Create soma dir if it doesnt exits
          if(soma$type == "sim"){
            typeBaseDir <- packageBaseDir # Simulations doesnt have status
          }
          else{
            typeBaseDir <- file.path(packageBaseDir,soma$type )
            if(!dir.exists( typeBaseDir ) ) dir.create(typeBaseDir , showWarnings = F)
          }
          
          # Get datafile
          data <- switch(soma$type,
                         raw = soma.real$original$data,
                         repaired = soma.real$repaired$data,
                         segmented = soma.real$segmented$data,
                         rep_and_seg = soma.real$rep_and_seg$data,
                         sim = soma.real$data )
          
          # Case A: data is a string (filepath) : just move it
          if( is.character(data)){
            file.copy(data,file.path(typeBaseDir,paste0(soma$name,".ply")))
          }
          # Case B: Write PLY
          else{
            vcgPlyWrite(tmesh3d( t(cbind(data$vertices,1)), t(data$faces) ), filename = file.path(typeBaseDir, soma$name ), binary =T )
          }
          
          # TODO: ADD METADATA
        },rv=rv)
        
        # Write metadata to tempdir
        writeSomaMSMetadata( meta, file.path(tempdir(), "somaMS_files", "metadata.xml") )
        
        # Create zip file
        #zip(zipfile=fname, files=list.files(path = baseDir, pattern = "*.ply", recursive = T , full.names=TRUE, ignore.case = T) )
        # Move before 
        prev_wd <- getwd()
        setwd(tempdir())
        zip(zipfile=fname, files="somaMS_files" )
        setwd(prev_wd)
        
        output$dl_download_log <- renderText( paste( "Compressing files...",
                                                     paste0(sapply(somas,function(x)sprintf("added: %s (%s)",x$name,x$type)),collapse = "\n"), 
                                                     "Process completed",
                                                     sep = "\n") )
        
        createAlert(session, "dl_download_alert", alertId = "dl_download_final", dismiss = T, append = F,
                    title = "Process Completed", style="success", content = sprintf("%d somas processed successfully",length(somas)))
      }
    ))
  
}
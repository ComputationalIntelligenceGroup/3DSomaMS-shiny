#' Update new model selector
#'
model_new_selector_update <- function (rv , output, session ){
  
  write( "[CALL] UPDATE NEW MODEL SELECTOR" , file=stderr())
  
  # Create hcbox
  if( nrow(rv$soma$characterization) > 0 ){
    
    structure <- build_hcbox_selector_characterizations(rv$soma$characterization ,removeEmpty = rv$config$selectors.removeEmpty,
                                                        withSimulations  = T, includePkgName = rv$config$selectors.packageName )
    
    output$mod_new_selector_ui<- renderUI({
      hCBoxInput("mod_new_selector", 
                               structure,
                               label = NULL,
                               skipTopLevels =  T )
    })
    closeAlert(session, "mod_new_nochar")
  }
  else{
    
    createAlert(session,"mod_new_alert","mod_new_nochar", title = "No data available",
                content = "No somas have been characterized yet. Go to Data loading -> preprocess to characterize somas or import a CSV table in Data loading -> data tables",
                style = "warning", dismiss = F, append = F )
    
    output$mod_new_selector_ui<- renderUI({ tags$p(" Characterize somas first") })
  }
}


model_new_button_enable <- function(input, session){
  
  if( length(input$mod_new_selector)>0 && length(input$mod_new_name)>0 ){
    updateButton(session,"mod_new_run", disabled = F)
  }
  else
    updateButton(session,"mod_new_run", disabled = F)
}


model_new_createModel <- function(rv, input, output, session){
  
  log <- "[INFO] START: Build model"
  errors <- F
  
  # Char table
  chars <- rv$soma$characterization
  
  # Get all params
  name <- input$mod_new_name
  nboots <- input$mod_new_nboots
  sigthreshold <- input$mod_new_sigthreshold
  nclust <- input$mod_new_nclust
  initmethod <- input$mod_new_initmethod

  withProgress(message = 'Building model:', value = 0, {
    
  incProgress( amount = 0.05, detail = "Prerpocessing data")
  createAlert(session, "mod_new_alert", "mod_new_info0", title = "Please wait", content = "Preprocessing data...",
                style = "info", append = F)
    
  # Check that we do not have a model with the same name already created
  if(!is.null(rv$soma$model[[name]])){
    log <- c(log, sprintf("[CRITICAL] Model name already used." ))
    createAlert(session, "mod_new_alert", "bn_fail", title = "Error", content = "Duplicated model name",
                style = "error", append = F)
    output$mod_new_log <- renderText( paste0(log,collapse = "\n" ) )
    return(NULL)
  }
  
  # Extract soma name and type (no need to isolate since this is intended to be inside of a observe event)
  selectedData <- extract_somalist_form_selector( input$mod_new_selector )
  
  # Remove "root" data
  toRemove <- NULL
  for(i in 1:length(selectedData) ) if( selectedData[[i]]$type == "root" ) toRemove <- c(toRemove,-i)
  if(!is.null(toRemove))
    selectedData <- selectedData[toRemove]
  
  # Check for dups
  names <- sapply(selectedData,function(x)x$name)
  types <- sapply(selectedData,function(x)x$type)
  inData <- NULL
  
  
  if( length(unique(names)) < length(names) ){
    errors <- T
    log <- c(log,sprintf("[WARN] Found two or more characterizations from the same soma %s", names[i] ) )
  } 
  
  # Extract selected data
  # Build parameter list to save the model
  parameters <- list( data = merge(chars, data.frame(name=names, from = types, stringsAsFactors = F)),
                      nboots = nboots,
                      sigthreshold = sigthreshold,
                      nclust = nclust,
                      initmethod = initmethod )
  
  log <- c(log, sprintf("[INFO] Number of instances: %d.",nrow(parameters$data) ))
  
  # Call bn builder
  incProgress( amount = 0.05, detail = "Learning Bayesian network")
  createAlert(session, "mod_new_alert", "mod_new_info1", title = "Please wait", content = "Learning Bayesian Network",
              style = "info", append = F)
  
  # remove non data fields (name, package, deleted, from)
  bn <- NULL
  tryCatch(bn <- BN_structure_learning(parameters$data[,-(1:5)], nboots, sigthreshold ),
           error = function(e) { log <<- c(log, sprintf("[ERROR] While learning BN: %s." , e)) ; errors <<- T })
  
  # Abort process
  if(is.null(bn)){
    log <- c(log, sprintf("[CRITICAL] Cannot continue without a BN." ))
    createAlert(session, "mod_new_alert", "bn_fail", title = "Error", content = "Fatal error while learning the Bayesian network model",
                style = "error", append = F)
    output$mod_new_log <- renderText( paste0(log,collapse = "\n" ) )
    return(NULL)
  }
  log <- c(log, sprintf("[INFO] Bayesian Network learnt. Number of arcs: %d .", nrow(arcs(bn)) ))
  
  incProgress( amount = 0.5, detail = "Clustering")
  createAlert(session, "mod_new_alert", "mod_new_info2", title = "Please wait", content = "Calculating clustering...",
              style = "info", append = F)
  
  # Continue with clustering
  cluster <- NULL
  tryCatch( cluster <- BN_clustering( bn, parameters$data[,-(1:5)], seq(nclust[1],nclust[2]), switch(initmethod,"Kmeans" = 0, "Random" = 1), F)
            ,error = function(e) { log <<- c(log, sprintf("[ERROR] While building clusters: %s." , e)) ; errors <<- T })
   
  log <- c(log, sprintf("[INFO] Clustering finished. Optimal number of clusters: %d .", length(cluster$priori) ) )
  
  # Abort process
  if(is.null(cluster)){
    log <- c(log, sprintf("[CRITICAL] Cannot continue without clustering." ))
    createAlert(session, "mod_new_alert", "clust_fail", title = "Error", content = "Fatal error while building clusters",
                style = "error", append = F)
    output$mod_new_log <- renderText( paste0(log,collapse = "\n" ) )
    return(NULL)
  }
  
  incProgress( amount = 0.4, detail = "Cleaning up")
  createAlert(session, "mod_new_alert", "mod_new_info3", title = "Please wait", content = "Finishing...",
              style = "info", append = F)
  
  # Add model
  storage_rv_add_model(name, bn, cluster, parameters, rv)
  
  # Disable button
  updateButton(session,"mod_new_run",disabled = T)
  
  # Out
  log <- c(log, sprintf("[INFO] Process finished successfully." ))
  createAlert(session, "mod_new_alert", "mod_new_ok", title = "Process completed", content = "Process completed successfully",
              style = "success", append = F)
  output$mod_new_log <- renderText( paste0(log,collapse = "\n" ) )
  
  })
}
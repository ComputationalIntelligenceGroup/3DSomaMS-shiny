#'
#' Update model selector
#'
update_sim_new_model_selector <- function(rv, output,session){
  names <- names(rv$soma$model)
  if(length(names)>0){
    closeAlert(session,"sim_nomodel")
    output$sim_new_model_ui <- renderUI( selectInput("sim_new_model", "Model selector", choices = names) )
  }
  else
  {
    createAlert(session,"sim_new_alert","sim_nomodel", title = "No model available",
                content = "No model has been created yet. Go to modelling -> new model to create a model.",
                style = "warning", dismiss = F, append = F )
  }
}

#'
#' Update cluster selector
#'
update_sim_new_cluster_selector <- function(rv, input, output,session){
  modelName <- input$sim_new_model
  if(length(modelName)>0){
    output$sim_new_cluster_ui <- renderUI( selectInput("sim_new_cluster", "Cluster selector", choices = c("All",seq(1,length(rv$soma$model[[modelName]]$cluster$priori)))) )
  }
  else
  {
    return ( NULL )
  }
}

# Unlock simulation button
update_sim_new_button <- function(rv, input, session){
  updateButton(session, "sim_new_run", disabled =  (!( ( length(input$sim_new_model) > 0 ) && ( length(input$sim_new_name) > 0 )  ) ) )
}

#'
#' Run simulation process
#'
run_sim_new_process <- function(rv, input, output, session ){
  
  log <- "[INFO] Starting simulation process"
  errors <- F
  
  # Get params
  modelName <- input$sim_new_model
  model <- rv$soma$model[[modelName]]
  name <- input$sim_new_name
  cluster <- input$sim_new_cluster
  nsomas <- input$sim_new_nsomas
  
  # Output log
  log <- c(log, sprintf("[INFO] name: %s, model: %s, somas: %d, cluster: %s", name, modelName, nsomas, cluster) )
  
  # Create info alert
  createAlert(session, "sim_new_alert", NULL, title = "Running simulation", content = "Starting preprocessing", dismiss = F, append = F, style = "info" )
  
  # With progress environment
  withProgress(message="Running simulation", value = 0, {
    
    # Sampling
    incProgress( amount = 0.05, detail = "Sampling new characteristics")
    
    # Cluster to numeric
    if(cluster=="All")
      cluster_num <- NULL
    else
      cluster_num <- as.numeric(cluster)
    
    createAlert(session, "sim_new_alert", NULL, title = "Running simulation", content = "Sampling new somas", dismiss = F, append = F, style = "info" )
    log <- c(log, sprintf("[INFO] Sampling new somas") )
    sim.chartable <- NULL
    tryCatch(sim.chartable <- simulate_new_somas( model$cluster, model$params$data, cluster_number = cluster_num, number_of_new_instances = nsomas ),
             error = function(e) { log <<- c(log, sprintf("[ERROR] While sampling CPTS: %s." , e)) ; errors <<- T } )
    
    # Abort process
    if(is.null(sim.chartable)){
      log <- c(log, sprintf("[CRITICAL] Critical error while sampling. Please check output log for more info" ))
      createAlert(session, "sim_new_alert", "sampling_fail", title = "Error", content = "Fatal error while sampling. Please check output log for more info",
                  style = "error", append = F)
      output$sim_new_log <- renderText( paste0(log,collapse = "\n" ) )
      return(NULL)
    }
    
    #  Create ellipses
    createAlert(session, "sim_new_alert", NULL, title = "Running simulation", content = "Reconstructing meshes", dismiss = F, append = F, style = "info" )
    incProgress( amount = 0.65, detail = "Reconstructing meshes")
    log <- c(log, sprintf("[INFO] Reconstructing meshes") )
    
    # Done
    meshes <- lapply(1:nsomas, function(x)
                     simulation_3D_mesh(sim.chartable,x)
                     )
    
    incProgress( amount = 0.20, detail = "Updating internal tables")
    
    # Add new simulation
    storage_rv_add_simulation(rv, name, modelName , nsomas, cluster)
    
    for ( i in 1:nrow(sim.chartable) ) {
       # Add new simulation (soma)
      storage_rv_add_simulatedSoma(rv, paste(name,i,sep="_sim_") , name, meshes[[i]] )
    }
    
    # Add characterizations FIXME
    storage_rv_add_simulation_chartable(rv, paste(name,1:nsomas,sep="_sim_"), name, sim.chartable )
    
    incProgress( amount = 0.10, detail = "Done")
  })
  
  log <- c(log, sprintf("[INFO] Process finished") )
  createAlert(session, "sim_new_alert", "sim_ok", title = "Simulation completed", content = sprintf("%d somas simulated successfully",nsomas),
              style = "success", append = F)
  output$sim_new_log <- renderText( paste0(log,collapse = "\n" ) )
}
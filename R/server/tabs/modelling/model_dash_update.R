#' Update model dashboard
#' 
model_dash_update <- function(rv, output){
  
  models <- rv$soma$model
  # First: update boxes
  
  # Number of models
  nmodels <- length(names(models))
  create_vbox(output,"mod_vbox_models",nmodels, "Models", color = "blue", icon("stats",lib="glyphicon"))
  
  # Average number of arcs
  if(nmodels == 0 ) narcs <- 0
  else narcs<- mean(sapply( models, function(x) nrow(arcs(x$bn)) ) )
  
  create_vbox(output,"mod_vbox_arcs",narcs, "Avg. number of arcs", color = "green", icon("exchange"))
  
  # Average number of clusters
  if(nmodels == 0 ) nclust <- 0
  else nclust<- mean(sapply( models, function(x) length(x$cluster$prior)) )

  create_vbox(output,"mod_vbox_clusters",nclust, "Avg. number of clusters", color = "orange", icon("boxes"))
  
  # Second: update table
  
  summary.df <- data.frame(
    Model = character(0),
    Instances = numeric(0),
    From = character(0),
    Packages = character(0),
    Arcs = numeric(0),
    Clusters = numeric(0),
    stringsAsFactors = F
  )
  
  # Create a row pero model
  modelname <- names(models)
  
  sapply(modelname, function(name){
    
    data <- models[[name]]$params$data
    
    from <- paste0( unique( data$from ), collapse = "," )
    packages <- paste0( unique( data$package ), collapse = "," )
    instances <- nrow(data)
    arcs <- nrow(arcs(models[[name]]$bn))
    clusters <- length(models[[name]]$cluster$prior)
    
    summary.df[nrow(summary.df)+1,] <<- c(name, instances, from, packages, arcs, clusters)
  })
  
  # Apply from editor
  summary.df <- df_from_to_icon_editor(summary.df)
  
  output$mod_summary <- renderDataTable(summary.df, 
                                         escape = -3,
                                         options = list( pageLength = 10, lengthChange = FALSE,
                                                         info = FALSE, searching = FALSE, searchable = NULL))
}
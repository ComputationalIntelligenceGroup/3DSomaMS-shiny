#'
#' Update model selector
#'
update_model_view_selector <- function(rv, output,session){
  names <- names(rv$soma$model)
  if(length(names)>0){
    closeAlert(session,"nomodel")
    output$mod_view_sel_ui <- renderUI( selectInput("mod_view_sel", "Model selector", choices = names) )
  }
  else
  {
    createAlert(session,"mod_view_alert","nomodel", title = "No model available",
                content = "No model has been created yet. Go to modelling -> new model to create a model.",
                style = "warning", dismiss = F, append = F )
  }
}

#'
#' Update General model "tab"
#'
update_model_view_general <- function(rv, input, output, session ){
  
  if( !is.null(input$mod_view_sel) ){
    
    # Get selected model
    selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
    
    # Update value boxes
    create_vbox(output,"mod_view_vbox_instances",
                        nrow(selected_model$params$data), "Instances", icon = icon("list"), 
                        color = "blue", width = 3)
  
    create_vbox(output,"mod_view_vbox_variables",
                ncol(selected_model$params$data)-5, "Variables", icon = icon("th-large"), 
                color = "green", width = 3)
    
    create_vbox(output,"mod_view_vbox_bnarcs",
                nrow(arcs(selected_model$bn)), "Arcs in the BN", icon = icon("exchange"), 
                color = "orange", width = 3)
    
    create_vbox(output,"mod_view_vbox_clusters",
                length(selected_model$cluster$priori), "Clusters", icon = icon("cubes"), 
                color = "purple", width = 3)
  }
  
  # Update model datatable
  df <- selected_model$params$data[,1:4]
  df <- df_from_to_icon_editor(df,params = list(colname = "from"))
  colnames( df ) <- c("Name", "From", "Package", "Simulation ID")
  output$mod_view_modeldata<- renderDataTable(df, escape=-2, options = list( pageLength = 10, lengthChange = FALSE,
                                                            info = FALSE, searching = FALSE, searchable = NULL))
  
}

##
# Model BN update
##
update_model_view_bnmodel <- function( rv, session, input, output){
  
  if( !is.null(input$mod_view_sel) ){
    
    # Get selected model
    selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
    
    # Get selected variables
    selected_vars <-input$mod_view_bnvarsel
    
    # Update cluster selector
    updateSelectInput(session,"mod_view_bnmodal_cluster",choices = 1:length(selected_model$cluster$prior))
    
    if( length(selected_vars)==0){
      d3bn <- d3BnInstance$new(model2network("[SELECT][A|SELECT][VARIABLE|A]"), d3BnList = asD3BnList.bnlearn )
      renderBnD3(session, "mod_view_bnio" , d3bn,
                 #layout = "hierachical",
                 layout = "force",
                 menu.options = list(),
                 node.subgraph = NA,
                 node.showTooltip = T,
                 node.radius = 25,
                 node.padding = 25,
                 force.linkdist = 150,
                 force.charge = -800,
                 arc.width = 1)
    }
    else{
      # Get bn 
      bn <- selected_model$bn
      
      # INDEV lib
      d3bn <- d3BnInstance$new(bn, d3BnList = asD3BnList.bnlearn )
      
      renderBnD3(session, "mod_view_bnio" , d3bn,
                 #layout = "hierachical",
                 layout = "force",
                 menu.options = list( list( title = "View marginals", actionType = "cpt") ),
                 node.subgraph = selected_vars,
                 node.showTooltip = T,
                 node.radius = 25,
                 node.padding = 25,
                 force.linkdist = 150,
                 force.charge = -800,
                 arc.width = 1)
    }
  }
}


#'
#'
mod_view_updatesourcedt <- function (rv, input, output ){
  
  if( !is.null(input$mod_view_sel) ){
    
    # Get selected model
    selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
    selected <- input$mod_view_sourcevarsel
    
    if( (nrow(selected_model$params$data) > 0) && (length(selected)>1) ){
      
      # Filter and select
      df <- df_applyfilters(selected_model$params$data, rv$soma$characterization.filter)[,selected]
      toEscape <- which(colnames(df) != "from")
      colnames(df) <- prettyColnames(colnames(df))
      output$mod_view_sourcedt <- renderDataTable(df, 
                                                  escape = toEscape,
                                                  options = list( pageLength = 10, lengthChange = FALSE,
                                                                info = FALSE, searching = FALSE, searchable = NULL))
      # Set upload button
      output$mod_view_savesource_ui <- renderUI(downloadButton("mod_view_savesource", label="Export as CSV"))
    }
    else
      return (NULL)
  }
}

#'  
#' Control export button
mod_view_sourcedown <- function ( rv, input, output ){
  
  if( !is.null(input$mod_view_sel) ){
    
    # Get selected model
    selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
    
    # Output may not exist yet
    tryCatch(
      # Export rv$soma$characterization table
      output$mod_view_savesource <- downloadHandler(
        
        filename = "source_data.csv",
        content = function(file){ write.csv(selected_model$params$data, file=file, row.names = F)}
      ))
  }
}

#'
#' Modal control
mod_view_bnmodal_control <- function(rv, input, output, session){
  value <- input$mod_view_bnio
  if(!is.null(value))
  {
    if(value$actionType == "cpt"){
      
      selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
      nclust <- as.numeric(isolate(input$mod_view_bnmodal_cluster))
      params <- selected_model$cluster$parameters[[nclust]]
      thisnode <-  params[[value$name]]
      
      # Fill modal stuff

      # nodename 
      output$mod_view_bnmodal_nodename <- renderText( value$name )
      
      # Update nodeval from rv$nodevals
      updateNumericInput(session, "mod_view_bnmodal_nodeval" , value =  unname(round(isolate(rv$bnmodal_vals[value$name]), digits=3) ) )
      
      # Update children
      output$mod_view_modal_children_ui <- renderUI(
        tagList(
          tags$h3("Descendants"),
          tags$ul(
            tagList(
              #lapply( thisnode$children, function(x, v ){ tags$li( tags$b("Node: "),x,tags$b("   Value: "), unname(v[x])) },  isolate(rv$bnmodal_vals)  ))
              lapply( thisnode$children, function(x, v ){ tags$li( tags$b("Node: "), x) } ))
          )
        )
      )
      
      # Update parents
      output$mod_view_modal_parents_ui <- renderUI(
          tagList(
            tags$h3("Parent nodes"),
            lapply( thisnode$parents, 
                    function(x, v ){ numericInput(sprintf("mod_view_bnmodal_parent_%s", x), label = x, value = unname(  v[x] ) ) }, isolate(rv$bnmodal_vals) ))
      )
      
      # Update equation
      output$mod_view_bnmodal_node_eq <- renderText(paste0(round(thisnode$coefficients,digits=3) ,collapse = ", ") )
      
      # Update sigma
      output$mod_view_bnmodal_node_sigma <- renderText(round(thisnode$sd,digits=3) )
      
      # Update freenode selector
      updateSelectInput(session, "mod_view_bnmodal_freevar", choices = c("None",thisnode$parents) )
      
      # Show modals
      toggleModal(session, "mod_view_bncond", toggle = "open")
    }
      
    
    
  }
}

##
# Cluster view
##
update_model_view_cluster <- function( rv, session, input, output){
  
  if( !is.null(input$mod_view_sel) ){
    
    # Get selected model
    selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
    
    nclust <- length(selected_model$cluster$priori) 
    if(nclust == 1){
      return ;
    }
    
    ## Update cluster table
    
    ctable <- cbind.data.frame(selected_model$params$data[,1], lapply(as.data.frame(selected_model$cluster$weights), round, digits=2))
    colnames(ctable) <- c("Name", paste("Prob. cluster",1:nclust) )
    output$mod_view_clusterprobs <- renderDataTable(ctable, 
                                                options = list( pageLength = 10, lengthChange = FALSE,
                                                                info = FALSE, searching = FALSE, searchable = NULL))
    
    # Prealloc JSD matrix
    JSD <- matrix(0, ncol = nclust, nrow = nclust)
    
    # remove0's 
    weights <- selected_model$cluster$weights
    
    
    
    weights[ weights == 0 ] <- .Machine$double.eps
    
    ## Fill matrix (Compute Jensen shanon divergence. IF for any position M is 0 then leave that cell at MAX DBL)
    for( i in 1:(nclust-1) ){
      for( j in (i+1):nclust){
          M <- (weights[,i] + weights[,j])/2
          JSD[i,j] <- sum( ( weights[,i] * log( weights[,j] )  + 
                               weights[,j] * log( weights[,i] ) ) / (2*M) )
          # JSD is symmetryic
          JSD[j,i] <- JSD[i,j]
      }
    }
    
    # Compute 3D eigv to locate points in space
    
    # CMD cannot be applied if nclust is <=3
    if(nclust > 3){
      cluster.coordinates <- cmdscale(JSD, k = 3, eig = T )$points
    }
    else {
#       x <- matrix(0, nclust, nclust)
#       x[row(x) > col(x)] <- JSD^2
#       x <- x + t(x)
#       e <- eigen(-x/2, symmetric = TRUE)
#       cluster.coordinates <- t(e$vectors * rep(sqrt(abs(e$values)), each = nclust))
      cluster.coordinates <- JSD
      
      
      if(nclust == 2)
        cluster.coordinates <- cbind(cluster.coordinates,0)  
    }
    
    # Assign a color to each cluster
    cluster.colors <- t( col2rgb(rainbow(nclust)) )
    
    # Cluster labels (1:n)
    cluster.labels <- paste("Cluster",1:nclust, "centroid")
    
    # Compute the color of each instance ( proportional mean by pertnenence probability )
    point.colors <- weights %*% cluster.colors
    
    # Transform colors to RGB
    point.colors <- apply(point.colors,1, function(x){x <- as.integer(x); sprintf("#%02X%02X%02X",x[1],x[2],x[3])})
    cluster.colors <- apply(cluster.colors,1, function(x){x <- as.integer(x); sprintf("#%02X%02X%02X",x[1],x[2],x[3])})
    
    # Use the same principle to compute their coordinates
    point.coordinates <-  weights %*% cluster.coordinates
    
    # Get point labels
    point.labels <- selected_model$params$data[,1]
    
    # Render scatterplot
    output$mod_view_clusterscatter <- renderScatterplotThree({
      scatterplot3js( rbind( cluster.coordinates , point.coordinates), color = c(cluster.colors, point.colors ), labels = c(cluster.labels, point.labels), renderer = "canvas", grid = T )
    })
  }
}


# Initialize cluster vals
listener_bnmodal_clustersel <- function(rv, input, output, session ){
  
  if( length(input$mod_view_bnmodal_cluster)>0 && input$mod_view_bnmodal_cluster!="" ){
    
    selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
    nclust <- as.numeric(input$mod_view_bnmodal_cluster)
    params <- selected_model$cluster$parameters[[nclust]]
    
    
    # Create array
    nodes <- names(params)
    nodevals <- rep( 0,length(nodes) )
    names(nodevals) <- nodes
    
    # Fill it
    order <- bnlearn::node.ordering(params)
    for( n in order ){
      
      # Get node coefs
      node_coefs <- params[[n]]$coefficients
      
      # Get parent values
      if( length(params[[n]]$parents)>0 )
        p_vals <- sapply(params[[n]]$parents, function(x,nodevals){ nodevals[x] },nodevals)
      else
        p_vals <- NULL
      
      # Add 1 at the begining and multiply by node coefs - That our node's mu val
      nodevals[n] <- sum( node_coefs * c(1,p_vals) ) 
    }
    # Add to RV
    rv$bnmodal_vals <- nodevals
  }
}

modview_update_nodevals <- function(rv, input, output, session){
  
  value <- input$mod_view_bnio
  name <- value$name
  
  if( length(name) > 0){
    
    # Get thisnode data
    selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
    nclust <- as.numeric(isolate(input$mod_view_bnmodal_cluster))
    params <- selected_model$cluster$parameters[[nclust]]
    thisnode <-  params[[name]]
    parents <- thisnode$parents
    
    # For each parent, get its value and update it in rv (when the node changes, the dependencies also change)
    for(p in parents){
      value <- input[[ paste0("mod_view_bnmodal_parent_",p) ]]
      if( !is.null(value) && !is.na(value))
        rv$bnmodal_vals[ p ] <- value
    }
    
    # Update my own value
    value <- input$mod_view_bnmodal_nodeval
    if(!is.null(value) && !is.na(value) )
      rv$bnmodal_vals[ name ] <- value
  }
}

modview_update_bnmodal_mu <- function(rv, input, output, session){
  
  value <- input$mod_view_bnio
  name <- value$name
  
  if( length(name) > 0){
    
  
    # Get thisnode data
    selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
    nclust <- as.numeric(isolate(input$mod_view_bnmodal_cluster))
    params <- selected_model$cluster$parameters[[nclust]]
    thisnode <-  params[[name]]
    
    # Update mu
    # Get parent values
    if( length(thisnode$parents)>0 )
      p_vals <- sapply(thisnode$parents, function(x,nodevals){ nodevals[x] },rv$bnmodal_vals)
    else
      p_vals <- NULL
    
    # Add 1 at the begining and multiply by node coefs - That our node's mu val
    muval <- unname( sum( thisnode$coefficients * c(1,p_vals) ) )
    
    # Update mu val
    # nodename 
    output$mod_view_bnmodal_node_mu <- renderText( round(muval, digits= 3) )
    
    # Update f(x)
    sigma <- thisnode$sd
    myval <-  unname(rv$bnmodal_vals[name])
    output$mod_view_bnmodal_node_fval <- renderText( round( dnorm( myval , mean = muval, sd = sigma ), digits=3 ) )
    
    # Plot
    if( !is.na(myval) ){
        output$mod_view_bnmodal_plot_2d <- renderPlot({ ggplot(data.frame(x = c(muval-3*sigma, muval+3*sigma)), aes(x)) + 
            stat_function(fun = dnorm, colour = "red", size=1, args=list(muval,sigma)) +
            ylab("density") +
            geom_vline(aes_q(xintercept=myval),   # Ignore NA values for mean
                       color="black", linetype="dashed", size=1) 
        })
    }
  }
}

#'  
#' Control export button
mod_view_modeldown <- function(rv, input, output ){
  
  if ( !is.null(input$mod_view_sel) ) {
    
    # Get selected model
    selected_model <- isolate(rv$soma$model[[input$mod_view_sel]])
    
    # Output may not exist yet
    tryCatch(
      # Export rv$soma$characterization table
      output$mod_view_down <- downloadHandler(
        
        filename = sprintf("%s.zip",input$mod_view_sel),
        contentType = "application/zip",
        content = function(fname){ 
          
          workdir <- file.path(tempdir(),"SomaMS_files")
          if (dir.exists(workdir)) unlink(workdir, recursive = T )
          dir.create( workdir )
          
          meta <- somaMSMetaheader()
          
          # Write model data to workingdir
          write.csv(selected_model$params$data, file = file.path(workdir, "data.csv"), row.names = F)
          
          # Write model to metadata
          createModelNode(parent = meta, name = input$mod_view_sel, data = selected_model$params$dat,
                          file = "data.csv", model = selected_model, nboots = selected_model$params$nboots,
                          sigthreshold = selected_model$params$sigthreshold, nclust = selected_model$params$nclust,
                          initmethod = selected_model$params$initmethod )
          
          writeSomaMSMetadata(root = meta, outputFile = file.path(workdir, sprintf("%s.xml",input$mod_view_sel) ))
          
          prev_wd <- getwd()
          setwd(tempdir())
          zip(zipfile = fname, files = "SomaMS_files" )
          setwd(prev_wd)
        }
      ))
  }
}
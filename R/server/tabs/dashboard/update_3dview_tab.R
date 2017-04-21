#' Functions to update controls and render of 3D visualization tab
#'
#'

#' Update soma selector (only by package)
update_view_somaselector <- function(rv , output,session ){
  
  if( nrow(rv$soma$summary.data) > 0 ){
  
    structure <- build_hcbox_selector(rv$soma$summary.data, NULL,
                         byStatus=F,
                         packageWithStatus = F,
                         includePkgName = rv$config$selectors.packageName,
                         removeEmpty = rv$config$selectors.removeEmpty)
    
    output$dash_view_selector_ui <- renderUI({
      hCBoxInput("dash_view_selector", 
                               structure,
                               label = NULL,
                               skipTopLevels =  T,
                               multiple = F,
                               checkbox = F)
    })
    
    closeAlert(session, "dash_view_nochar")
  }
  else{
    output$dash_view_selector_ui <- renderUI({ tags$p(" No data has been loaded yet ") })
    createAlert(session,"dash_view_alert","dash_view_nochar", title = "No data available",
                content = "No soma have been loaded yet. Go to Data loading -> load data to load somas",
                style = "warning", dismiss = F, append = F )
  }
  
}

update_view_mesh_selector <- function(rv, output, input ){
  
  # Get selected soma
  soma.selected <- input$dash_view_selector
  if(is.null(soma.selected)){
    output$dash_view_mesh_ui <- renderUI({ p(" No suitable soma selected ")})
    return(NULL)
  }
  
  # Remove everything after the last :
  soma.selected <- sub(":[^:]*$","",soma.selected,perl=T)
  
  # Build "choices"
  choices <- NULL
  
  #Original
  if( !is.null( rv$files$real[[soma.selected]] ) ){
      choices <- "Original"
      
      # Repaired
      if( !is.null( rv$files$real[[soma.selected]]$repaired )) choices <- c( choices, "Repaired" )
      
      # Segmented
      if( !is.null( rv$files$real[[soma.selected]]$segmented)) choices <- c( choices, "Segmented" )
      
      # rep_and_seg
      if( !is.null( rv$files$real[[soma.selected]]$rep_and_seg)) choices <- c( choices, "Rep. & Seg."  )
  }
  else if( !is.null ( rv$files$simulated[[soma.selected]] ) ){
    choices <- "Simulated"
  }
  
  output$dash_view_mesh_ui  <- renderUI({
    checkboxGroupInput("dash_view_mesh", NULL, choices)
  })
}

update_cao_selector <- function(rv, output, input ){
  
  # Get selected meshes
  mesh.selected <- input$dash_view_mesh
  if(is.null(mesh.selected) ){
    output$dash_view_cao_selector_ui <- renderUI({ p(" No mesh selected ")})
    return(NULL)
  }
  else{
    output$dash_view_cao_selector_ui <- renderUI({
      selectInput("dash_view_cao_selector",NULL,  mesh.selected,multiple = F)
    })
  }
}

update_cao_lower_build <- function(rv, output, input){
  
  cao.selected <- input$dash_view_cao_selector
  if(is.null(cao.selected)){
    return(NULL)
  }
  else{
    
    # Get color config
    cao.config <- switch (cao.selected,
                    Original = isolate(rv$dash_view$original),
                    Simulated = isolate(rv$dash_view$original),
                    Repaired = isolate(rv$dash_view$rep),
                    Segmented = isolate(rv$dash_view$seg),
                    "Rep. & Seg." = isolate(rv$dash_view$repAndSeg))
    
    # Build inputs
    output$dash_view_cao_lower_ui <- renderUI({
      tagList(
        colourInput("dash_view_cao_color", "Mesh color", value = cao.config$color ),
        numericInput("dash_view_cao_alpha", "Opacity", value = cao.config$alpha, min = 0, max = 1, step=0.01)
      )
    })
  }
}

update_cao_lower_setrv <- function( rv, input ){
  
  cao.selected <- input$dash_view_cao_selector
  color <- input$dash_view_cao_color
  alpha <- input$dash_view_cao_alpha
  if(is.null(cao.selected) || is.null(color) || is.null(alpha) ){
    return(NULL)
  }
  else{
   configName <-  switch (cao.selected,
                          Original = "original",
                          Simulated = "original",
                          Repaired = "rep",
                          Segmented = "seg",
                          "Rep. & Seg." = "repAndSeg")
    
   # Update
   rv$dash_view[[configName]] <- list( color = color, alpha = alpha)
  }
}

unblock_render_button <- function(rv ,input, session){
  # Get selected meshes
  mesh.selected <- input$dash_view_mesh
  # Additional deps
  input$dash_view_cao_color
  input$dash_view_cao_alpha
  input$dash_view_quality
  input$dash_view_cao_selector
  if(is.null(mesh.selected)){
    if( !is.null(isolate(input$dash_view_render) ) )
      updateButton(session, "dash_view_render", disabled = T)
  }
  else{
    updateButton(session, "dash_view_render", disabled = F)
  }
}

render_somas <- function(rv, session, input, output){
  
  # Selected soma (clean)
  soma.selected <- input$dash_view_selector
  soma.selected <- sub(":[^:]*$","",soma.selected,perl=T)
  
  # Selected versions (cannot be null since button is disabled in that case)
  soma.types <- input$dash_view_mesh
  
  # Build list for each soma type
  createAlert(session,"dash_view_alert","pleaseWait_dv",title = "Please wait", content = "Loading somas...", style  ="info" , append = F)
  somalist <- lapply(soma.types, function(x, name,rv){
    
    # Get Config
    configName <-  switch (x,
                           Original = "original",
                           Simulated = "original",
                           Repaired = "rep",
                           Segmented = "seg",
                           "Rep. & Seg." = "repAndSeg")
    
    color <- rv$dash_view[[configName]]$color
    alpha <- rv$dash_view[[configName]]$alpha
    
    # Get data
    data <-switch (x,
                   Original = rv$files$real[[name]]$original$data,
                   Simulated = rv$files$simulated[[name]]$data,
                   Repaired = read_binary_PLY(rv$files$real[[name]]$repaired$data),
                   Segmented = rv$files$real[[name]]$segmented$data,
                   "Rep. & Seg." = rv$files$real[[name]]$rep_and_seg$data)
    
    return(list( vertices = data$vertices, faces = data$faces, color = color, alpha = alpha, name = x))
    
  },soma.selected,rv)
  
  createAlert(session,"dash_view_alert","pleaseWait_dv2",title = "Please wait", content = "Rendering process may take some time..", style  ="info" , 
              append = F, dismiss = T)
  output$dash_view_out <- renderScene({ renderMeshes3D( somalist, quality = input$dash_view_quality )})
  
  # Disable render (double render)
  updateButton(session,"dash_view_render",disabled = T)
}
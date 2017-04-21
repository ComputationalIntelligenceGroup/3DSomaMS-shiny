update_dl_preproc_selector <- function( rv , output, session ){
  
  write( "[CALL] UPDATE DL SELECTOR" , file=stderr())
  
  # Create hcbox
  if( nrow(rv$soma$summary.data) > 0 ){
    
    structure <- build_hcbox_selector( rv$soma$summary.data, NULL,
                                       includePkgName = rv$config$selectors.packageName,
                                       removeEmpty = rv$config$selectors.removeEmpty,
                                       withSimulations = F,
                                       byPackage = F) # We need to fix the swap thing
    
    output$dl_pproc_selector_ui <- renderUI({
      hCBoxInput("dl_pproc_selector", 
                               structure,
                               label = NULL,
                               skipTopLevels =  T )
      
    })
    closeAlert(session,"load_pproc_nochar")

  }
  else{
    output$dl_pproc_selector_ui <- renderUI({ tags$p("No data has been loaded yet") })
    createAlert(session,"dl_pproc_alert","load_pproc_nochar", title = "No data available",
                content = "No soma have been loaded yet. Go to Data loading -> load data to load somas",
                style = "warning", dismiss = F, append = F )
  }
}
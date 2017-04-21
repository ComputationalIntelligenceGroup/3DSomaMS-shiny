#' Update simulation summare datatable
#'
sim_dash_dt_update <- function(rv , output ){
  
  # Get sim names
  sim_ids <- names(rv$soma$simulation)
  
  sim.df <- data.frame(sim_id = character(0), 
                       model = character(0),
                       cluster = character(0),
                       nsism = numeric(0),
                       stringsAsFactors = F)
  # Render dt
  colnames(sim.df) <- c("Simulation ID", "Model", "Cluster", "# Somas")
  
  if(length(sim_ids)>0){
    # Populate
    for( i in 1:length(sim_ids)){
      sim.df[i,] <- c( 
        sim_ids[i],
        rv$soma$simulation[[ sim_ids[i] ]]$model,
        rv$soma$simulation[[ sim_ids[i] ]]$cluster,
        rv$soma$simulation[[ sim_ids[i] ]]$nsomas)
    }
  }

    output$sim_summary_dt <- renderDataTable( sim.df ,options = list( pageLength = 10, lengthChange = FALSE,
                                                                      info = FALSE, searching = FALSE, searchable = NULL))

}
#' Dashboard body ui builder functions
#'

somaMS_build_body <- function(){
  
  ## Tabbed pages
  tabItems(
    # Dashboard
    somaMS_build_dashboard_summary_tab(),
    
    # Dashboard -> preview
    somaMS_build_dashboard_3dvisualization_tab(),
    
    # Data loading
    somaMS_build_dataloading_summary_tab(),
    
    # Data loading -> files
    somaMS_build_dataloading_files_tab(),
    
    # Data loading -> preproc
    somaMS_build_dataloading_preproc_tab(),
    
    # Data loading -> characterization
    somaMS_build_dataloading_characterization_tab(),
    
    # Data loading -> download somas
    somaMS_build_dataloading_download_tab(),
    
    # Data modelling
    somaMS_build_modelling_summary_tab(),
    
    # Data modelling -> view
    somaMS_build_modelling_view_tab(),
    
    # Data modelling -> new
    somaMS_build_modelling_new_tab(),
    
    # Simulation
    somaMS_build_simulation_summary_tab(),
    
    # Simulation -> new
    somaMS_build_simulation_new_tab(),
    
    # Validation
    #somaMS_build_validation_summary_tab(),
    
    # Validation -> new
    #somaMS_build_validation_new_tab(),
    
    # Info
    somaMS_build_info_tab()#,
    
    # Config
    #somaMS_build_config_tab()
  )
}
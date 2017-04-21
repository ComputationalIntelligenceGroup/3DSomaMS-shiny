
somaMS_setReactiveValues <- function(){
  
  # Create element
  rv <- reactiveValues() 
  
  # Storage vars
  rv$files <- files_rv()
  
  # Config var
  rv$config <- somaMS_config_rv()
  
  # Soma vars
  rv$soma <- soma_rv()
  
  # Sidebar menu
  rv$sidebar_menu <- sidebar_menu_rv()
 
  # Alert menu
  rv$alert_menu <- alert_menu_rv()
  
  # Task menu
  rv$task_menu <- task_menu_rv()
  
  # Dash view vars
  rv$dash_view <- dash_3dview_rv()
  
  return (rv)
}
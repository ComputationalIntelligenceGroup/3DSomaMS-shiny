#' Dashboard sidebar ui builder functions (server side )
#'

# Initialize
sidebar_menu_rv <- function (){
  
  sb_dataloading_files <- list ( id = "sb_dataloading_files", icon = icon("file-archive-o"), label="Load files", 
                                 badgeLabel = NULL, badgeColor = "green",
                                 tabName = "dataloading_files", children = NULL, visible = T  )
  
  sb_dataloading_preproc <- list ( id = "sb_dataloading_preproc", icon = icon("cubes"), label="Preprocessing", 
                                   badgeLabel = NULL, badgeColor = "green",
                                   tabName = "dataloading_preproc", children = NULL, visible = T  )
  
  sb_dataloading_charact <- list ( id = "sb_dataloading_charact", icon = icon("table"), label="View data table", 
                                   badgeLabel = NULL, badgeColor = "green",
                                   tabName = "dataloading_characterization", children = NULL, visible = T  )
  
  sb_dataloading_down <- list ( id = "sb_dataloading_download", icon = icon("download"), label="Download somas", 
                                   badgeLabel = NULL, badgeColor = "green",
                                   tabName = "dataloading_download", children = NULL, visible = T  )
  
  sb_dashboard_3dvisualization <- list ( id = "sb_dashboard_3dvisualization", icon = icon("eye"), label="3D Visualization", 
                                   badgeLabel = NULL, badgeColor = "green",
                                   tabName = "3dvisualization", children = NULL, visible = T  )
  
  sb_modeling_newmodel <- list ( id = "sb_modelling_new", icon = icon("bolt"), label="New model", 
                                 badgeLabel = NULL, badgeColor = "green",
                                 tabName = "modelling_new", children = NULL, visible = T  )
  
  sb_modeling_viewmodel <- list ( id = "sb_modelling_view", icon = icon("eye"), label="Explore model", 
                                 badgeLabel = NULL, badgeColor = "green",
                                 tabName = "modelling_view", children = NULL, visible = T  )
  
  sb_simulation_newsim <- list ( id = "sb_simulation_new", icon = icon("play"), label="New simulation", 
                                 badgeLabel = NULL, badgeColor = "green",
                                 tabName = "simulation_new", children = NULL, visible = T  )
  
#   sb_validation_newval <- list ( id = "sb_validation_newval", icon = icon("square"), label="Run validation", 
#                                  badgeLabel = NULL, badgeColor = "green",
#                                  tabName = "validation_new", children = NULL, visible = T  )
#   
  return ( list(
    sb_dashboard = list ( id = "sb_dashboard", icon = icon("dashboard"), label="Dashboard", 
                                         badgeLabel = NULL, badgeColor = "green",
                                         tabName = "dashboard", children = list(sb_dashboard_3dvisualization), visible = T ),
    
    sb_dataloading = list ( id = "sb_dataloading", icon = icon("database"), label="Data loading", 
                                           badgeLabel = NULL, badgeColor = "green",
                                           tabName = "dataloading", children = list(sb_dataloading_files,sb_dataloading_preproc,sb_dataloading_charact,sb_dataloading_down),
                                           visible = T  ),
      
    sb_modelling = list ( id = "sb_modelling", icon = icon("stats", lib="glyphicon"), label="Modelling", 
                                         badgeLabel = NULL, badgeColor = "green",
                                         tabName = "modelling_dash", children = list(sb_modeling_newmodel, sb_modeling_viewmodel), 
                                         visible = T  ),
      
    sb_simulation = list ( id = "sb_simulation", icon =  icon("cogs"), label="Simulation", 
                                          badgeLabel = NULL, badgeColor = "green",
                                          tabName = "simulation", children = list(sb_simulation_newsim), visible = T  )
      
#    sb_validation = list ( id = "sb_validation", icon = icon("check-square"), label="Validation",
#                                          badgeLabel = NULL, badgeColor = "green",
#                                          tabName = "validation", children = list(sb_validation_newval), visible = T  )
  ) )
}

create_sidebar_menu_item <- function(values){
  
  if( ! is.null(values$children) )
    return(customMenuItem( values$label, lapply(values$children, function(x){ isolate( create_sidebar_menu_item(x) ) }),
                   icon = values$icon, badgeLabel = values$badgeLabel, badgeColor = values$badgeColor,
                   tabName = values$tabName, hasTab = T))
  else
    return(menuItem( values$label,
                     icon = values$icon, badgeLabel = values$badgeLabel, badgeColor = values$badgeColor,
                     tabName = values$tabName))
}

create_sidebar_menu <- function(values, output){
  
  # Get values from react.sidebar_status and create menu Item
  output[[values$id]] <- renderMenu(create_sidebar_menu_item(values))
  
}

##
# Watchers
##
watcher.sb_menu.dashboard <- function(rv,output){
  create_sidebar_menu(rv$sidebar_menu$sb_dashboard, output)
}

watcher.sb_menu.dataloading <- function(rv,output){
  create_sidebar_menu(rv$sidebar_menu$sb_dataloading, output)
}

watcher.sb_menu.modelling <- function(rv,output){
  create_sidebar_menu(rv$sidebar_menu$sb_modelling, output)
}

watcher.sb_menu.simulation <- function(rv,output){
  create_sidebar_menu(rv$sidebar_menu$sb_simulation, output)
}

#watcher.sb_menu.validation <- function(rv,output){
#  create_sidebar_menu(rv$sidebar_menu$sb_validation, output)
#}
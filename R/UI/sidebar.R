
somaMS_build_sidebar <- function(){
  tagList( somaMS_build_sidebar_menu(),
           br(),
           hr(),
           somaMS_build_sidebar_controls()
           #,
           #br(),
           #hr()
           #,somaMS_build_sidebar_footer() 
           )
}

somaMS_build_sidebar_menu <- function(){
  ##
  #  Sidebar Menu
  ##
  sidebarMenu(
    
    
    #Dashboard
    # menuItem( "Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItemOutput("sb_dashboard"),
    
    # Data loading
    #menuItem( "Data loading", tabName = "dataloading", icon = icon("database")),
    menuItemOutput("sb_dataloading"),
    
    # Modelling
    #menuItem( "Modelling", tabName = "modelling", icon = icon("stats", lib="glyphicon")),
    menuItemOutput("sb_modelling"),
    
    # Simulation
    #menuItem( "Simulation", tabName = "simulation", icon = icon("cogs")),
    menuItemOutput("sb_simulation"),
    
    # Validation
    #menuItem( "Validation", tabName = "validation", icon = icon("check-square")),
    #menuItemOutput("sb_validation"),
    br(),
    column(4, menuItem("",icon = icon("info-circle", class = "big-icon") , tabName = "info_tab", selected = T),offset=4),
    br(),
    br(),
    # Menu id
    id  ="sb_menu"
  )
}


somaMS_build_sidebar_controls <- function(){
  sidebarMenu(
    uiOutput("sb_controls")
  )
}

somaMS_build_sidebar_footer <- function(){
  ##
  #  Sidebar Menu
  ##
  sidebarMenu(
    #column(5, menuItem("",icon = icon("gear", class = "big-icon") , tabName = "config_tab", selected = F),offset = 1),
    column(1, menuItem("",icon = NULL , tabName = "invisible", selected = T),offset=11),
    id = "footer_controls"
  )
}

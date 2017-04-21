#' changes sidebar controls to match current tab
#' 
sidebar_controls.tabchange <- function(input, output){
  
  write("[CALL] SIDEBAR CONTROLS TABCHANGE", file=stderr())
  
  if( !is.null(input$sb_menu) ){
    func <- switch (input$sb_menu,
                    #'3dvisualization' = sidebar_controls.visualization,
                    'dataloading_files' = sidebar_controls.dl_files,
                    #'dataloading_preproc' = sidebar_controls.preproccess,
                     # 'modelling_new' = sidebar_controls.modelling,
                    #'simulation_new' = sidebar_controls.simulation,
                    'validation_new' = sidebar_controls.validation,
                     default = NULL )
    
    if(is.null(func)){
      output$sb_controls <- renderUI(NULL)
    }
    else
      output$sb_controls <- renderUI(func())
  }
  else
    output$sb_controls <- renderUI(NULL)
}

sidebar_controls.visualization <- function(){
  radioButtons("vis_quality","Shadow quality", c("High", "Medium", "Low"), selected = "High")
}

sidebar_controls.preproccess<- function(){
  checkboxGroupInput("pre_repandseg","Preprocess operations", choices = c("Repair", "Segment", "Characterize"),selected = c("Repair", "Segment", "Characterize") )
}

sidebar_controls.modelling<- function(){
  sliderInput("mod_gaussrange","Range of Gaussian mixtures",min = 2,max=20,step = 1,value = c(2,10))
}

sidebar_controls.simulation<- function(){
  selectInput("sim_model", "Model", c("A","B","C"), "A")
}

sidebar_controls.validation<- function(){
  numericInput("val_rep","Number of repetitions",value = 10,min = 2,max=100,step=1)
}

sidebar_controls.dl_files <- function(){
  tagList(
    textInput("dl_package", "Package name"),
    #fileInput("dl_file", "File explorer", multiple = T, accept = infiles.accepted.types),
    fileInput("dl_file", "File explorer", multiple = T),
    column(8, bsButton("dl_upstart", "Load files", block = T, style = "primary", disabled = T), offset=2),
    br()
  )
    
}

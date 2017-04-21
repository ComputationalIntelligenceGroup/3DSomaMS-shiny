#' Create visualization reactive values
#'
dash_3dview_rv <- function(){
  
  dash_3dview <- list()
  
  dash_3dview$original <- list()
  dash_3dview$original$color <- "#FFFFFF"
  dash_3dview$original$alpha <- 1.0
  
  dash_3dview$rep <- list()
  dash_3dview$rep$color <- "#FF0000"
  dash_3dview$rep$alpha <- 0.3
  
  dash_3dview$seg <- list()
  dash_3dview$seg$color <- "#0000FF"
  dash_3dview$seg$alpha <- 0.3
  
  dash_3dview$repAndSeg <- list()
  dash_3dview$repAndSeg$color <- "#00FF00"
  dash_3dview$repAndSeg$alpha <- 0.3
  
  return(dash_3dview)
}
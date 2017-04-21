library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyHierarchicalCbox)
library(threejs)
library(threejswrapper)
library(tools)
library(SomaMS)
library(ShinyD3Bn)
library(ggplot2)

## SET THIS!
options(SomaMS.meshlabserver.path="/home/luis/package_sources/MESHLAB_133/meshlab_working/meshlab/distrib/meshlabserver")

# Make logos (and static imgs) available
addResourcePath('imgs', "inst/imgs/")
addResourcePath('fancybox', "inst/fancybox/")

# Call sources! 
somaMS_serverFiles <- "server/"
somaMS_UIFiles <- "UI/"
somaMS_auxFiles <- "auxiliar/"

sapply(list.files(path = somaMS_serverFiles, pattern = "*.R", recursive = T , full.names=TRUE, ignore.case = T), debugSource, .GlobalEnv)
sapply(list.files(path = somaMS_UIFiles, pattern = "*.R", recursive = T , full.names=TRUE, ignore.case = T), debugSource, .GlobalEnv)
sapply(list.files(path = somaMS_auxFiles, pattern = "*.R", recursive = T , full.names=TRUE, ignore.case = T), debugSource, .GlobalEnv)

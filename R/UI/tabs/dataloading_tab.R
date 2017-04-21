
somaMS_build_dataloading_summary_tab <- function(){
  tabItem( tabName = "dataloading",
           fluidPage(
             somaMS_create_dl_valueboxes(),
             hr(class="hr-greencolor"),
             h1("Summary table"),
             fluidRow( 
               column(10,
                dataTableOutput("dl_summary"),
                offset = 1
               )
             )
           )
  )
}


somaMS_create_dl_valueboxes <- function(){
  fluidRow( 
            valueBoxOutput("dl_vbox_repaired",width = 3),
            valueBoxOutput("dl_vbox_oproc",width = 3),
            valueBoxOutput("dl_vbox_repproc",width = 3),
            valueBoxOutput("dl_vbox_char",width = 3) ) 
}


somaMS_build_dataloading_files_tab <- function(){
  tabItem( tabName = "dataloading_files",
           fluidPage(
             h1( "Load PLY and/or VRML files"),
             p( "To load a 3D mesh in PLY or VRML format, please use the file explorer on the sidebar to select the files. The set of reconstructions can be grouped in
                a package by filling the \"package\" text field in the sidebar. To speed-up the uploading proccess, files can be also be in ZIP format."),
             p("Metadata XML files can be also uploaded at the same time. Data and metadata files will be paired by name. For example: this_soma.xml will be processed as
               the metadata relative to this_soma.ply or this_soma.vrml."),
             p("Any errors detected during the upload or the parsing phase will be displayed in this same page. Files with errors will not be uploaded into 3DsomaMS. 
               After completion, please proceed to \"preprocessing\"."),
             hr(class="hr-greencolor"),
             h2( "Process log"),
              bsAlert("dl_files_alert"),
              uiOutput("dl_files_log")
           )
  )
}

somaMS_build_dataloading_preproc_tab <- function(){
  tabItem( tabName = "dataloading_preproc",
           fluidPage(
             h1( "Process loaded files"),
             p(" Original 3D reconstructions of the soma can present holes produced in the staining proccess, also the initial part of the neurites can be part of the original mesh."),
             p(" To address these issues, SomaMS provides two preprocessing algoriths:"),
             tags$ul(
               tags$li( tags$b("Repair algorithm"),  ": Based on the ambient occlusion algorithm and then applying clustering techniques. It fills the holes in the soma surface and applies smoothing." ),
               tags$li( tags$b("Segmentation algorithm"),  ":Based on the shape diameter function value. Distinguish between soma and neurites, removing the latest." )
             ),
             p(" Finally, the", tags$b( "characterization process" ), "mesures descriptive morphological variables from the mesh that are used in the modelling and simulation steps."),
             "More information about the preprocessing step can be found in the article: ",
             "Luengo-Sanchez et al. ", tags$a(href="http://www.frontiersin.org/Journal/Abstract.aspx?s=742&name=neuroanatomy&ART_DOI=10.3389/fnana.2015.00137","A univocal definition of the neuronal soma morphology using Gaussian mixture models"), 
             ",",tags$b(" Frontiers in Neuroanatomy"),
             "vol. 9, 2015.",
             hr(class="hr-greencolor"),
             bsAlert("dl_pproc_alert"),
             # Three cols: first to select files, second to select processes and third to output reports? maybe 2 is better
             column(6, 
                    box( title = "Soma selector", status = "primary", solidHeader = T, width = NULL, collapsible = T, uiOutput("dl_pproc_selector_ui") ),
                    box( title = "Operations", status = "primary", solidHeader = T, width = NULL,  ccollapsible = T,
                         checkboxGroupInput("dl_pproc_op","Preprocess operations",
                                                           choices = c("Repair", "Segment", "Characterize"),
                                                           selected = c("Repair", "Segment", "Characterize") ),
                                                           bsButton("dl_pproc_run", "Run processes", style = "primary" , disabled = T ) )  
             ),
             column(6, 
                    box( title = "Process log", solidHeader = T, status = "info", width = NULL,verbatimTextOutput("dl_pproc_log"), collapsed = T, collapsible = T )
                    ) 
           )
  )

}

#' update table preview tab
#' 
#' NOT REACTIVE!
create_dl_table_selector <- function (input, label, onlyDataColumns = F, allChecked=F, ...){
  
  selector_structure <- list()
  
  if(!onlyDataColumns){
    # Create name and from 
    selector_structure[[1]] <- hcbox_item("name", label = "Cell", selected = T)
    selector_structure[[2]] <- hcbox_item("from", label = "From", selected = T)
    selector_structure[[3]] <- hcbox_item("package", label = "Package", selected = T)
    selector_structure[[4]] <- hcbox_item("simulation_id", label = "Simulation id", selected = T)
    offset_str <- 4
  }
  else offset_str <- 0
  
  cardinality <- c( 6, 6, 6, 6 , 6, 6, 6, 6, 6, 6, 7)
  variables <-  c("phi", "theta", "r_h", "e", "PCA_phi", "PCA_theta", "w", "b_0", "b_1", "b_2", "height" )
  
  for( i in 1:length(variables) ){
    child <- list()
    
    facename <- gsub("_"," ",variables[i],fixed = T)
    
    # Create children
    for(j in 1:cardinality[i])
      child[[j]] <- hcbox_item(paste0(variables[i],j), label = paste(facename,j,sep=" "), selected = allChecked )
    
    # Create box
    selector_structure[[i+offset_str]] <- hcbox_item(variables[i], label = facename, clist = child, selected = allChecked )
  }
  
  # Create selector
  return( hCBoxInput(inputId = input,label = NULL,structure = selector_structure, ...) )
}

somaMS_build_dataloading_characterization_tab <- function(){
  tabItem( tabName = "dataloading_characterization",
           fluidPage(
             h1( "Characterizations data table"),
             p("The characterization table displays all variables measured in the characterization proccess (one row per soma). On the left side you can select which variables to show in the table in the column selector. Due to the high number of variables the table may not display propperly if too many variables are selected."),
             p("If you already have characterized a set of somas in a previous session, you can import a CSV table with the previous data and avoid to go through the repair, segment and characterize process. To do so, just select the CSV file in the file explorer under the column selector and click import."),
             p("To export the table in CSV format, click the export button under the data table. The whole table will be saved, no matter which columns are active in the column selector."),
             hr(class="hr-greencolor"),
             bsAlert("dl_table_alert"),
             column(4, 
                box( title = "Column selector", status = "primary", solidHeader = T, width = NULL, collapsible = T,
                     create_dl_table_selector("dl_table_colsel","Column selector") ),
                box ( title = "Import data tables", status = "info", solidHeader = T, width = NULL, collapsible = F,
                      fileInput("dl_table_file", label = "Select CSV file"),
                                bsButton("dl_table_load", label = "Import", style = "primary"))
             ),
             column( 8,
                     dataTableOutput("dl_table_dt"),
                     uiOutput("dl_table_save_ui") )
                    #bsButton("dl_table_save", label="Export as CSV", style = "primary"))
           )
  )
}


somaMS_build_dataloading_download_tab <- function(){
  tabItem( tabName = "dataloading_download",
           fluidPage(
             h1("Download somas"),
             p("All uploaded and processed somas in the application can be downloaded in binary PLY format. Use the control on the left to select which somas you want to download, then click on the button to download a ZIP file with all selected somas."),
             hr(class="hr-greencolor"),
             bsAlert("dl_download_alert"),
             # Three cols: first to select files, second to select processes and third to output reports? maybe 2 is better
             column(6, 
                    box( title = "Soma selector", status = "primary", solidHeader = T, width = NULL, collapsible = T, uiOutput("dl_download_selector_ui") ),
                    downloadButton("dl_download_save")
             ),
             column(6, 
                    box( title = "Process log", solidHeader = T, status = "info", width = NULL,verbatimTextOutput("dl_download_log"), collapsed = T, collapsible = T )
             ) 
           )
  )
  
}
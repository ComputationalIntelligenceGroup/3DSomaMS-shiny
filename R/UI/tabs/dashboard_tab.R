#somaMS_build_dashboard_tab <- function(){
#  span(somaMS_build_dashboard_summary_tab(), somaMS_build_dashboard_3dvisualization_tab() )
#}

###
# SUMMARY
##
somaMS_build_dashboard_summary_tab <- function(){
  tabItem( tabName = "dashboard",
           fluidPage(
              somaMS_create_dash_valueboxes(),
               hr(class="hr-greencolor"),
               h1("Summary table"),
               fluidRow(
                 column(10,
                  dataTableOutput("dash_summary"),
                  offset = 1
                 )
               )
           )
  )
}

somaMS_create_dash_valueboxes <- function(){
  fluidRow( valueBoxOutput("dash_vbox_neur"),
            valueBoxOutput("dash_vbox_mod"),
            valueBoxOutput("dash_vbox_sim"))
}


###
# 3D visualization
##
somaMS_build_dashboard_3dvisualization_tab <- function(){
  tabItem( tabName = "3dvisualization",
           fluidPage(
             h1( "3D visualization"),
                       p("The 3D visualization in this tab is implemented with ThreeJS, a javascript library that uses webGL. If you experience any problem during the visualization update your browser and/or check that your browser supports webGL."),
                       p("To visualize an uploaded soma, select it using the soma selector on the left side. If no soma has been uploaded yet, please proceed to data load tab. Once a soma has been selected, in the next box you will see the different versions available, which are: Original, Repaired, Segmented, Repaired & Segmented, Simulation."),
                       p("For each version, you can set its color and opacity (from 0 to 1). Any version of a soma can be plotted at the same time. Finally, you can select the render quality. The better the quality, the slower the rendering process will be. After all parameters have been set you can click on the render button."),
                       hr(class="hr-greencolor"),
                       bsAlert("dash_view_alert"),
                       fluidRow(
                          column(3,
                                # File hierarchy
                                box( title = "Soma selector",width = NULL, collapsible = T,
                                     status = "primary", solidHeader = T, uiOutput("dash_view_selector_ui") ),
                                # Controls
                                box( side = "right", width = NULL,collapsible = T,
                                     title = tagList(shiny::icon("gear"), "Options"), status = "primary", solidHeader = T,
                                     h3("Mesh selection"),
                                     uiOutput("dash_view_mesh_ui"),
                                     hr(class="hr-bluecolor"),
                                     h3("Color and opacity"),
                                     uiOutput("dash_view_cao_selector_ui"),
                                     uiOutput("dash_view_cao_lower_ui"),
                                     hr(class="hr-bluecolor"),
                                     h3("Renderer options"),
                                     radioButtons("dash_view_quality","Render quality", c("High", "Medium", "Low"), selected = "Medium") ),
                                column(12,
                                       bsButton("dash_view_render", "Render", style = "primary", block = T, disabled = T)
                                )
                             ),
                             column(9, sceneOutput("dash_view_out", width = "100%", height="600px")))
           )
    )
}

somaMS_build_simulation_summary_tab <- function(){
  
  tabItem( tabName = "simulation",
           fluidPage(
            tags$h1("Simulation summary"),
            dataTableOutput("sim_summary_dt")
           )
  )
}

somaMS_build_simulation_new_tab <- function(){
  
  tabItem( tabName = "simulation_new",
           fluidPage(
             h1( "Run new simulation"),
             p( "Based on models built in the previous step, 3DSomaMS can create new characterizations and from them create a 3D mesh, a simulated soma." ),
             p( "To run a simulation select which model will be used as reference in the Model selector. Name the simulation to identify it afterwards. Then, in the simulation parameters box, choose one of the clusters. To sample from all of the clusters according to the priori distribution of the cluster select All. Finally select the number of somas to simulate and click Run simulation to start.. You must also name the simulation to identify it afterwards." ),
             hr(class="hr-greencolor"),
             bsAlert("sim_new_alert"),
             uiOutput("sim_new_model_ui"),
             conditionalPanel("input.sim_new_model",
             column(6, 
                    box( title = "Simulation Parameters", status = "primary", solidHeader = T, width = NULL,  ccollapsible = T,
                          
                          textInput("sim_new_name",label = "Simulation name"),
                          uiOutput("sim_new_cluster_ui"),
                          numericInput("sim_new_nsomas", label = "Number of simulations", value = 20, min = 1 , max = 500, step = 1)),
                          # Run methods
                          column(8 , bsButton("sim_new_run", "Run simulation", style = "primary" , disabled = T, block = T ), offset = 2 )
                    
             ),
             column(6, 
                    box( title = "Simulation log", solidHeader = T, status = "info", width = NULL,verbatimTextOutput("sim_new_log"), collapsed = T, collapsible = T )
             )
           )
           )
  )
}
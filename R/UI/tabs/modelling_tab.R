somaMS_build_modelling_tab <- function(){
  tagList(somaMS_build_modelling_new_tab(),
          somaMS_build_modelling_view_tab(),
          somaMS_build_modelling_summary_tab()
          )
}

somaMS_build_modelling_summary_tab <- function(){
  tabItem( tabName = "modelling_dash",
           fluidPage(
             fluidRow( valueBoxOutput("mod_vbox_models"),
                       valueBoxOutput("mod_vbox_arcs"),
                       valueBoxOutput("mod_vbox_clusters")),
             hr(class="hr-greencolor"),
             h1("Summary table"),
             fluidRow(
               column(10,
                      dataTableOutput("mod_summary"),
                      offset = 1
               )
             )
           )
  )
}

somaMS_build_modelling_new_tab <- function(){
  tabItem( tabName = "modelling_new",
           fluidPage(
             h1( "Create new model"),
             p( "3DSomaMS builds a model based on a  Gaussian Bayesian network with the variables measured in the characterization step. Then it applies clustering techniques to identify groups of somas with common characteristics." ),
             p( "To create a new model use the data selector on the left side to select which characterizations will be used as source data for the new model. You must also name the new model to identify it afterwards." ),
             p( "There are 4 parameters setteable in model building phase: "),
             tags$ul(
               tags$li( tags$b("Number of reboots"), ":  Number of times that the bootstrap process will learn the Bayesian structure." ),
               tags$li( tags$b("Significant threshold"), ":  Threshold over percentage of times that an arc appeared in the boostrap structures to be included in the final structure." ),
               tags$li( tags$b("Number of clusters"), ":   Range that determines the number of clusters to consider. The final clustering will be selected based on its BIC score." ),
               tags$li( tags$b("Initialization method"), ":  Initialization method to build the clusters, it can be K-means or at random initialization." )
             ),
             hr(class="hr-greencolor"),
             bsAlert("mod_new_alert"),
             column(6, 
                    box( title = "Data selector", status = "primary", solidHeader = T, width = NULL, collapsible = T, uiOutput("mod_new_selector_ui") ),
                    box( title = "Parameters", status = "primary", solidHeader = T, width = NULL,  ccollapsible = T,
                         textInput("mod_new_name",label = "Model name"),
                         numericInput("mod_new_nboots", label = "Number of reboots", value = 200, min = 1 , max = 500, step = 1),
                         numericInput("mod_new_sigthreshold", label = "Significant threshold", value = 0.95, min = 0.01, max = 1, step = 0.01),
                         sliderInput("mod_new_nclust", label = "Number of clusters", value = c(2,3), min = 1, max = 7, step=1),
                         radioButtons("mod_new_initmethod", label = "Initialization method", choices = c("Kmeans","Random"), selected = "Kmeans") ),
                    # Run methods
                    column(8 , bsButton("mod_new_run", "Create model", style = "primary" , disabled = T, block = T ), offset = 2 )
                    
             ),
             column(6, 
                    box( title = "Process log", solidHeader = T, status = "info", width = NULL,verbatimTextOutput("mod_new_log"), collapsed = T, collapsible = T )
             )
    )
  )
}

somaMS_build_modelling_view_tab <- function(){
  tabItem( tabName = "modelling_view",
           fluidPage(
           h1( "Model visualization"),
           p( "In this tab you can explore every model in the application. After selecting a model, the list of characterizations and the view selector are in the first section. There are three detailed views available, one for each component of the model:"),
           tags$ul(
             tags$li( tags$b("Source data view"),  ":  Displays the data used to create the model, similar to the view data table tab under data loading. Here you can explore the data and download it in CSV format." ),
             tags$li( tags$b("Bayesian Network"),  ":  Shows the Bayesian network structure as well as the marginal distribution of each node (left click on a node). You can also select which variables are represented in the variable selector on the left."),
             tags$li( tags$b("Cluster"),  ":  Plots instances and cluster centroids in a 3D scatterplot using multidimensional scaling." )
           ),
           hr(class="hr-greencolor"),
           bsAlert("mod_view_alert"),
           fluidRow( uiOutput("mod_view_sel_ui"), 
                     downloadButton("mod_view_down", label = "Download model") ),
           hr(class="hr-greencolor"),
           conditionalPanel(condition = "input.mod_view_sel",
                            tags$h1("Model general info"),
                            # Value boxes
                            fluidRow( 
                              valueBoxOutput("mod_view_vbox_instances",width = 3),
                              valueBoxOutput("mod_view_vbox_variables",width = 3),
                              valueBoxOutput("mod_view_vbox_bnarcs",width = 3),
                              valueBoxOutput("mod_view_vbox_clusters",width = 3) ),
                            # Data
                            fluidRow(
                              column(9,  dataTableOutput("mod_view_modeldata") ),
                              column(3,  checkboxGroupInput("mod_view_views", "Additional views",
                                                            choices = c("BN model", "Clusters", "Source data"),
                                                            selected = c("BN model") ) )
                            ),
                            # Sep
                            fluidRow(hr(class="hr-greencolor")),
                            # BN model
                            conditionalPanel("input.mod_view_views.indexOf('BN model')>-1",
                                tags$h3("Bayesian network view"),
                                
                                bsModal("mod_view_bncond","Node Conditional Probability",NULL,
                                        
                                          # Node name
                                        tags$h3("Node Info"),
                                        fluidRow(
                                          column(6,
                                            # Name
                                            tags$b("Name: ") ,textOutput("mod_view_bnmodal_nodename", inline = T) , tags$br(),
                                            # Value
                                            tags$b("Current value: ") , numericInput("mod_view_bnmodal_nodeval", value = NULL, label = NULL) ,
                                            # F
                                            tags$b("f(x): "), textOutput("mod_view_bnmodal_node_fval", inline = T)  
                                          ),
                                          column(6,
                                            tags$ul(
                                            tags$li(tags$b("Coefficients: ") ,textOutput("mod_view_bnmodal_node_eq", inline = T) ) ,
                                            tags$li(tags$b("Mean (mu): ") ,textOutput("mod_view_bnmodal_node_mu", inline = T) ),
                                            tags$li(tags$b("Deviation (sigma): ") ,textOutput("mod_view_bnmodal_node_sigma", inline = T) ) )
                                          )
                                        ),
                                        fluidRow(hr(class="hr-greencolor")),
                                        fluidRow(
                                          column(6,
                                            # Children
                                            uiOutput("mod_view_modal_children_ui"),
                                            
                                            # Parents
                                            uiOutput("mod_view_modal_parents_ui")
                                          ),
                                          column(6,
                                                 plotOutput("mod_view_bnmodal_plot_2d")
                                          )
                                        ),
                                        size="large"),
                                
                                column(4, 
                                          selectInput("mod_view_bnmodal_cluster",choices = NULL, label = "Cluster", multiple = F),
                                          box( title = "Variable selector", status = "primary",
                                               solidHeader = T, width = NULL, collapsible = T,
                                               create_dl_table_selector("mod_view_bnvarsel","Variable selector",
                                                                        onlyDataColumns = T, allChecked = F) )
                                ),
                                column( 8 , BnD3IO("mod_view_bnio") ),
                                fluidRow(hr(class="hr-greencolor"))
                                
                            ),
                            # Clusters
                            conditionalPanel("input.mod_view_views.indexOf('Clusters')>-1",
                                tags$h1(" Clustering visualization "),
                                fluidRow(hr(class="hr-greencolor")),
                                column(7,
                                       tags$h3("Title"),
                                       dataTableOutput("mod_view_clusterprobs")
                                ),
                                column(5,
                                    box( title= "Cluster distance plot (MDS)", status = "primary", solidHeader = T, width = NULL, collapsible = F,
                                    scatterplotThreeOutput("mod_view_clusterscatter",width = "100%",height = "500px") ))
                            ),
                            # Source
                            conditionalPanel("input.mod_view_views.indexOf('Source data')>-1",
                                tags$h3(" Source data view"),
                                column(4, 
                                          box( title = "Column selector", status = "primary",
                                                solidHeader = T, width = NULL, collapsible = T,
                                                create_dl_table_selector("mod_view_sourcevarsel","Column selector") )
                                ),
                                column( 8,
                                        dataTableOutput("mod_view_sourcedt"),
                                        uiOutput("mod_view_savesource_ui") ),
                                fluidRow(hr(class="hr-greencolor"))
                            )
           ))
           
  )
}
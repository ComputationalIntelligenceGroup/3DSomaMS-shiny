
somaMS_build_info_tab <- function(){
  
  tabItem( tabName = "info_tab",
           fluidPage(
             
             tags$h1("3D Soma MS Graphical User Interface"),
             
             fluidRow(tags$p("Welcome to 3DSomaMS, an  R package for soma reparation, segmentation and characterization. This web-based user interface provides access to all main algorithms implemented in 3DSomaMS. The interface is divided in four sections:"),
                             tags$ul(
                               tags$li( tags$b("Dashboard"), ": ", "This section provides a general overview of the application as well as a 3D soma visualizator."),
                               tags$li( tags$b("Data loading"),": ", "In this tab the user can upload new files, preprocces somas and import/export its characterization." ),
                               tags$li( tags$b("Modelling"),": ", "Based on the characterization of the somas, in this page the user can create soma models and visualize them." ),
                               tags$li( tags$b("Simulation"), ": ", "Finally, in the simulation tab the user can generate new somas from the models built in the previous section.")
                             ),
                             "In the boxes below, you can find detailed information about these four sections. If you have any question about 3DSomaMS, this interface or you have found any bug, the contact information is at the end of this page."
             ),
             br(),
             fluidRow(
                      # DASHBOARD BOX
                      box( 
                            # Dashboard 
                            tags$h3("Summary tab"),
                            fluidRow(
                              tags$a(href="imgs/info/big/dash.jpg", class="fancybox", rel="dash",
                              tags$img(src="imgs/info/small/dash.png", alt="Dashboard tab", class="info-screenshot" )),
                              "In the dashboard principal tab you will find a summary table that depicts the status of every soma loaded into the application. Just above this table, there are some boxes that show some key values in the application: number of somas, models and simulations"                            
                              ,style = "padding-right:20px"
                              ),
                            # 3D View
                            tags$h3("3D Visualization tab"),
                            fluidRow(
                              tags$a(href="imgs/info/big/dash_3dview.jpg", class="fancybox", rel="dash",
                              tags$img(src="imgs/info/small/dash_3dview.png", alt="Dashboard tab", class="info-screenshot" )),
                              "The 3D visualization tab contains an interactive 3D plot of any soma in the application. In the controls on the left side you can find a soma selector and the render options, while the 3D plot is on the right side.",
                              br(),
                              "The first step is to select a soma using the soma selector. Once a soma has been selected the options in the box below will be updated. On the options box you can select which versions of the soma you want to plot, and for each version you can set face color and opacity, After setting all the parameters, click on render button to create or update the 3D plot. ",
                              br(),
                              "You can move the camera in the 3D plot using your keyboard, specific controls are detailed on the top-side of the plot."
                              ,style = "padding-right:20px"
                            ),
                            title = "Dashboard", status="info", solidHeader = T, width = 12, collapsible = T , collapsed = T )),
             
             fluidRow(  # DATA LOADING BOX
                        box( 
                            tags$h3("Summary tab"),
                            fluidRow(
                              tags$a(href="imgs/info/big/load_dash.jpg", class="fancybox", rel="load",
                              tags$img(src="imgs/info/small/load_dash.png", alt="Loading tab", class="info-screenshot" )),
                              "The summary tab of the data loading section briefly shows all loaded soma meshes in the app. The upper boxes display how many somas are in each possible status ( repaired, segmented, repaired & segmented and characterized). The summary table contains one row per soma and their number of faces and vertices."
                              ,style = "padding-right:20px"
                            ),
                            tags$h3("Data load tab"),
                            fluidRow(
                              tags$a(href="imgs/info/big/load_files.jpg", class="fancybox", rel="load",
                              tags$img(src="imgs/info/small/load_files.png", alt="Data load tab", class="info-screenshot" )),
                              "In the data load tab the user can upload meshes in VRML or PLY format (or even ZIP files) as well as XML metadata files.",
                              "The controls are located in the sidebar on the left side. The user can select one or more files to upload using the file explorer. A set of somas can be grouped in a package by setting a package name."
                              ,style = "padding-right:20px"
                            ),
                            tags$h3("Preprocessing tab"),
                            fluidRow(
                              tags$a(href="imgs/info/big/load_pproc.jpg", class="fancybox", rel="load",
                              tags$img(src="imgs/info/small/load_pproc.png", alt="Preprocessing tab", class="info-screenshot" )),
                              "In the preprocessing tab user can apply automatic repairing and segmentation algorithms to the previously loaded neurons and also characterize them. 
						                   In the Soma selector box, user can check an individual neuron or select recursively all the files in a folder tree. 
						                   Then, in the Operations box, user points out the algorithms that wants to apply. User can choose any combination of algorithms, that is, segmentation can be applied without reparation or the three algorithms can be chained if all of them are checked.
						                   The repairing step removes holes and cavities on the surface of the neuron. 
						                   The segmentation step provides a way to delimit the neuronal soma discarding dendrites. 
						                   User can follow the steps of the operations in Process log box.
						                   Information about the algorithms can be found in the article: ",
                              "Luengo-Sanchez et al. ", tags$a(href="http://www.frontiersin.org/Journal/Abstract.aspx?s=742&name=neuroanatomy&ART_DOI=10.3389/fnana.2015.00137","A univocal definition of the neuronal soma morphology using Gaussian mixture models"), 
                              ",",tags$b(" Frontiers in Neuroanatomy"),
                              "vol. 9, 2015."
                              ,style = "padding-right:20px"
                            ),
                            tags$h3("Characterization table tab"),
                            fluidRow(
                              tags$a(href="imgs/info/big/load_table.jpg", class="fancybox", rel="load",
                              tags$img(src="imgs/info/small/load_table.png", alt="Char table tab", class="info-screenshot" )),
                              "Per each characterized soma, a new row is added to the characterization data table, that has more than 50 variables. On the left side there are two boxes, the column selector that control which columns of the characterizations data table are displayed, and the import data table control, where previous characterizations in CSV format can be loaded into the application."
                              ,style = "padding-right:20px"
                            ),
                            title = "Data loading", status="info", solidHeader = T, width = 12, collapsible = T , collapsed = T )),
             fluidRow( 
                        # MODELLING BOX
                        box( 
                          tags$h3("Summary tab"),
                          fluidRow(
                            tags$a(href="imgs/info/big/mod_dash.jpg", class="fancybox", rel="mod",
                            tags$img(src="imgs/info/small/mod_dash.png", alt="Model dash tab", class="info-screenshot" )),
                            "Similarly to the Data loading Summary tab, the Modelling Summary tab show general information about the modelled somas. The upper boxes show the number of models available, the average number of arcs of the Bayesian network models and the average number of clusters discovered in the clustering process.
					 Also it displays in a table the model name, the number of instances used to train the model, if it has been repaired and processed (thumb), the name of a set of neurons, the number of arcs in each one of the Bayesian networks and the number of clusters discovered for that model."
                            ,style = "padding-right:20px"
                          ),
                          tags$h3("New model tab"),
                          fluidRow(
                            tags$a(href="imgs/info/big/mod_new.jpg", class="fancybox", rel="mod",
                            tags$img(src="imgs/info/small/mod_new.png", alt="New model tab", class="info-screenshot" )),
                            "In this tab the user can create a new model and:",
                            tags$ul(
                              tags$li("Select the data to build the model"),
                              tags$li("Name the new model"),
                              tags$li("Set the Bayesian network learning parameters"),
                              tags$li("Define the clustering parameters")
                            )
                          ),
                          tags$h3("Model visualization tab"),
                          fluidRow(
                            tags$a(href="imgs/info/big/mod_view_general.jpg", class="fancybox", rel="mod",
                            tags$img(src="imgs/info/small/mod_view_general.png", alt="Model view tab", class="info-screenshot" )),
                            "Given a model, this tab shows detailed information about it. In the general info section some general number such as the number of instances used in the learning process or the number of arcs in the Bayesian network are shown. This section also includes a table with the somas used in the learning process.",
                            "On the right side, there is a checkbox control named Additional views. Each additional view focus on one component of the model, either the Bayesian network, Clusters or the source data."
                            ,style = "padding-right:20px"
                          ), 
                          tags$h4("Model visualization: BN view"),
                          fluidRow(
                            tags$a(href="imgs/info/big/mod_view_bn.jpg", class="fancybox", rel="mod",
                            tags$img(src="imgs/info/small/mod_view_bn.png", alt="View bn", class="info-screenshot" )),
                            "The Bayesian network view contains the representation of the Bayesian network learnt in the first step of the modelling phase. The graph on the right side represents the sub Bayesian network that involves the variables selected on the variable selector. The user can highlight the Markov blanket of a variable by double-clicking on the node."
                            ,style = "padding-right:20px"
                          ), 
                          tags$h4("Model visualization: Source data view"),
                          fluidRow(
                            tags$a(href="imgs/info/big/mod_view_source.jpg", class="fancybox", rel="mod",
                            tags$img(src="imgs/info/small/mod_view_source.png", alt="View source", class="info-screenshot" )),
                            "The goal of this tab is to provide information about the training data. This box is divided in two parts. The first one is the column selector which presents general information about each instance, like the name cell or the package, and the morphological variables of the neuron. The second part is a table that displays the checked values in the first part. The dataset can be downloaded as a CSV file clicking the \"Export as CSV\" button."
                            ,style = "padding-right:20px"
                          ), 
                             title = "Modelling", status="info", solidHeader = T, width = 12, collapsible = T , collapsed = T )),
             fluidRow( 
                        # SIMULATION BOX
                        box(  tags$h3("Summary tab"),
                              fluidRow(
                                tags$a(href="imgs/info/big/sim_dash.jpg", class="fancybox", rel="sim",
                                       tags$img(src="imgs/info/small/sim_dash.png", alt="Simulation dash tab", class="info-screenshot" )),
                                " This view present a table with general information about each simulation. Concretely, information like the name of the simulation, the model from the simulation was run, the cluster number or the number of somas simulated can be found. "
                                ,style = "padding-right:20px"
                              ),
                              tags$h3("New simulation tab"),
                              fluidRow(
                                tags$a(href="imgs/info/big/sim_new.jpg", class="fancybox", rel="sim",
                                       tags$img(src="imgs/info/small/sim_new.png", alt="New simulation tab", class="info-screenshot" )),
                                "Simulation step sample new somas from the probabilistic model. To do that, users selects one of the previously computed models in Model selector. Then, in the Simulation Parameters box, user gives a name to the simulation, selects one cluster of somas or simulate from all of the clusters according to priori probabilities of each cluster and finally introduce the number of new somas to simulate. User can find information about the simulation in the Simulation log box."
                                ,style = "padding-right:20px"
                              ),
                             title = "Simulation", status="info", solidHeader = T, width = 12, collapsible = T , collapsed = T )),
             
             
             fluidRow(hr(class="hr-greencolor")),
              # CIG row
             fluidRow(
                         tags$a( href="http://cig.fi.upm.es/",
                                 tags$img(src="imgs/logo_medq.png", alt="Computational Intelligence Group", style="height:125px; margin-right:40px; margin-left:50px; float:left" )),
                         tags$a( href="http://www.upm.es/institucional",
                                 tags$img(src="imgs/upm_logo.png", alt="Universidad Politécnica de Madrid", style="height:125px; margin-right:80px; margin-left:80px; float:left" )),
                         tags$div(
                                 "3DSomaMS library and this GUI have been developed by the Computational Intelligence Group (CIG) at the Universidad Politecnica de Madrid (Spain).",br(),
                                 tags$b("Contact info:"),
                                 tags$ul(
                                   tags$li( tags$b("Webpage"), ": ", tags$a( href="http://cig.fi.upm.es/","link")),
                                   tags$li( tags$b("3DSomaMS library"),": ", tags$a( href="mailto:sergio.luengo@upm.es","sergio.luengo@upm.es") ),
                                   tags$li( tags$b("GUI"),": ", tags$a( href="mailto:luis.rodriguezl@upm.es","luis.rodriguezl@upm.es") ),
                                   tags$li( tags$b("Phone"), ": +34 - 913363675")
                                 ),
                                 style= "float:left"
                         )
              ),
             fluidRow(hr(class="hr-greencolor")),
              # HBP and EC row
             fluidRow(
                        tags$a( href="https://www.humanbrainproject.eu",
                                tags$img(src="imgs/hbp-logo.png", alt="Human Brain Project", style="height:125px; margin-right:40px; float:left" )),
                        tags$a( href="http://ec.europa.eu/",
                                tags$img(src="imgs/ec_logo.png", alt="European Commision", style="height:125px; margin-right:40px; float:left" )),
                        tags$div(
                          " This work has been supported by European Union's Seventh Framework Programme (FP7/2007-2013) under grant agreement no. 604102 (Human Brain Project).",
                          style= "float:left"
                        )
             )
        )
  )
}

somaMS_build_config_tab <- function(){
  
  tabItem( tabName = "config_tab",
           fluidRow( tags$a(href="#shiny-tab-modelling", "config" ) )
  )
}
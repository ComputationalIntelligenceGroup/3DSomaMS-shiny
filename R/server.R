
shinyServer(function(input, output, session) {
  
  # Initialize reactive values
  rv <- somaMS_setReactiveValues();
  
  # Sidebar menu auto-updater ( see server/sidebar/sidebar_menu.R)
  observe({watcher.sb_menu.dashboard(rv,output)})
  observe({watcher.sb_menu.dataloading(rv,output)})
  observe({watcher.sb_menu.modelling(rv,output)})
  observe({watcher.sb_menu.simulation(rv,output)})
#  observe({watcher.sb_menu.validation(rv,output)})
  
  # Sidebar controls update
  observe({sidebar_controls.tabchange(input,output)})
  
  # header notification
  #observe({watcher.h_alert_menu(rv, output)}, priority = 100)
  
  # Header task menu
  #observe({watcher.h_task_menu(rv,output)}, priority = 100)
  
  ##
  # Dashboard page
  ##
  
  # TAB 0 : SUMMARY
  
  observe({dash_update_vbox(rv,output)})
  observe({dash_update_summary(rv,output)})
  
  # TAB 1: 3d Visualization
  observe({ update_view_somaselector( rv, output, session ) })
  
  observe({ update_view_mesh_selector(rv, output, input) },priority = 4)
  observe({ update_cao_selector(rv, output, input) },priority = 3)
  observe({ update_cao_lower_build(rv, output, input) },priority = 2)
  observe({ update_cao_lower_setrv(rv, input) },priority = 1)
  observe({ unblock_render_button(rv, input, session)})
  observeEvent(input$dash_view_render,{render_somas(rv, session, input, output)})

  ##
  # Data loading
  ##
  
  ## TAB 0: DASH
  # Dashboard
  observe({dl_update_summary_vbox(rv,output)})
  observe({dl_update_summary_table(rv, output)})
  
  # TAB 1: LOAD
  # File upload
  observeEvent(input$dl_file, {dl_update_button(session)}) # Enables button when files are uploaded
  observeEvent(input$dl_upstart, {manage_file_upload(rv, input, output, session )})
  
  #  TAB 2: Preprocess
  # Update selectors
  observe( { update_dl_preproc_selector(rv , output, session ) })
  
  # Unlock button
  observe({ update_dl_preproc_button(session, input)})
  
  # Run processes
  observeEvent(input$dl_pproc_run, {run_dl_preproc_processes(rv,session,input,output) } )
  
  # TAB 3: Characterization
  
  # Update characterization
  observe({dl_table_update_df(rv, input, output,session )})
  
  # Export button
  observe({ dl_table_export(rv,output) })
  
  # Import button
  observe({ dl_table_importButtonEnabler(input,session) })
  
  # Import action
  observeEvent(input$dl_table_load,{ dl_table_importCSV( rv, input, output, session ) })
  
  # TAB 4: Download
  
  observe({ update_dl_download_selector( rv , output, session ) })
  observe({dl_download_create( rv, input, output, session )})
  
  ###
  # Modelling
  ###
  
  # Tab0 : DAsh
  observe({ model_dash_update(rv, output) })
  
  # TAB 1: NEW MODEL
  observe({model_new_selector_update(rv , output, session )}) # File selector
  observe({model_new_button_enable(input,session)}) # Run button enabler
  observeEvent(input$mod_new_run,{model_new_createModel(rv, input, output, session)}) # Create model
  
  # TAB 2: VIEW MODEL
  observe({update_model_view_selector(rv, output,session )})
  observeEvent(input$mod_view_sel,{update_model_view_general(rv, input, output, session )})
  # BN VIEW
  observe({update_model_view_bnmodel( rv, session, input, output)})
  observeEvent(input$mod_view_sel,{mod_view_modeldown(rv, input, output )})
  # Modal
  observeEvent(input$mod_view_bnmodal_cluster, {listener_bnmodal_clustersel(rv, input, output, session )})
  observe({mod_view_bnmodal_control(rv, input, output, session)})
  observe({modview_update_nodevals(rv, input, output, session)})
  observe({modview_update_bnmodal_mu(rv, input, output, session)})

  # SOURCE
  observe({mod_view_updatesourcedt(rv, input, output )})
  observe({mod_view_sourcedown( rv,input, output )})
  # CLUSTER
  observe({update_model_view_cluster(rv, session, input, output)})
  
  ####
  # Simulation
  ####
  
  # Tab0 dash
  observe({sim_dash_dt_update(rv,output)})
  
  # Tab 1 NEW
  observe({ update_sim_new_model_selector(rv,output,session)})
  observe({update_sim_new_cluster_selector(rv, input, output,session)})
  observe({update_sim_new_button(rv, input, session)})
  observeEvent(input$sim_new_run,{run_sim_new_process(rv, input, output, session )})
})
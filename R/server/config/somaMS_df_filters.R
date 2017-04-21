#' Dataframe filters

dashboard_df_filters <- function(){
  
  # Empty
  filters <- list()
  filters$selectors <- NULL
  
  
  filters$editors <- list(
                            # Logical data substitution
                            list(name="logic-to-icon", func = df_logical_to_icon_editor, params = NULL)
                          )
  
  
  return(filters)
}

characterization_df_filters <- function(){
  
  # Empty
  filters <- list()
  filters$selectors <- NULL
  
  
  filters$editors <- list(
    # Logical data substitution
    list(name="from-to-icon", func = df_from_to_icon_editor, params = list(colname = "from"))
  )
  
  
  return(filters)
}
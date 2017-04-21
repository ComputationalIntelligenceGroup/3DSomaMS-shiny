#' Editors
#'
#'

#' Change logical columns for icons
df_logical_to_icon_editor <- function(df , params  = NULL){
  # Change each logical column for a icon
  as.data.frame(lapply(df, function(x){
    if(is.logical(x)){
      return ( ifelse(x, toString( icon("check", class = "big-green-check")) , 
                         toString( icon("close", class = "big-red-close")) ) ) 
    }
    else
      return(x)
  }),stringsAsFactors = F)
}


df_from_to_icon_editor <- function(df, params = list(colname = "From")){
  
  # Change only from
  if( params$colname %in% names(df) ){
    df[,params$colname] <- sapply(df[,params$colname], function(x){
      
      # One or more names
      totalNames <- strsplit(x,",")[[1]]
      return(paste(sapply(totalNames, function(y){
        iconName <- switch(y,
                           raw = "file-archive-o",
                           rep = "wrench",
                           repaired = "wrench",
                           seg = "cut",
                           segmented = "cut",
                           rep_and_seg = "thumbs-o-up",
                           sim = "gears",
                           "question"
                           )
        
        alttext <- switch(y,
                           raw = "Unprocessed",
                           rep = "Repaired",
                           repaired = "Repaired",
                           seg = "Segmented",
                           segmented = "Segmented",
                           rep_and_seg = "Repaired and segmented",
                           sim = "Simulation",
                           x)
      
      
      
        return( toString( tags$i( class = sprintf("fa fa-%s somams-from-icon",iconName), alt = alttext, title = alttext) ) )
      }),collapse=""))
    })
  }
  
  return(df)
}
##
## Data frame filters
##
df_applyfilters <- function (df, filters){
  if( is.null(filters) || (nrow(df)==0) )
    return (df)
  else{
    df <- df_apply_rowselectors(df,filters$row_selectors)
    df <- df_apply_editors(df,filters$editors)
    return(df)
  }
}

df_apply_rowselectors <- function(df, selectors){
  if( length(selectors)==0 )
    return (df)
  else{
    res <- lapply(selectors,function(x){x$func(df,x$params)})
    selection <- Reduce(function(x,y){unlist(x) & unlist(y)},res)
    return (df[selection,])
  }
}

df_apply_editors <- function( df, editors ){
  if( length(editors)==0 )
    return (df)
  else{
    # Apply each editor
    for( i in 1:length(editors)){
      df <- editors[[i]]$func(df,editors[[i]]$params)
    }
    return(df)
  }
}


## Pretty colnames
prettyColnames <- function (names){
  new_names <- sapply(names, function(x){
    s <- gsub("_"," ",x,fixed = T)
    s <- strsplit(s," ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  })
  return(new_names)
}
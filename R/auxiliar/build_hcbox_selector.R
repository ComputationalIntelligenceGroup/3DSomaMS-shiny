#' Builds hcbox_items from given summary df
#'
build_hcbox_selector <- function( data, filters, 
                                  byPackage = T , byStatus = T, 
                                  includePkgName = T, removeEmpty = F, packageWithStatus = T, withSimulations = T ){
  
  # First step: apply filters
  data.filtered <- df_applyfilters(data,filters)
  
  # Build package boxes
  if(byPackage)
    pkg_item <- hcbox_item("pkg_selector",  
                                  clist = build_hcbox_selector_package(data.filtered, 
                                                                       removeEmpty = removeEmpty, 
                                                                       packageWithStatus = packageWithStatus,
                                                                       withSimulations = withSimulations),
                                  label = "Packages" )
  else
    pkg_item <- NULL
  
  # Build status boxes
  if(byStatus)
    stat_item <- hcbox_item("stat_selector",  
                                   clist = build_hcbox_selector_status(data.filtered, "root", 
                                                                       includePkgName = includePkgName, 
                                                                       removeEmpty = removeEmpty,
                                                                       withSimulations = withSimulations),
                                   label = "By Status" )
  else
    stat_item <- NULL
  
  ## Create list and return
  return ( list(pkg_item,stat_item) )
}


build_hcbox_selector_status <- function( data, pkgtag ,includePkgName = T, removeEmpty = F, withSimulations = T){
  
  # Create boxes for each possible status
  
  # Raw
  rawBox <- build_hcbox_selector_box( paste("raw", pkgtag, sep=":") , "raw", label="Unprocessed",
                                      data[ is.na(data$simulation_id) & !data$deleted,  ], 
                                      includePkgName = includePkgName, removeEmpty = removeEmpty )
  
  # Repaired
  repBox <- build_hcbox_selector_box( paste("repaired", pkgtag, sep=":")  , "repaired", label="Repaired",
                                      data[ is.na(data$simulation_id) & !data$deleted & data$repaired,  ], 
                                      includePkgName = includePkgName, removeEmpty = removeEmpty )
  
  # Segmented
  segBox <- build_hcbox_selector_box( paste("segmented", pkgtag, sep=":"), "segmented", label="Segmented",
                                      data[ is.na(data$simulation_id) & !data$deleted & data$segmented,  ], 
                                      includePkgName = includePkgName, removeEmpty = removeEmpty )
  
  # Both
  rasBox <- build_hcbox_selector_box( paste("rep_and_seg", pkgtag, sep=":") , "rep_and_seg", label="Repaired and segmented",
                                      data[ is.na(data$simulation_id) & !data$deleted & data$rep_and_seg,  ], 
                                      includePkgName = includePkgName, removeEmpty = removeEmpty )
  
  # sims
  if(withSimulations)
    simBox <- build_hcbox_selector_box( paste("simulation", pkgtag, sep=":") , "sim", label = "Simulations",
                                      data[ !is.na(data$simulation_id) & !data$deleted,  ], 
                                      includePkgName = includePkgName, removeEmpty = removeEmpty )
  else
    simBox <- NULL
  
  return( list( rawBox, repBox, segBox, rasBox, simBox ) )
}

build_hcbox_selector_package <- function( data, removeEmpty = F, withSimulations  = T, packageWithStatus = T ){
  
  #Create a box per package
  nonsim <- lapply( unique(data$package[ !data$deleted & is.na(data$simulation_id)] ), function(x, remove, data){
    
    if(is.na(x)){
      # Select only files no package
      data.pkg <- data[ is.na(data$package) & !data$deleted, ]
      boxname <- "No package"
      tag <- "nopackage"
    }
    else{
      
      # Select only package files
      data.pkg <- data[ data$package == x & !data$deleted , ]
      boxname <- x
      tag <- x
    }
    
    # Create box
    if( nrow(data.pkg) == 0 )
      if( removeEmpty )
        return(NULL)
    else
      return ( hcbox_item(boxname, disabled = T) )
    else{
      if(packageWithStatus)
        return ( hcbox_item(boxname, clist = build_hcbox_selector_status(data.pkg, tag, 
                                                                                includePkgName = F, removeEmpty = remove, withSimulations = withSimulations )) )
      else
        return ( build_hcbox_selector_box(boxname, tag, data.pkg, includePkgName = F, removeEmpty = remove ))
    }
  }, removeEmpty, data)
  
  # Now, for sims use simulation_id as package
  if(withSimulations){
    # Create boxes for each simulation
    sims <- lapply( unique( data$simulation_id[ !is.na(data$simulation_id) & !data$deleted ] ), function(x, remove, data){
      
      data.sim_id <- data[ data$simulation_id == x & !is.na(data$simulation_id) & !data$deleted, ]
      
      # Create box
      build_hcbox_selector_box(x, "sim", data.sim_id, includePkgName = F, removeEmpty = remove)
      
    },removeEmpty, data)
    
    # check for no simulations
    if(length(sims) == 0){
      if(removeEmpty)
        simbox <- NULL
      else
        simbox <- hcbox_item("Simulations", disabled = T)
    }
    else{
      simbox <- hcbox_item("Simulations", clist = sims)
    }
  }
  else
    simbox <- NULL
  
  return( c( nonsim, list(simbox) ) )
}

build_hcbox_selector_box <- function ( boxid, tag, data, label = boxid, includePkgName = T, removeEmpty = F ){
  
  # Check if box is empty
  if( is.null(data) || nrow(data) == 0 ){
    if(removeEmpty)
      return(NULL)
    else
      return(hcbox_item(boxid, label = label, disabled = T)) # Create the box, but disable it
  }
  else{
    
    # Use is as list instead of df
    data <- unlist(apply(data[ , c("name","package") ], 1, list), recursive = FALSE)
    # Remove "row" names
    names(data) <- NULL
    data <- lapply(data, as.list)
    
    # Create children list
    cdn <- lapply(data, function(x, includePkg){
      if(includePkg && !is.na(x$package) )
          return( hcbox_item(paste(x$name,tag,sep=":"), label = sprintf("[%s] %s", x$package, x$name ) ) )
      else
        return( hcbox_item(paste(x$name,tag,sep=":"), label = x$name ) )
    },includePkgName)
    
    # Create box
    return( hcbox_item(boxid, label = label, clist = cdn) )
    
  }
}

build_hcbox_selector_characterizations <- function( data, removeEmpty = F, withSimulations  = T, includePkgName = T ){
  
  #  Create summary.data table alike to build hc_boxes :D
  mock.df <- soma.create_summary_df()
  
  # Add new column : raw
  mock.df <- cbind( mock.df, raw=logical(0) )
  
  pkgtag <- "root"

  for( i in 1:nrow(data)){
  
    # initialize new row for this soma
    if( all(mock.df$name != data$name[i]) ){
      mock.df[ nrow(mock.df)+1,] <- c( data[i,c("name","package")],
                                       data$simulation_id[i],
                                               rep(F,7))
    }
    
    # Check "from" and get colname
    colname <- switch( tolower(data$from[i]), 
                       raw = "raw",
                       rep = "repaired",
                       repaired = "repaired",
                       seg = "segmented",
                       segmented = "segmented",
                       rep_and_seg = "rep_and_seg",
                       sim = NULL)
    
    if( !is.null(colname) ){
      mock.df[mock.df$name == data$name[i], colname ] <- T
    }
    
  }
    
    # Create boxes for each possible status
    
    # Raw
    rawBox <- build_hcbox_selector_box( paste("raw", pkgtag, sep=":") , "raw", label="Unprocessed",
                                        mock.df[ is.na(mock.df$simulation_id) & !mock.df$deleted  & mock.df$raw,  ], 
                                        includePkgName = includePkgName, removeEmpty = removeEmpty )
    
    # Repaired
    repBox <- build_hcbox_selector_box( paste("repaired", pkgtag, sep=":")  , "repaired", label="Repaired",
                                        mock.df[ is.na(mock.df$simulation_id) & !mock.df$deleted & mock.df$repaired,  ], 
                                        includePkgName = includePkgName, removeEmpty = removeEmpty )
    
    # Segmented
    segBox <- build_hcbox_selector_box( paste("segmented", pkgtag, sep=":"), "segmented", label="Segmented",
                                        mock.df[ is.na(mock.df$simulation_id) & !mock.df$deleted & mock.df$segmented,  ], 
                                        includePkgName = includePkgName, removeEmpty = removeEmpty )
    
    # Both
    rasBox <- build_hcbox_selector_box( paste("rep_and_seg", pkgtag, sep=":") , "rep_and_seg", label="Repaired and segmented",
                                        mock.df[ is.na(mock.df$simulation_id) & !mock.df$deleted & mock.df$rep_and_seg,  ], 
                                        includePkgName = includePkgName, removeEmpty = removeEmpty )
    
    # sims
    if(withSimulations)
      simBox <- build_hcbox_selector_box( paste("simulation", pkgtag, sep=":") , "sim", label = "Simulations",
                                          mock.df[ !is.na(mock.df$simulation_id) & !mock.df$deleted,  ], 
                                          includePkgName = includePkgName, removeEmpty = removeEmpty )
    else
      simBox <- NULL
    
    return ( list ( hcbox_item("stat_selector",  
                                   clist = list( rawBox, repBox, segBox, rasBox, simBox) ,
                                   label = "By Status" ) ) )
}
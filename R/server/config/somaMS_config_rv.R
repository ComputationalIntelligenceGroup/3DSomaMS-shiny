somaMS_config_rv <- function(){
  list(
    # Just some info?
    version = "0.0.1",
    
    # Number of parallel workers
    workers = 4,
    
    # Selector variables
    selectors.removeEmpty = F,
    selectors.packageName = T
  )
}
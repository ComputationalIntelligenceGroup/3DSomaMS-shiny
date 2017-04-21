#' Given the output returned by the file selector creates a list with soma name and type
extract_somalist_form_selector <- function( input ){
  
  # Check for null or empty values
  if(length(input) == 0) return (NULL)
  
  ## For each input value
  return (lapply(input, function(x){
    s <- strsplit( x , ":", fixed  = T )[[1]]
    # Get tag and name
    tag <- s[length(s)]
    name <- paste0(s[- length(s)], collapse = ":")
    # create list
    list(type = tag, name = name)
  }))
}
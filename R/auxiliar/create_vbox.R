#' vbox auxiliar
create_vbox <- function(output,id,value, title, icon = NULL, color = "aqua", width = 4, href = NULL){
  output[[id]] <- renderValueBox( valueBox(value, title, icon = icon, color = color, width = width, href = href)  )
}

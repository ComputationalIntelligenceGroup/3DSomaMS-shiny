
task_menu_rv <- function (){
  return (list( 
    msgs = list(),
    status = "primary",
    icon = NULL
  ))
}

watcher.h_task_menu <- function(rv, output){
  
  # Get msgs
  msgs <- rv$task_menu$msgs
  status <- rv$task_menu$status
  icon <- rv$task_menu$icon
  
  # Create notification menu
  create_task_menu("h_taskmenu",output,msgs,status,icon)
}

h_task_menu.add <- function(rv, id, text = id, color = "green", value = 0 ){
  rv$task_menu$msgs[[id]] <- list(text = text , color = color, value = value)
}

h_task_menu.remove <- function (rv, id ){ 
  rv$task_menu$msgs[[id]] <- NULL
}

create_task_menu <- function(id, output, msgs, status, icon){
  output[[id]] <- renderMenu(dropdownMenu(type = "task",
                                          badgeStatus = status, icon = icon,
                                          .list = lapply(msgs, create_single_task)))
}

create_single_task <- function(msg){
  taskItem(value = msg$value, color = msg$color, msg$text)
}

alert_menu_rv <- function ()
  
  return (list( 
    msgs = list(),
    status = "primary",
    icon = NULL
))

watcher.h_alert_menu <- function(rv, output){
  
  # Get msgs
    msgs <- rv$alert_menu$msgs
    status <- rv$alert_menu$status
    icon <- rv$alert_menu$icon
    
    # Create notification menu
    create_notification_menu("h_alertmenu",output,msgs,status,icon)
}

h_alert_menu.add <- function(rv, id, text = id, icon = shiny::icon("warning"), status = "success", href = NULL){
  rv$alert_menu$msgs[[id]] <- list(text = text , icon = icon, status = status, href = href)
}

h_alert_menu.remove <- function (rv, id ){ 
  rv$alert_menu$msgs[[id]] <- NULL
}

create_notification_menu <- function(id, output, msgs, status, icon){
  output[[id]] <- renderMenu(dropdownMenu(type = "notifications",
                               badgeStatus = status, icon = icon,
                               .list = lapply(msgs, create_single_notification)))
}

create_single_notification <- function(msg){
  notificationItem(text = msg$text, icon = msg$icon, status = msg$status, href = msg$href)
}
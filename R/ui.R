
# Shiny UI 
shinyUI(
  
  dashboardPage(
    ##
    # HEADER
    ##
    dashboardHeader(
      
      ## Header title on the left
      title = tagList("3D SomaMS",tags$span("(beta)", class="subscript-header")),
      
      # build header
      .list = somaMS_build_header()

    ),
    
    ##
    # SIDEBAR
    ##
    dashboardSidebar(
      
      somaMS_build_sidebar()
      
    ),
    
    ##
    # BODY
    ##
    dashboardBody(
      somaMS_build_body(),
      includeCSS("inst/somaMS.css"),
      tags$script(src="fancybox/jquery.fancybox.pack.js"),
      tags$link(rel="stylesheet", type="text/css", href="fancybox/jquery.fancybox.css"),
      tags$script( "$(document).ready(function() {
                    $(\".fancybox\").fancybox();
                     });")
    ),
    title = "SomaMS",
    skin = "green"
  )
)

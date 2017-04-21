#' Renders a neuron in the given output
#'
#'
#'@export
renderMeshes3D <- function(data , quality=c("High","Medium","Low")) {
  

  quality <- match.arg(quality)
  
  # List of lists with five elements:
  # 
  # * vertices - list of nodes, each one with the following attributes:
  #
  #     * x
  #     * y
  #     * z
  #
  # * faces - list of faces (each one build from three nodes)
  #
  #     * v1 id
  #     * v2 id
  #     * v3 id
  # 
  # * color - RGB mesh color
  # * alpha - [0 - 1] mesh alpha value
  # * name - Mesh id

    
    
  if(!is.null(data)){
      
      # Replace ids in faces (index starts at 0)
      for( i in 1:length(data)){
        
        ## Quality
        if(quality == "Medium" || quality == "Low"){
          
          # Apply decimate
          vcgMesh <- tmesh3d( t(cbind(data[[i]]$vertices,1)), t(data[[i]]$faces) )
          vcgMesh <- vcgQEdecim( vcgMesh, percent = switch (quality, Medium=0.5, Low=0.25) )
          
          # Apply to data
          data[[i]]$vertices <- t(vcgMesh$vb[1:3,])
          data[[i]]$faces <- t(vcgMesh$it)
        }
        
        data[[i]]$faces  <-  data[[i]]$faces -1
      }
    
      # Create mesh for each data
      objs <- lapply(data, function(x){
        return( objThreeJS( meshGeometry(x$vertices, x$faces, compute.fn = T), lambertMaterial(color = x$color, opacity = x$alpha), name = x$name ) )
      })
      
      ## Create ambiental lights
      ligths <- list( directionalLight("#FFFFFF",intensity=0.33,from = c(1,0,1)) ,
                      directionalLight("#FFFFFF",intensity=0.33,from = c(0,1,1)) ,
                      directionalLight("#FFFFFF",intensity=0.33,from = c(1,1,0)) ) 
    
      # Retrun value for JSON (list of meshes)
      return ( scene( objects = objs , lights = ligths, bgColor = "#FFFFFF",     
                            camera.pos = c(10.5, 88, 77),
                            camera.lookat = c(85, 52, -10))  ) 
    }
    return (NULL)
    
}

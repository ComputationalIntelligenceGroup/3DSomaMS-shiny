#' Output XML files manager
#'
#' Functions to create / read XML files as output/input files for somaMS GUI
#'
#' @author Luis Rodríguez-Luján


#' createMetaNode
#' 
#' Auxiliar function to create XML metadata nodes
#' 
#' @param tree metadata tree
#' @param value Value to put in tag
#' @param name Node name
#' 
#' @return XML Node
createMetaNode <- function(tree,value,name){
  
  if ( !is.null(value) )
    return( XML::newXMLNode(name,value,parent = tree))
  else
    return(NULL)
}


#' createParamNode
#' 
#' Create XML parameter node
#' 
#' @param parent parent node
#' @param name parameter name
#' @param type parameter type
#' @param value parameter value
#' @param desc parameter description
#' 
#' @return XML node
createParamNode <- function(parent, name, type = c("Float","Integer","Boolean","Text","Point3D","Other"), value = NULL , desc = NULL ){
  
  type <- match.arg(type);
  
  return( XML::newXMLNode("Parameter", value, parent = parent,
                          attrs = c( type = type,
                          description = desc) ))
}

#' Create meshlab method node
#' 
#' @param parent parent node
#' @param method method name
#' @param ... method parameters
#' 
#' @return XML node
createMeshlabMethod <- function(parent, method, parameters ){
  
  # Create base node
  base <- XML::newXMLNode("MeshlabMethod", attrs = c(methodName = method), parent = parent )
  
  # Add all parameters (named list with type, value and desc )
  for (param in names(parameters)) {
    # Create parameters
    createParamNode( base, param, type = parameters[[param]]$type, value = parameters[[param]]$value, desc = parameters[[param]]$desc )
  }
  
  # Return method
  return( base )
}

#' Create ambient occlusion meshlab script node
#' 
#' @param parent parent node
#' @param dirBias
#' @param reqViews
#' @param coneDir
#' @param coneAngle
#' @param useGPU
#' @param useVBO
#' @param depthTextSize
#' 
#' @return XML node
createAONode <- function(parent, 
                         dirBias = 0,
                         reqViews = 128,
                         coneDir = c(0,1,0),
                         coneAngle = 30,
                         useGPU = F,
                         useVBO = F,
                         depthTexSize = 512){
  # Create params list
  params <- list(
    dirBias = list( value = dirBias, type =  "Float", desc = "The balance between a uniform and a directionally biased set of lighting direction"),
    reqViews = list( value = reqViews, type = "Integer", desc = "Number of different views uniformly placed around the mesh"),
    coneDir = list( value = coneDir, type = "Point3D", desc = "Cone Lighting Direction"),
    coneAngle = list( value = coneAngle, type = "Float", desc = "Cone amplitude"),
    useGPU = list( value = useGPU, type = "Boolean", desc = "Use GPU acceleration"),
    useVBO = list( value = useVBO, type = "Boolean", desc = "Use VBO if supported"),
    depthTexSize = list( value = depthTexSize, type = "Integer", desc = "Depth texture size(should be 2^n)")
  )
  
  # Return node
  createMeshlabMethod(parent,"AmbientOcclusion",params)
  
}

#' Create Poisson surface reconstruction node
#' 
#' @param parent parent node
#' @param octDepth
#' @param solverDivide
#' @param samplesPerNode 
#' @param offset
#'
#' @return XML node
createPSRNode <- function(parent,
                          octDepth = 11,
                          solverDivide = 6,
                          samplesPerNode = 1,
                          offset = 1){
  params <- list(
    octDepth = list( value = octDepth, type = "Integer", desc = "TBD"),
    solverDivide = list( value = solverDivide, type  = "Integer", desc = "TBD"),
    samplesPerNode = list( value = samplesPerNode, type = "Float", desc = "TBD"),
    offset = list( value = offset, type = "Float", desc = "TBD")
  )
  
  # Return node
  createMeshlabMethod(parent,"PoissonSurfaceReconstruction",params)
}

#' Create ComputeNormalSet node
#' 
#' @param parent parent node
#' @param K
#' @param flipFlag
#' @param viewPos
#'
#' @return XML node
createCNSNode <- function(parent,
                          K = 10,
                          flipFlag = F,
                          viewPos = c(0,0,0)){
  params <- list(
    K = list( value = K, type = "Integer", desc = "TBD"),
    flipFlag = list( value = flipFlag, type  = "Boolean", desc = "TBD"),
    viewPos = list( value = viewPos, type = "Point3D", desc = "TBD")
  )
  
  # Return node
  createMeshlabMethod(parent,"ComputeNormalSet",params)
}


#' Create ShapeDiameterFunction node
#' 
#' @param parent parent node
#' @param fromVertices
#' @param numberRays
#' @param peelingIteration
#' @param peelingTolerance
#' @param coneAngle
#' @param useVBO
#' @param removeFalse
#' @param removeOutliers
#'
#' @return XML node
createSDFNode <- function(parent,
                          fromVertices = T,
                          numberRays = 32,
                          depthTextureSize = 512,
                          peelingIteration = 10,
                          peelingTolerance = 1E-7,
                          coneAngle = 20,
                          useVBO = F,
                          removeFalse = T,
                          removeOutliers = F
                          ){
  params <- list(
    fromVertices = list( value = fromVertices, type = "Boolean", desc = "trace rays from faces or from vertices"),
    numberRays = list( value = numberRays, type = "Integer", desc = "The number of rays that will be casted around the normals"),
    depthTextureSize = list( value = depthTextureSize, type = "Integer", desc = "Size of the depth texture for depth peeling"),
    peelingIteration = list( value = peelingIteration, type = "Integer", desc = "Number of depth peeling iteration"),
    peelingTolerance = list( value = peelingTolerance, type = "Float", desc =  "Depth tolerance used during depth peeling"),
    coneAngle = list( value = coneAngle, type = "Float", desc = "Cone amplitude around normals in degrees"),
    useVBO = list( value = useVBO, type = "Boolean", desc = "Use VBO if supported"),
    removeFalse = list( value = removeFalse, type = "Boolean", desc = "Remove false intersections"),
    removeOutliers = list( value = removeOutliers, type = "Boolean", desc = "Remove outliers")
  )
  
  # Return node
  createMeshlabMethod(parent,"ShapeDiameterFunction",params)
}

#' createFileNode
#'
#' Auxiliar function to create a file "element" of the list of inputs/outputs
#' 
#' @param type type of IO node (file or manual so far)
#' @param filename file name (only for type file)
#' @param extension Despite of the name, this is actually file mimetype
#' @param description Description of the file contents
#' @param parent Parent XML node
#' @param sample Sample  ID number
#' @param layer Sample layer
#'
#'@return XML IO node
#'
createIONode <- function(parent , type=c("file", "manual", "other"), filename=NULL,
                         extension = NULL, description = NULL,
                         ... ){
  node <- switch(type,
                 file = XML::newXMLNode("file",attrs = c( filename = filename,
                                                          extension = extension,
                                                          description = description,
                                                          ... ), parent = parent),
                 
                 manual = XML::newXMLNode("manual",parent = parent, attrs = list(...) ),
                 other = XML::newXMLNode("other", parent = parent, attrs = list(...) )
  )
  return(node)
}

#' Create Repair method node
#' 
#' TODO: Support AO/PSR params
createRepairNode <- function(parent){
  
  base <- XML::newXMLNode("Repair", parent = parent )
  
  # Create AO node
  createAONode( base )
  
  # Create SDF node
  createPSRNode( base )
  
  return( base )
}

#' Create Segmetation method node
#' 
#' TODO: Support SDF/PSR params
createSegmentationNode <- function(parent){
  
  base <- XML::newXMLNode("Segmentation", parent = parent )
  
  # Create AO node
  createSDFNode( base )
  
  # Create NORMA node
  createCNSNode( base )
  
  # Create SDF node
  createPSRNode( base )
  
  return( base )
}

createSomaNode <- function(parent, name, file, repaired = F , segmented = F, package = NULL ){
  
  # Create base node
  base <- XML::newXMLNode("Soma", parent = parent, attrs = c(name = name, package = package) )
  
  # Add file
  createIONode(base, type = "file",filename = file, extension = tools::file_ext(file) )
  
  # Add preprocs
  if (repaired)
    createRepairNode(base)
  
  if (segmented)
    createSegmentationNode(base)
  
  return(base)
}

createSimsomaNode <- function(parent, name, file, simulationId){
  
  # Create base node
  base <- XML::newXMLNode("Soma", parent = parent, attrs = c(name = name, simulation = simulationId) )
  
  # Add file
  createIONode(base, type = "file",filename = file, extension = "ply" )
  
  return(base)
}
  
#' Dump bayesian network structure as a XML structure
#' 
createBNStructureNode <- function(parent, bn){
  
  # Create base node
  base <- XML::newXMLNode("BNStructure", parent = parent)
  
  # Add test and algorithm
  XML::newXMLNode("Algorithm", bn$learning$algo  ,parent = base )
  XML::newXMLNode("Test", bn$learning$test  ,parent = base )
  
  # Add nodes
  variables <- XML::newXMLNode("Variables", parent = base)
  for (v in names(bn$nodes))
    XML::newXMLNode("Var", attrs = c(name = v, type = "Continuous-Gaussian"), parent = variables )
  
  # Add arcs
  arcs <- XML::newXMLNode("Arcs", parent = base)
  for (i in 1:nrow(bn$arcs ))
    XML::newXMLNode("Arc", attrs = c(bn$arcs[i,1], bn$arcs[i,2]), parent = arcs )
  
  return(base)
}

#' Dump cluster info to a node
#' 
createClusterNode <- function(parent, clusters){
  
  # Create base node
  base <- XML::newXMLNode( "Clusters", parent = parent, attrs = c(nclusters = length(clusters$priori) , ninstances = nrow(clusters$weights) ) )
  
  # Add instances (wheights)
  weights <- XML::newXMLNode( "Weights", parent = base)
  
  for (i in 1:nrow(clusters$weights))
    XML::newXMLNode("Instance", attrs = c(probabilities = paste0(clusters$weights[i,],collapse = ";" )), parent = weights)
  
  return(base)
}

#' Create model node
createModelNode <- function(parent, name, data, file, model,
                            nboots = 200,
                            sigthreshold = 0.95,
                            nclust = c(2,3),
                            initmethod = "kmeans" ){
  
  # Create base node
  base <- XML::newXMLNode("Model", parent = parent, attrs = c(name = name) )
  
  # Create learning parameters
  params <- XML::newXMLNode("LearningParameters", parent = base) 
  createParamNode( params, "nboots", type = "Integer", value = nboots, desc = "Number of reboots" )
  createParamNode( params, "sigthreshold", type = "Float", value = sigthreshold, desc = "Arc significance threshold" )
  createParamNode( params, "nclust", type = "Other", value = paste0(nclust,collapse = ";"), desc = "Number of cluster range" )
  createParamNode( params, "initmethod", type = "Text", value = initmethod, desc = "Initialization method" )
  
  # Create Source data
  sourceData <- XML::newXMLNode("SourceData", parent = base, attrs = c(ninst = nrow(data), nvars = ncol(data) )) 
  
  # Add file
  createIONode(sourceData, type = "file", filename = file, extension = "CSV", description = "Model source data")
  
  # Add BN structure
  createBNStructureNode( base, model$bn )
  
  # Add cluster
  createClusterNode(base, model$cluster )
  
  return( base )
}

#' Create simulation node
#' 
createSimulationNode <- function(parent, name, model, ncluster, nsims){
  
  # Create base node
  base <- XML::newXMLNode("Simulation", parent = parent, attrs = c(name = name, model = model) )
  
  # Create learning parameters
  params <- XML::newXMLNode("Parameters", parent = base) 
  createParamNode( params, "ncluster", type = "Integer", value = ncluster, desc = "Cluster selection" )
  createParamNode( params, "nsims", type = "Integer", value = nsims, desc = "Number of simulations" )
  
  return(base)
}

#' somaMSMetaHeader
#' 
#' Creates an XML with a common metadata header
#' 
#' @param application Application name
#' @param version Application version
#' 
#' @return XML tree (metaHeader)
#' 
#' @export
somaMSMetaheader <- function(application="SomaMS",
                              version=packageVersion("SomaMS")
                            ){
  
  parent <- XML::newXMLNode("somaMS" )
  
  # Create root metadata tree
  header <- XML::newXMLNode("Header", parent = parent )
  
  # Create application node
  createMetaNode(header,application,"application")
  
  # Create version node
  createMetaNode(header,version,"version")
  
  return(parent)
}

createPackageNode <- function(parent, name, sim = F){
  if (!sim)
    return( XML::newXMLNode("Package", parent = parent, attrs = c(name = name) ))
  else
    return( XML::newXMLNode("Simulations", parent = parent, attrs = c(name = name) ))
}

writeSomaMSMetadata <- function(root, outputFile ){
  doc <- XML::newXMLDoc( root )
  XML::saveXML(doc, file = outputFile )
}
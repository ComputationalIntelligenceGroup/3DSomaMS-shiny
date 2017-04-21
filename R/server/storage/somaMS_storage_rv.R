
files_rv <- function(){
  list(
    real = list(),
    simulated = list()
  )
}

soma_rv <- function(){
  list(
    summary.data = soma.create_summary_df(),
    summary.data.filter = dashboard_df_filters(),
    characterization = soma.create_characterization_df(),
    characterization.filter = characterization_df_filters(),
    model = list(),
    simulation = list()
  )
}

soma.create_summary_df <- function(){
  data.frame( 
    name = character(0),
    package = character(0),
    simulation_id = character(0), 
    repaired = logical(0),
    segmented = logical(0),
    rep_and_seg = logical(0),
    characterized = logical(0),
    char_uptodate =logical(0),
    deleted =logical(0),
    stringsAsFactors = F)
}

soma.create_characterization_df <- function(){

  data.frame( 
    name = character(0),
    from = character(0),
    package = character(0),
    simulation_id = character(0),
    deleted = logical(0),
    height1 = numeric(0),
    height2 = numeric(0),
    height3 = numeric(0),
    height4 = numeric(0),
    height5 = numeric(0),
    height6 = numeric(0),
    height7 = numeric(0),
    phi1 = numeric(0),
    phi2 = numeric(0),
    phi3 = numeric(0),
    phi4 = numeric(0),
    phi5 = numeric(0),
    phi6 = numeric(0),
    theta1 = numeric(0),
    theta2 = numeric(0),
    theta3 = numeric(0),
    theta4 = numeric(0),
    theta5 = numeric(0),
    theta6 = numeric(0),
    r_h1 = numeric(0),
    r_h2 = numeric(0),
    r_h3 = numeric(0),
    r_h4 = numeric(0),
    r_h5 = numeric(0),
    r_h6 = numeric(0),
    e1 = numeric(0),
    e2 = numeric(0),
    e3 = numeric(0),
    e4 = numeric(0),
    e5 = numeric(0),
    e6 = numeric(0),
    PCA_phi1 = numeric(0),
    PCA_phi2 = numeric(0),
    PCA_phi3 = numeric(0),
    PCA_phi4 = numeric(0),
    PCA_phi5 = numeric(0),
    PCA_phi6 = numeric(0),
    PCA_theta1 = numeric(0),
    PCA_theta2 = numeric(0),
    PCA_theta3 = numeric(0),
    PCA_theta4 = numeric(0),
    PCA_theta5 = numeric(0),
    PCA_theta6 = numeric(0),
    w1 = numeric(0),
    w2 = numeric(0),
    w3 = numeric(0),
    w4 = numeric(0),
    w5 = numeric(0),
    w6 = numeric(0),
    b_01 = numeric(0),
    b_02 = numeric(0),
    b_03 = numeric(0),
    b_04 = numeric(0),
    b_05 = numeric(0),
    b_06 = numeric(0),
    b_11 = numeric(0),
    b_12 = numeric(0),
    b_13 = numeric(0),
    b_14 = numeric(0),
    b_15 = numeric(0),
    b_16 = numeric(0),
    b_21 = numeric(0),
    b_22 = numeric(0),
    b_23 = numeric(0),
    b_24 = numeric(0),
    b_25 = numeric(0),
    b_26 = numeric(0),
    stringsAsFactors = F)
}
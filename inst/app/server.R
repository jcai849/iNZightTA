server <- function(input, output, session){

  # Loop through all .R files in the subfolder "/R" and load them
  
  for (r_file in list.files(path = "R", pattern = "\\.R$")) { 
    source(paste0("R/", r_file), local = TRUE)$value 
  }
  
}

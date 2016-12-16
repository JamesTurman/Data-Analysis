library(data.table)

# load db connector if not there
if(!exists("flight.db")){
  source("src/connectDB.R")
}

#list all files in directory
files <- list.files(path = "data", full.names = TRUE)

for(i in 1:length(files)){
  sprintf("Processing file %s", files[i])
  
  # Unzip file into data folder
  unzip(zipfile = files[i], exdir = "data")
  
  # Read the unziped file
  fl <- fread(input = gsub(".zip", replacement = ".csv", x = files[i]), 
              header = TRUE, stringsAsFactors=FALSE, select = c(1:64))
  
  # Write the results into the database
  dbWriteTable(flight.db, name = "connections", fl, append = TRUE)
  
  # Clean up the extracted files
  file.remove(c(gsub(".zip", replacement = ".csv", x = files[i]), "data/readme.html"))
}


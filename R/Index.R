# Index.R

# -------------------------------------
# Directory Setup
# -------------------------------------

# Try to create folders if they don't already exist
dir.create(file.path("dist"), showWarnings = FALSE)        
dir.create(file.path("dist/img"), showWarnings = FALSE)
dir.create(file.path("dist/img/pdf"), showWarnings = FALSE)
dir.create(file.path("dist/img/png"), showWarnings = FALSE)

# -------------------------------------
# Source Scripts
# -------------------------------------

# Manifest and Data Extraction, Transformation, Loading (ETL)
source("Manifest.R")
source("00-ETL.R")

# Uncomment the line below when data imputation is necessary
#source("data-imputation.R")

# Spatial ETL
source("01-spatial-ETL.R")

# Data Visualization
source("02-plots.R")
source("03-maps.R")
source("04-bubble-faceted-maps.R")
source("04-changes-in-atm-validations-per-stop-and-month-faceted-map.R")

# Main Map Creation
source("05-interurban-bivariate-sparklines-map.R")
source("06-urban-bivariate-sparklines-map.R")


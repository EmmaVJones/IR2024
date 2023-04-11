
# To pull any pinned data, you need to:
#  - Read the pinned data article in the DEQ Methods Encyclopedia: https://rconnect.deq.virginia.gov/MethodsEncyclopedia/vnbellzNy/connectToConnectPins.html#connectToConnectPins
#  - Install the appropriate version of the pins and filelock packages to connect to the older version of pins on the R server
#  - Email Emma Jones (emma.jones@deq.virginia.gov) to acquire the necessary API key to access pinned data on the R server
#  - Run the scrip below

# Load necessary packages
library(tidyverse)
library(pins)
library(config)


# Bring in a secure API key into R environment
# Email Emma Jones to acquire the PINSconfig.yml file
conn <- config::get(file = "PINSconfig.yml", "connectionSettings") # get configuration settings

# Use API Key to Register RStudio Connect
board_register_rsconnect(key = conn$CONNECT_API_KEY, 
                         server = conn$CONNECT_SERVER)

# Retrieve Pin
citmon2024 <- pin_get("ejones/citmonNonAgencyDataIR2024", board = "rsconnect")

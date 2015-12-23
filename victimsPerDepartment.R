# This doesn't actually do what I want, but it's still useful code. Maybe?
library(readxl)
library(dplyr)
library(tidyr)

mpvData <- read_excel("MPVDatasetDownload-83zj.xlsx", sheet = 1)

# Pitch bad columns, which are empty and raise errors in dplyr
mpvData <- mpvData[, 1:19]

mpvData <- mpvData %>%
  # Only data from 2015 and agency must be specified
  filter(`Date of injury resulting in death (month/day/year)` >=
           as.POSIXct("2015-01-01"),
         !is.na(`Agency responsible for death`)) %>%
  # Split up the agencies
  mutate(agency = sub(",? and ", ", ", `Agency responsible for death`)) %>%
  separate(agency, paste("agency", 1:4, sep="_"), 
           "[[:space:]]*,[[:space:]]*", fill = "right") %>%
  # Gather back into a single column
  gather("agency_number", "agency_name", starts_with("agency_"), na.rm  = TRUE)

# Count the number of victims per department
victimsPerDepartment <- mpvData %>%
  count(agency_name)

setwd("/Users/dspaude/code/b-arbeit/gepris-crawls/DQ-Notebook-with-crawl-from_final_2018-10-28--11-07-37-EN/stage2")

if (!require("tidyr")) install.packages("tidyr")
if (!require("dplyr")) install.packages("dplyr")

library(tidyr)
library(dplyr)


reshape_by_resource_type = function(generic_fields, resource_type) {
  return(
    generic_fields %>%
      filter(resource_type == resource_type) %>%
      select(-resource_type) %>%
      spread(field_name, field_value)
  )
}

generic_fields = read.csv("TMP_generic_field_extractions.csv")

colnames(generic_fields)

projects = reshape_by_resource_type(generic_fields, "project")

people = reshape_by_resource_type(generic_fields, "person")

institutions = reshape_by_resource_type(generic_fields, "institution")



# 
  
foo = generic_fields %>%
  spread(field_name, field_value)

generic_fields[1047013,]
generic_fields[1047014,]
generic_fields[1047015,]



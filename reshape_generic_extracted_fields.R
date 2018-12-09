setwd("/Users/dspaude/code/b-arbeit/gepris-crawls/DQ-Notebook-with-crawl-from_final_2018-10-28--11-07-37-EN/stage2")

if (!require("tidyr")) install.packages("tidyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")

library(tidyr)
library(dplyr)
library(stringr)


reshape_by_resource_type = function(generic_fields, resource_type_str) {
  return(
    generic_fields %>%
      filter(resource_type == resource_type_str) %>%
      select(-resource_type) %>%
      filter(!grepl("^ProjectRelation: .*$", field_name)) %>%
      spread(field_name, field_value)
  )
}




  
generic_fields = read.csv("generic_field_extractions.csv")

colnames(generic_fields)




# Define predicate for relation fields for resource 'projects'
# so where field-name is part of the following list or starts with the following pattern/regex
# distinct_field_names = generic_fields %>%
#   select(field_name) %>%
#   distinct %>%
#   arrange(field_name)
# as.character(distinct_field_names$field_name)

# Besides the 'ProjectRelation: .*' pattern: in general, I identified the following fields which needs 
# consideration for special handling (because they have relation character): 
# Applicant
# Applicants
# Applying institution
# Co-Applicant
# Co-Applicants
# Co-applicant
# Co-applicant institution
# Co-applicants
# Cooperation partner
# Cooperation partners
# Deputy spokespeople
# Deputy spokesperson
# Foreign institution
# Foreign spokespeople
# Foreign spokesperson
# Former applicant
# Former applicants
# Head
# Heads
# International Co-Applicant
# International Co-Applicants
# Participating Institution
# Participating Person
# Participating Persons
# Participating institution
# Participating scientist
# Participating scientists
# Participating subject areas
# Participating university
# Partner organisation
# Project leader
# Project leaders
# Spokesperson
# Spokespersons
# Subject Area


# Or other approach: filter out all rows where the field_value starts with "<a
# foo3 = generic_fields %>%
  # filter(grepl("^<a.*$", field_value))



# Desired table structure of project_relations: 
# project_id | reference_resource_type | reference_resource_id | 
#   relation_found_on (PROJECT or REFERENCE_RESOURCE) | relation_type ("Research Units", "As Spokesperson", ...)

project_relations_from_non_project_pages = generic_fields %>% 
  filter(resource_type != "project") %>%
  filter(grepl("^ProjectRelation: .*$", field_name)) %>%
  mutate(project_id = as.character(field_value)) %>%
  mutate(reference_resource_type = resource_type) %>%
  mutate(reference_resource_id = as.character(resource_id)) %>%
  mutate(relation_type = gsub("ProjectRelation: ", "", field_name)) %>%
  mutate(relation_found_on = "REFERENCE_RESOURCE") %>%
  select(project_id, reference_resource_type, reference_resource_id, relation_type, relation_found_on)



# Approach for the relations found on the project detail pages: 
# 1. 
# 2. 
project_relation_field_names = c(
  "Applicant", 
  "Applicants", 
  "Applying institution", 
  "Co-Applicant", 
  "Co-Applicants", 
  "Co-applicant", 
  "Co-applicant institution", 
  "Co-applicants", 
  "Cooperation partner", 
  "Cooperation partners", 
  "Deputy spokespeople", 
  "Deputy spokesperson", 
  "Foreign institution", 
  "Foreign spokespeople", 
  "Foreign spokesperson", 
  "Former applicant", 
  "Former applicants", 
  "Head", 
  "Heads", 
  "International Co-Applicant", 
  "International Co-Applicants", 
  "Participating Institution", 
  "Participating Person", 
  "Participating Persons", 
  "Participating institution", 
  "Participating scientist", 
  "Participating scientists", 
  "Participating university", 
  "Partner organisation", 
  "Project leader", 
  "Project leaders", 
  "Spokesperson", 
  "Spokespersons"
  )
# project_id | reference_resource_type | reference_resource_id | 
#   relation_found_on (PROJECT or REFERENCE_RESOURCE) | relation_type ("Research Units", "As Spokesperson", ...)
project_relations_from_project_pages = generic_fields %>% 
  filter(resource_type == "project") %>%
  filter(field_name %in% project_relation_field_names) %>%
  mutate(project_id = as.character(resource_id)) %>%
  # reference_resource_type und reference_resource_id aus field_value extracten
  mutate(reference_resource_type = str_match(field_value, '<a class="intern" href="/gepris/([a-z]*)/')[,2]) %>%
  mutate(reference_resource_id = str_match(field_value, '<a class="intern" href="/gepris/[a-z]*/(\\d*)')[,2]) %>%
  mutate(relation_type = field_name) %>%
  mutate(relation_found_on = "PROJECT") %>%
  select(project_id, reference_resource_type, reference_resource_id, relation_type, relation_found_on)


str(project_relations_from_project_pages)
# merge project_relations_from_project_pages and project_relations_from_non_project_pages
project_relations = project_relations_from_project_pages %>% 
  bind_rows(project_relations_from_non_project_pages)









# TODO: replace synonyms in project_relations_from_project_pages$relation_type (cluster synonyms (like c("Applicant", "Applicants")))

# Reduce(f = "-", x = 1:6, accumulate = T)

synonym_groups = c(
  c("Applicant", "Applicants", "As Applicant")
)

# I identified the following groups which should be 'normalised' to only one of their representations
# As Former applicant
# Former applicant
# Former applicants
# 
# As Cooperation partner
# Cooperation partner
# Cooperation partners
# 
# As Deputy spokesperson
# Deputy spokespeople
# Deputy spokesperson
# 
# As Foreign spokesperson
# Foreign spokespeople
# Foreign spokesperson
# 
# As Head
# Head
# Heads
# 
# As International Co-Applicant
# International Co-Applicant
# International Co-Applicants
# 
# As Participating Person
# Participating Person
# Participating Persons
# 
# As Participating scientist
# Participating scientist
# Participating scientists
# 
# As Project leader
# Project leader
# Project leaders
# 
# As Spokesperson
# Spokesperson
# Spokespersons
# 
# Participating Institution
# Participating institution

foo = project_relations %>% 
  mutate(relation_type = case_when(
    relation_type %in% c("Applicant", "Applicants", "As Applicant") ~ "Applicant",
    relation_type %in% c("Co-Applicant", "Co-applicants", "Co-applicant", "Co-Applicants", "As Co-Applicant", "As Co-applicant") ~ "Co-Applicant", 
    relation_type %in% c("Co-Applicant", "Co-applicants", "Co-applicant", "Co-Applicants", "As Co-Applicant", "As Co-applicant") ~ "Co-Applicant", 
    relation_type %in% c("Co-Applicant", "Co-applicants", "Co-applicant", "Co-Applicants", "As Co-Applicant", "As Co-applicant") ~ "Co-Applicant", 
    relation_type %in% c("Co-Applicant", "Co-applicants", "Co-applicant", "Co-Applicants", "As Co-Applicant", "As Co-applicant") ~ "Co-Applicant", 
    relation_type %in% c("Co-Applicant", "Co-applicants", "Co-applicant", "Co-Applicants", "As Co-Applicant", "As Co-applicant") ~ "Co-Applicant", 
    TRUE ~ relation_type
    )
  )

# foo = Reduce(function(project_relations, synonym_group){
#   return(
#     project_relations %>%
#       mutate(relation_type = ifelse(relation_type %in% synonym_group, synonym_group[1], relation_type))
#   )
# }, project_relations_from_project_pages, accumulate = F)

bar










# str_match(project_relations$relation_type, 'As\\s(.*)')
bar = project_relations %>%
  select(relation_type) %>%
  # mutate(relation_type = str_match(relation_type, '^As (.*)$')[,2])
  distinct(relation_type) %>%
  arrange(relation_type)


# foo = project_relations_from_project_pages %>%
#   filter(!grepl("^.*<a class.*$", field_value)) 
  

# TODO
# Subject Areas and Participating subject areas should be handled separately and should be extracted into 
# their own respective tables (also since there is no link-href string extraction needed for a referenced resource id)
project_subject_areas_field_names = c(
  "Participating subject areas", 
  "Subject Area"
)

projects = reshape_by_resource_type(generic_fields, "project")

people = reshape_by_resource_type(generic_fields, "person")
colnames(people)

institutions = reshape_by_resource_type(generic_fields, "institution")
colnames(institutions)


# 
  
foo = generic_fields %>%
  spread(field_name, field_value)

generic_fields[1047013,]
generic_fields[1047014,]
generic_fields[1047015,]



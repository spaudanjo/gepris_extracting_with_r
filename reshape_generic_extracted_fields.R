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


# Define predicate for relation fields for resource 'projects'
# so where field-name is part of the following list or starts with the following pattern/regex
# distinct_field_names = generic_fields %>%
#   select(field_name) %>%
#   distinct %>%
#   arrange(field_name)
# as.character(distinct_field_names$field_name)

# Besides the 'ProjectRelation: .*' pattern: in general, we identified the following fields which needs 
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

project_relations_from_non_project_pages = generic_fields %>% 
  filter(resource_type != "project") %>%
  filter(grepl("^ProjectRelation: .*$", field_name)) %>%
  mutate(project_id = as.character(field_value)) %>%
  mutate(reference_resource_type = resource_type) %>%
  mutate(reference_resource_id = as.character(resource_id)) %>%
  mutate(relation_type = gsub("ProjectRelation: ", "", field_name)) %>%
  mutate(relation_found_on = "REFERENCE_RESOURCE") %>%
  select(project_id, reference_resource_type, reference_resource_id, relation_type, relation_found_on)


project_relations_from_project_pages = generic_fields %>% 
  filter(resource_type == "project") %>%
  filter(field_name %in% project_relation_field_names) %>%
  mutate(project_id = as.character(resource_id)) %>%
  # extract field_value from reference_resource_type and reference_resource_id
  mutate(reference_resource_type = str_match(field_value, '<a class="intern" href="/gepris/([a-z]*)/')[,2]) %>%
  mutate(reference_resource_id = str_match(field_value, '<a class="intern" href="/gepris/[a-z]*/(\\d*)')[,2]) %>%
  mutate(relation_type = field_name) %>%
  mutate(relation_found_on = "PROJECT") %>%
  select(project_id, reference_resource_type, reference_resource_id, relation_type, relation_found_on)


# merge project_relations_from_project_pages and project_relations_from_non_project_pages
project_relations = project_relations_from_project_pages %>% 
  bind_rows(project_relations_from_non_project_pages) %>% 
# replace synonyms in project_relations_from_project_pages$relation_type (cluster synonyms (like c("Applicant", "Applicants")))
# I identified the following groups which should be 'normalised' to only one of their representations: 

# Applicant
# Applicants
# As Applicant

# Co-Applicant
# Co-applicants
# Co-applicant
# Co-Applicants
# As Co-Applicant
# As Co-applicant

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

  mutate(relation_type = case_when( 
    relation_type %in% c("Applicant", "Applicants", "As Applicant") ~ "Applicant", 
    relation_type %in% c("Co-Applicant", "Co-applicants", "Co-applicant", "Co-Applicants", "As Co-Applicant", "As Co-applicant") ~ "Co-Applicant", 
    relation_type %in% c("As Former applicant", "Former applicant", "Former applicants") ~ "Former applicant", 
    relation_type %in% c("As Cooperation partner", "Cooperation partner", "Cooperation partners") ~ "Cooperation partner", 
    relation_type %in% c("As Deputy spokesperson", "Deputy spokespeople", "Deputy spokesperson") ~ "Deputy spokesperson", 
    relation_type %in% c("As Foreign spokesperson", "Foreign spokespeople", "Foreign spokesperson") ~ "Foreign spokesperson", 
    relation_type %in% c("As Head", "Head", "Heads") ~ "Head", 
    relation_type %in% c("As International Co-Applicant", "International Co-Applicant", "International Co-Applicants") ~ "International Co-Applicant", 
    relation_type %in% c("As Participating Person", "Participating Person", "Participating Persons") ~ "Participating Person", 
    relation_type %in% c("As Participating scientist", "Participating scientist", "Participating scientists") ~ "Participating scientist", 
    relation_type %in% c("As Project leader", "Project leader", "Project leaders") ~ "Project leader", 
    relation_type %in% c("As Spokesperson", "Spokesperson", "Spokespersons") ~ "Spokesperson", 
    relation_type %in% c("Participating Institution", "Participating institution") ~ "Participating Institution", 
    TRUE ~ relation_type 
    ) 
  ) 

View(unique(project_relations$relation_type))

cleaned_project_relations = project_relations %>%
  select(-relation_found_on) %>%
  unique()



extract_start_and_end_year_of_term = function(term_str) {
  
  from_to = str_match(term_str, "^.*from ([0-9]+) to ([0-9]+).*$")
  if(length(from_to) == 3 && !is.na(from_to[1])) {
    return(c(from_to[2], from_to[3]))
  }
  
  since = str_match(term_str, "^.*since ([0-9]+).*$")
  if(length(since) == 2 && !is.na(since[1])) {
    return(c(since[2], NA))
  }
  
  fundedIn = str_match(term_str, "^.*Funded in ([0-9]+).*$")
  if(length(fundedIn) == 2 && !is.na(fundedIn[1])) {
    return(c(fundedIn[2], fundedIn[2]))
  }
  
  until = str_match(term_str, "^.*until ([0-9]+).*$")
  if(length(until) == 2 && !is.na(until[1])) {
    return(c(NA, until[2]))
  }
  
  ongoing = str_match(term_str, "^.*Currently being funded.*$")
  if(length(ongoing) == 1 && !is.na(ongoing[1])) {
    return(c("ongoing", "ongoing"))
  }
  
  return(c(NA, NA))
}
  
  


  # def extractFundingDateRange(parentElement: Element) = {
  #   val fundingDateRange = parentElement
  #   .select("span.name:matches(Term) + span.value")
  #   .text()
  #   
  #   val fromToRegex = "^.*from ([0-9]+) to ([0-9]+).*$".r
  #   val sinceRegex = "^.*since ([0-9]+).*$".r
  #   val fundedInRegex = "^.*Funded in ([0-9]+).*$".r
  #   val untilRegex = "^.*until ([0-9]+).*$".r
  #   val FundingOngoing = "^.*Currently being funded.*$".r
  #   
  #   val (start, end) = fundingDateRange match {
  #     case fromToRegex(start, end) => (start, end)
  #     case sinceRegex(seit) => (seit, "")
  #     case fundedInRegex(in) => (in, in)
  #     case untilRegex(bis) => ("", bis)
  #     case FundingOngoing() => ("ongoing", "ongoing")
  #     case _ => ("", "")
  #   }
  #   
  #   (start, end)
  # }


# length(str_match("from 1995 to 2004", "^.*from ([0-9]+) to ([0-9]+).*$"))
# length(str_match("since 2016<br>", "^.*since ([0-9]+).*$"))
# length(str_match("Funded in 2005", "^.*Funded in ([0-9]+).*$"))
extract_start_and_end_year_of_term("from 1995 to 2004")
extract_start_and_end_year_of_term("since 2016<br>")
extract_start_and_end_year_of_term("Funded in 2005")
extract_start_and_end_year_of_term("until 2008<br>")
extract_start_and_end_year_of_term("Currently being funded.")
extract_start_and_end_year_of_term("asd3jf fajksj 5")


projects = reshape_by_resource_type(generic_fields %>%
# For projects, we wan't to remove alle fields (that would be transformed now into columns) that 
# have relationship character, as these are handled separately in the cleaned_project_relations table
  filter(! field_name %in% project_relation_field_names) %>%
  # Also the fields "Subject Area" and "Participating subject areas" will be handled separately in a relation table. 
  # And we can ignore the field "Project identifier" (since it's core information is already covered by the column resource_id)
  filter(! field_name %in% c("Subject Area", "Participating subject areas", "Project identifier"))
                                      , "project") %>%




people = reshape_by_resource_type(generic_fields, "person")

institutions = reshape_by_resource_type(generic_fields, "institution")





# Subject Areas and Participating subject areas should be handled separately and should be extracted into 
# their own respective tables
# subject_area_test = "Theoretical Computer Science <br> Interactive and Intelligent Systems, Image and Language Processing, Computer Graphics and Visualisation"

extract_subject_areas_from_generic_fields = function(generic_fields, subject_area_column_name) {
  return(
    generic_fields %>% 
      filter(resource_type == "project") %>%
      filter(field_name == subject_area_column_name) %>%
      # 1. split by <br>
      mutate(field_value = strsplit(as.character(field_value), "<br> ")) %>% 
      unnest(field_value) %>%
      # 2. split by comma
      mutate(field_value = strsplit(as.character(field_value), ", ")) %>% 
      unnest(field_value) %>%
      select(
        project_id = resource_id,
        subject_area = field_value
      ) %>%
      mutate(subject_area = gsub("<br>", "", subject_area))
  )
}

project_subject_areas = extract_subject_areas_from_generic_fields(generic_fields, "Subject Area")
project_participating_subject_areas = extract_subject_areas_from_generic_fields(generic_fields, "Participating subject areas")


# WARNING: 
# Even though most of the times they are used to separate subject areas, 
# sometimes commas appear within one subject area. 
# Check for example the subject area string of the project with the id=246965231: 
# It is Operating, Communication, Database and Distributed Systems (which is just one subject area) 

# TODO: we could investigate wether a more accurate splitting is possible by doing a matching with the crawled and extracted subject areas 
# from the catalog search form of the GEPRIS system or the "official" webpage which lists the subject areas









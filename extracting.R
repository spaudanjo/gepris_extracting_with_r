
setwd("/Users/dspaude/code/b-arbeit/gepris-crawls/DQ-Notebook-with-crawl-from_final_2018-10-28--11-07-37-EN/stage1")

library(rvest)

# Get all HTML file names as list
project_html_file_names = list.files("project/html", full.names = T)
?list.files
length(project_html_file_names)
View(project_html_file_names)


extract_field_names_and_values_from_html_by_file_path = function(html_path){
  html_file = read_html(html_path)
  field_names = html_nodes(html_file, "span.name") %>%
    html_text %>%
    trimws(which = "both")
  
  field_name_value_tuples = lapply(field_names, function(field_name) {
    field_value = html_nodes(html_file, paste("span.name:contains('", field_name, "') + span.value", sep = "")) %>%
      html_text %>%
      trimws(which = "both")
    
    # print(paste("field_name:", field_name))
    # print(paste("field_value:", field_value))
    
    return(c(field_name, field_value))
  })
  
  return(field_name_value_tuples)
}

foo = extract_field_names_and_values_from_html_by_file_path(project_html_file_names[1])
foo[2]
print(foo)
project_html_file_names[1]

start_time <- Sys.time()
bar = lapply(
  # head(
    project_html_file_names
    # , n = 1000)
    , 
              extract_field_names_and_values_from_html_by_file_path
  )
end_time <- Sys.time()
end_time - start_time
# Time difference of 2.350484 hours





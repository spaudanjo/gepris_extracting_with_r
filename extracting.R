
setwd("/Users/dspaude/code/b-arbeit/gepris-crawls/DQ-Notebook-with-crawl-from_final_2018-10-28--11-07-37-EN/stage1")

library(rvest)

# Get all HTML file names as list
project_html_file_names = list.files("project/html", full.names = T)
?list.files
length(project_html_file_names)
View(project_html_file_names)


extract_field_names_and_values_from_html_by_file_path = function(html_path){
  html_file = read_html(html_path)
  # foo = html_nodes(html_file, "span.name:contains(DFG Programme) + span.value") %>%
  field_names = html_nodes(html_file, "span.name") %>%
    html_text %>%
    trimws(which = "both")
  
  foo = lapply(field_names, function(field_name) {
    # TODO/NEXT: hier werden gerade nicht die werte gefunden / selektor funzst nicht
    field_value = html_nodes(html_file, paste("span.name:contains('", field_name, "') + span")) %>%
      html_text %>%
      trimws(which = "both")
    return(c(field_name, field_value))
  })
  
  print(foo)
  
  return(field_names)
}

extract_field_names_and_values_from_html_by_file_path(project_html_file_names[1])

lapply(project_html_file_names, extract_field_names_and_values_from_html_by_file_path)

require(stringi)
require(stringr)

topoJSON_fillColor <- function(topoJSON_string,last_property,property_length=50,
                               fillColor="#ff3300",fillOpacity=0.7) {
                               #,weight=1,color="#555555",opacity=1) {
  
  last_property_pattern <- paste0(
    '"',last_property,'":',"[[#][\"][0-9][a-zA-Z][.]]","{0,",property_length,"}[}]"
    )
  
  old_pattern <- as.character(stringr::str_extract_all(string = topoJSON_string, 
                                                       pattern = last_property_pattern,
                                                       simplify = TRUE))
  
  removed_trailing_curly <- stringr::str_replace(string = old_pattern, 
                                                 pattern = "[}]", 
                                                 replacement = "")

  replacement_pattern <- paste0(removed_trailing_curly,
                                ',"style":{"fillColor":"',fillColor,
                                '","fillOpacity":',fillOpacity,'}}')
    
#  replacement_pattern <- paste0(removed_trailing_curly,
#                                ',"style":{"weight":',weight,',"color":"',color,
#                                '","opacity":',opacity,',"fillColor":"',fillColor,
#                                '","fillOpacity":',fillOpacity,'}}')
  
#  topoJSON_string_styled <- topoJSON_string
#  for (i in 1:length(replacement_pattern)) {
#    topoJSON_string_styled <- sub(topoJSON_string_styled,
#                                 pattern = old_pattern[i],
#                                 replacement = replacement_pattern[i])
#  }
  string <- topoJSON_string
  replacement <- replacement_pattern
  pattern <- old_pattern
  vec <- FALSE
  fixed = stri_replace_all_fixed(string, pattern, replacement, 
                                 vectorize_all = FALSE, 
                                 opts_fixed = attr(pattern, "options"))
  
  return(fixed)
}

# forfillColor <- town_binpal(as.numeric(topoJSON_property_extract(topoJSON_string = town_tj,
#                                                       property_name = "DENSITY",
#                                                       property_length = 50)))
# 
# last_property_pattern1 <- paste0(
#    '"',"DENSITY",'":',"[[#][\"][0-9][a-zA-Z][.]]","{0,",50,"}[}]"
#  )
#  old_pattern1 <- as.character(str_extract_all(string = town_tj, 
#                                              pattern = last_property_pattern1,
#                                              simplify = TRUE))
#  
#  removed_trailing_curly1 <- str_replace(string = old_pattern1, 
#                                         pattern = "[}]", 
#                                         replacement = "")
#  replacement_pattern1 <- paste0(removed_trailing_curly1,
#                                ',"style":{"weight":',2,',"color":"',"#FFFFFF",
#                                '","opacity":',1,',"fillColor":"',forfillColor,
#                                '","fillOpacity":',1,'}}')
#   string <- town_tj
#   replacement <- replacement_pattern1
#   pattern <- old_pattern1
#   vec <- FALSE
#   switch(typeof(pattern), empty = , bound = stop("Not implemented",
#         call. = FALSE), 
#         fixed = stri_replace_all_fixed(string, pattern, replacement, vectorize_all = FALSE, opts_fixed = attr(pattern, "options")), 
#         coll = stri_replace_all_coll(string,pattern, replacement, vectorize_all = FALSE, opts_collator = attr(pattern,"options")), 
#         regex = stri_replace_all_regex(string, pattern, fix_replacement(replacement), vectorize_all = FALSE, opts_regex = attr(pattern, "options"))
#         )
#  
#   leaflet() %>% addTiles() %>% addTopoJSON(coll) %>% setView(lng = 26,lat = -27,zoom = 6)
  

topoJSON_property_extract <- function(topoJSON_string,property_name,property_length=50){
  
  index_property_pattern <- paste0(
    '"',property_name,'":',"[[#][\"][0-9][a-zA-Z][.]]","{0,",property_length,"}[[}][,]]"
  )
  
  old_pattern <- as.character(
    stringr::str_extract_all(string = topoJSON_string,
                             pattern = index_property_pattern,
                             simplify = TRUE))
  
  removed_trailing_quote.comma.curly <- stringr::str_replace(
    string = old_pattern,
    pattern = "[\"]{0,1}[[,][}]]",
    replacement = "")
  
  stringr::str_replace(
    string = removed_trailing_quote.comma.curly,
    pattern = paste0('"',property_name,'":',"[\"]{0,1}"),
    replacement = "")
  
}


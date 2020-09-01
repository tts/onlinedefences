list_event_pages <- function() {
  
  httr::GET(
  
  url = "https://api.airtable.com/v0/appZhwU09LO6Hcr8F/Imported%20table?fields%5B%5D=Yliopisto&fields%5B%5D=URL&view=Grid%20view",
  
  httr::add_headers(
    `authorization` = sprintf("Bearer %s", key)
  ),
  
  encode = "json",
  
  httr::content_type_json(), 
  
  httr::verbose()
  
) -> res

res_list <- jsonlite::fromJSON(content(res, "text"), simplifyVector = FALSE)

# To data frame
dfs <- lapply(res_list$records, data.frame, stringsAsFactors = FALSE)
events <- bind_rows(dfs)

}

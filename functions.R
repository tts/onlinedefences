post_it <- function(df) {
  
  apply(df, 1, function(x) {
    
    httr::POST(
      url = airtablebase_events,
      
      httr::add_headers(
        `authorization` = sprintf("Bearer %s", key)
      ),
      
      encode = "json",
      
      httr::content_type_json(), 
      
      body = list(
        records = list(
          list(
            fields = list(
              Title = x[["title"]],
              URL = x[["link"]],
              Date = x[["date"]],
              University = x[["university"]],
              Info = x[["id"]]
            )
          )
        )
      ),
      
      httr::verbose()
      
    )
    
  })}


list_event_pages <- function() {
  
  httr::GET(
    
    url = airtablebase_sites,
    
    httr::add_headers(
      `authorization` = sprintf("Bearer %s", key)
    ),
    
    encode = "json",
    
    httr::content_type_json(), 
    
    httr::verbose()
    
  ) -> res
  
  res_list <- jsonlite::fromJSON(content(res, "text"), simplifyVector = FALSE)
  
  dfs <- lapply(res_list$records, data.frame, stringsAsFactors = FALSE)
  events <- bind_rows(dfs)
  
}

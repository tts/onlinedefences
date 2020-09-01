write_hy_event_records <- function() {
  
    hy <- events %>% 
      filter(fields.Yliopisto == 'Helsingin yliopisto') %>% 
      select(fields.URL)
    
    hy <- as.character(hy)
    
    nodes <- hy %>%
      read_html() %>%
      html_nodes('entry')
    
    df <- map_df(nodes, function(item) {
      
      data.frame(university = "Helsingin yliopisto",
                 id = str_squish(item %>% 
                                   html_node('id') %>% 
                                   html_text()),
                 type = str_squish(item %>% 
                                     html_node('eventtype') %>% 
                                     html_text()),
                 title = str_squish(item %>% 
                                      html_node('title') %>% 
                                      html_text()),
                 content = str_squish(item %>% 
                                        html_node('content') %>% 
                                        html_text() %>%
                                        paste0(collapse = " ") %>%
                                        gsub("\\n", "", .) %>%
                                        gsub("\ +", " ", .) %>%
                                        trimws()),
                 date = str_squish(item %>% 
                                     html_node('eventdate') %>% 
                                     html_text()),
                 stringsAsFactors=FALSE)
      
    })
    
    diss_online_events <- df %>% 
      filter(type == 'Väitöstilaisuudet') %>% 
      mutate(link = str_extract(content, 'http[^"<]+')) %>% 
      mutate(link = ifelse(str_detect(link, "urn") | str_detect(link, "juvenes"), NA, link)) %>% 
      filter(!is.na(link))
    
    
    for(i in 1:nrow(diss_online_events)) {
      
      httr::POST(
        
        url = "https://api.airtable.com/v0/appd2NiVv18KsG49j/Events",
        
        httr::add_headers(
          `authorization` = sprintf("Bearer %s", key)
        ),
        
        encode = "json",
        
        httr::content_type_json(), 
        
        body = make_body(diss_online_events[i, "title"], 
                         diss_online_events[i, "link"], 
                         diss_online_events[i, "date"],
                         diss_online_events[i, "university"],
                         diss_online_events[i, "id"]),
        
        httr::verbose()
        
      )
      
    }
}
    

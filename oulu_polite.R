write_oulu_event_records <- function() {
  
  oulu <- events %>% 
    filter(fields.Yliopisto == 'Oulun yliopisto') %>% 
    select(fields.URL)
  
  oulu <- as.character(oulu)
  
  session <- bow(oulu,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//div[@class = 'thesis liftup col-sm-12']")
  
  
  df_all <- map_df(nodes, function(item) {
    
    data.frame(university = "Oulun yliopisto",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::a[contains(@href, '/university/node/')]") %>% 
                                 html_attr("href")),
               person = str_squish(item %>% 
                                     html_node("h4") %>% 
                                     html_text()),
               field = str_squish(item %>% 
                                    html_node(xpath = "descendant::div[@class = 'field-content']") %>% 
                                    html_text()),
               title = NA,
               link = NA,
               date = str_squish(item %>% 
                                   html_node(xpath = "descendant::span[@class='date-display-single']") %>% 
                                   html_text()),
               stringsAsFactors=FALSE)
    
  })
  
  # Filter to future dates
  df <- df_all %>% 
    mutate(date = as.Date(date, "%d.%m.%Y")) %>% 
    filter(date >= Sys.Date())
  
  
  for (i in 1:nrow(df)) {
    
    url <- paste0("https://www.oulu.fi", df[i, "id"])
    
    page <- nod(session, url)
    
    time <- scrape(page) %>% 
      html_node(xpath = "//span[@class='date-display-single']") %>% 
      html_text()
    
    place <- scrape(page) %>% 
      html_node(xpath = "descendant::*[contains(h4, 'Place of the thesis')]/p") %>% 
      html_text()
    
    title <- scrape(page) %>% 
      html_node(xpath = "descendant::*[contains(h4, 'Topic of the')]/p") %>% 
      html_text()
    
    if(length(place) > 0) {
      df[i, "link"] <- place
    } else {
      df[i, "link"] <- NA
    }
    
    df[i, "time"] <- time
    df[i, "title"] <- title
    
  }
  
  df_tidy <- df %>% 
    mutate(title = str_squish(title),
           title_person = paste0(person, ", ", field, " : ", title),
           id = paste0("https://www.oulu.fi", id),
           link = str_extract(link, "https://[^\\s]+"),
           time = paste0(str_extract(time, "[0-9]*\\:[0-9]*$"), ":00"),
           datetime = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-title, -person, -field, -date, -time) %>% 
    rename(title = title_person,
           date = datetime) %>%
    filter(!is.na(link))
  
  
  for(i in 1:nrow(df_tidy)) {
    
    httr::POST(
      
      url = "https://api.airtable.com/v0/appd2NiVv18KsG49j/Events",
      
      httr::add_headers(
        `authorization` = sprintf("Bearer %s", key)
      ),
      
      encode = "json",
      
      httr::content_type_json(), 
      
      body = make_body(df_tidy[i, "title"], 
                       df_tidy[i, "link"], 
                       df_tidy[i, "date"],
                       df_tidy[i, "university"],
                       df_tidy[i, "id"]),
      
      httr::verbose()
      
    )
    
  }
  
}

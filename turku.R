write_turku_event_records <- function() {
  
  turku <- events %>% 
    filter(fields.Yliopisto == 'Turun yliopisto') %>% 
    select(fields.URL)
  
  turku <- as.character(turku)
  
  nodes <- turku %>% 
    read_html() %>% 
    html_nodes('.listing__cell')
  
  
  df_all <- map_df(nodes, function(item) {
    
    data.frame(university = "Turun yliopisto",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::h3/a") %>% 
                                 html_attr("href")),
               # Title includes also the person
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::h3/a") %>% 
                                    html_text()),
               link = NA,
               # If this contains 'etänä', parse the link from the event page (if available)
               link_future = str_squish(item %>% 
                                          html_node('.event__item.event__item--map-link') %>% 
                                          html_text()),
               date = str_squish(item %>% 
                                   html_node(xpath = "descendant::div[@class='event__time-value']") %>% 
                                   html_text()),
               stringsAsFactors=FALSE)
    
  })
  
  df <- df_all %>% 
    filter(str_detect(link_future, "[Ee]tänä"))
  
  
  for (i in 1:nrow(df)) {
    
    url <- paste0("https://www.utu.fi", df[i, "id"])
    
    page <- url %>% 
      read_html()
    
    title <- page %>% 
      html_node(xpath = "descendant::div[@class='field field__body event__body']") %>% 
      html_text() 
    
    place <- page %>% 
      html_node(xpath = "descendant::div[contains(@class, 'event__item--map-link')]/a") %>% 
      html_attr("href")
    
    # Future links are given ATM as Google placeholders
    if(is.na(place) | length(place) == 0 | str_detect(place, "google")) {
      df[i, "link"] <-  "https://to.be.announced"
    } else { 
      df[i, "link"] <- place
    }
    
    df[i, "title_long"] <- title
    
  }
  
  
  df_tidy <- df %>% 
    mutate(title = gsub("Väitös (.*): ", "", title),
           title_long = gsub('[”“]', '"', title_long),
           #title_long = gsub('"', '', title_long),
           title_long_stripped = str_extract(title_long, '".*"'),
           title_long_stripped = gsub('"', '', title_long_stripped),
           person_title = paste0(title, " : ", title_long_stripped),
           id = paste0("https://www.utu.fi", id),
           time_from_date = paste0(str_extract(date, "\\s[0-9]*\\.[0-9]*"), ":00"),
           time_from_date = gsub("\\.", ":", time_from_date),
           date_from_date = as.Date(str_extract(date, "^[^\\s]+"), "%d.%m.%Y"),
           datetime = as.POSIXct(paste(date_from_date, time_from_date), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-title, -title_long, -title_long_stripped, -link_future, -date, -time_from_date, -date_from_date) %>% 
    rename(date = datetime,
           title = person_title) 
  
  
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

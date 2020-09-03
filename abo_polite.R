write_abo_event_records <- function() {
  
  abo <- events %>% 
    filter(fields.Yliopisto == 'Åbo Akademi') %>% 
    select(fields.URL)
  
  abo <- as.character(abo)
  
  session <- bow(abo,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//a[@class='news-event-card-wrapper col-12 col-sm-6 col-lg-4 col-xl-3']")
  
  
  df <- map_df(nodes, function(item) {
    
    data.frame(university = "Åbo Akademi",
               id = str_squish(item %>% 
                                 html_attr("href")),
               person = NA,
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::h3") %>% 
                                    html_text()),
               link = NA,
               date = str_squish(item %>% 
                                   html_node(xpath = "descendant::div[@class='begintime']") %>% 
                                   html_text()),
               time = str_squish(item %>% 
                                   html_node(xpath = "descendant::div[@class='begintime']/../p") %>% 
                                   html_text()),
               stringsAsFactors=FALSE)
    
  })
  
  
  for (i in 1:nrow(df)) {
    
    url <- df[i, "id"]
    
    page <- nod(session, url)
    
    person <- scrape(page) %>% 
      html_node(xpath = "descendant::p/descendant::strong") %>% 
      html_text()
    
    title <- scrape(page) %>% 
      html_node(xpath = "descendant::p/descendant::em/descendant::strong") %>% 
      html_text()
    
    content <- scrape(page) %>% 
      html_text()
    
    # ATM all are streamed via this 
    link <- str_extract(content, "https://aboakademi.zoom.us[^\\s]+")
    
    if(!is.na(link)) {
      df[i, "link"] <- link
    } else {
      df[i, "link"] <- NA # so not online at all we assume
    }
    
    df[i, "title_long"] <- title
    df[i, "person"] <- person
    
  }
  
  df <- df %>% 
    filter(!is.na(link))
  
  
  df_tidy <- df %>% 
    mutate(title_person = paste0(title, ", ", str_squish(person), " : ", title_long),
           time = paste0(str_extract(time, "\\s[0-9]*\\.[0-9]*"), ":00"),
           time = gsub("\\.", ":", time),
           date = as.Date(date, "%d.%m.%Y"),
           datetime = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-title, -title_long, -person, -date, -time) %>% 
    rename(title = title_person,
           date = datetime) 
  
  
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

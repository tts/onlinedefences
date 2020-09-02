write_jyu_event_records <- function() {
  
  jyu <- events %>% 
    filter(fields.Yliopisto == 'Jyväskylän yliopisto') %>% 
    select(fields.URL)
  
  jyu <- as.character(jyu)
  
  nodes <- jyu %>% 
    read_html() %>% 
    html_nodes('.item-listing')
  
  items <- nodes %>% 
    html_nodes('li')
  
  df_all <- map_df(items, function(item) {
    
    data.frame(university = "Jyväskylän yliopisto",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::a") %>% 
                                 html_attr("href")),
               # Title often includes also the person
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::a") %>% 
                                    html_text()),
               link = NA,
               # If this contains 'online' etc, parse the link from the event page (if available)
               link_future = str_squish(item %>% 
                                          html_node(xpath = "descendant::p[@class='location']/span[last()]") %>% 
                                          html_text() %>% 
                                          tolower()),
               date = str_squish(item %>% 
                                   html_node(xpath = "descendant::span[@class='date']") %>% 
                                   html_text()),
               stringsAsFactors=FALSE)
    
  })
  
  df_all <- df_all %>% 
    mutate(link = str_extract(link_future, "http[s]?://[^\\s]+")) 
  
  df <- df_all %>% 
    filter(!is.na(link) | (str_detect(link_future, "online") | str_detect(tolower(link_future), "verkkovälitteinen")
           | str_detect(link_future, "http") | str_detect(link_future, "zoom")))

  for (i in 1:nrow(df)) {
    
    url <- df[i, "id"]
    
    page <- url %>% 
      read_html()
    
    # Title is usually given inside quotes, that's the best quess
    content <- page %>% 
      html_nodes(xpath = "descendant::div[@id='parent-fieldname-text']") %>% 
      html_text()
    
    content_tidy_quotes <- gsub('[”“]', '"', content)
    
    # Very hacky. Trying to select a "title-length" string
    quoted_texts <- str_extract_all(content_tidy_quotes, '".*"')
  
    possible_titles <- quoted_texts %>% 
      flatten(.) %>% 
      keep(nchar(.) > 30 & nchar(.) < 150)

    if (length(possible_titles) == 1) {
      df[i, "title_long"] <- as.character(possible_titles)
    } else {
      df[i, "title_long"] <- NA # giving up
    }
    
    # Link to stream
    if (is.na(df[i, "link"])) {
      
      # Parsing all links there are on the page
      content_nodes <- page %>% 
        html_nodes(xpath = "descendant::div[@id='parent-fieldname-text']") 
      
      links <- content_nodes %>% 
        html_nodes(xpath = "descendant::a") %>% 
        html_text()
      
      # These seem to be the top two
      hy_video <- match(1, str_detect(links, "http://video[^\\s]+"))
      jyu_video <- match(1, str_detect(links, 'https://r.jyu.fi/[^\\s]+'))
      
  
      if (!is.na(jyu_video)) {
        df[i, "link"] <- links[jyu_video]
      } 
      
      if (!is.na(hy_video)) {
        df[i, "link"] <- links[hy_video]
      } 
      
      if (is.na(hy_video) & is.na(jyu_video)) {
        df[i, "link"] <- "https://to.be.announced"
      }
      
    }
    
  }
  
  
  df_tidy <- df %>% 
    mutate(title = gsub("^Väitös: [0-9\\.\\:]+", "", title),
           person_title = ifelse(!is.na(title_long), paste0(title, " : ", title_long), title),
           person_title = gsub('["]', '', person_title),
           person_title = gsub('VERKKOVÄLITTEINEN', '', person_title),
           time_from_date = paste0(str_extract(date, "\\s[0-9]*\\.[0-9]*"), ":00"),
           time_from_date = gsub("\\.", ":", time_from_date),
           date_from_date = as.Date(str_extract(date, "^[^\\s]+"), "%d.%m.%Y"),
           datetime = as.POSIXct(paste(date_from_date, time_from_date), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-title, -title_long, -link_future, -date, -time_from_date, -date_from_date) %>% 
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

write_lappi_event_records <- function() {
  
  lappi <- events %>% 
    filter(fields.Yliopisto == 'Lapin yliopisto') %>% 
    select(fields.URL)
  
  lappi <- as.character(lappi)
  
  session <- bow(lappi,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//li[@class='iw_listitem col-xs-12 col-sm-3 clearfix']")
  
  
  df <- map_df(nodes, function(item) {
    
    data.frame(university = "Lapin yliopisto",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::a[@class='datevisual']") %>% 
                                 html_attr("href")),
               person = NA,
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::h3[@class='title']") %>% 
                                    html_text()),
               link = NA,
               date = str_squish(item %>% 
                                   html_node(xpath = "descendant::a[@class='dates']") %>% 
                                   html_text()),
               stringsAsFactors=FALSE)
    
  })
  
  df <- df %>% 
    filter(str_detect(title, "[vV]äitös|[dD]efence"))
  
  
  for (i in 1:nrow(df)) {
    
    url <- paste0("https://www.ulapland.fi", df[i, "id"])
    
    # Note, bow here
    session <- bow(url) 
    
    title <- scrape(session) %>% 
      html_node(xpath = "descendant::div[@class='text']/descendant::strong") %>% 
      html_text()
    
    time <- scrape(session) %>% 
      html_node('.dates') %>% 
      html_text()
    
    content <- scrape(session) %>% 
      html_text()
    
    # ATM all are streamed via this 
    link <- str_extract(content, "https://connect.eoppimispalvelut[^\\s]+")
    
    if(!is.na(link)) {
      df[i, "link"] <- link
    } else {
      df[i, "link"] <- NA # so not online at all we assume
    }
    
    df[i, "title_long"] <- title
    df[i, "time"] <- time
    
  }
  
  df <- df %>% 
    filter(!is.na(link))
  
  
  df_tidy <- df %>% 
    mutate(title_person = paste0(title, " : ", title_long),
           id = paste0("https://www.ulapland.fi", id),
           time_from_date = paste0(str_extract(time, "\\s[0-9]*\\:[0-9]*"), ":00"),
           date_from_date = as.Date(str_extract(time, "^[^\\s]+"), "%d.%m.%Y"),
           datetime = as.POSIXct(paste(date_from_date, time_from_date), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-title, -title_long, -person, -date, -time, -time_from_date, -date_from_date) %>% 
    rename(title = title_person,
           date = datetime) 
  
  post_it(df_tidy)
  
}

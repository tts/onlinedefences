write_jyu_event_records <- function() {
  
  jyu <- events %>% 
    filter(fields.Yliopisto == 'Jyväskylän yliopisto') %>% 
    select(fields.URL)
  
  jyu <- as.character(jyu)
  
  session <- bow(jyu,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
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
               link_text = str_squish(item %>% 
                                        html_node(xpath = "descendant::p[@class='location']/span[last()]") %>% 
                                        html_text() %>% 
                                        tolower()),
               date = str_squish(item %>% 
                                   html_node(xpath = "descendant::span[@class='date']") %>% 
                                   html_text()),
               stringsAsFactors=FALSE)
    
  })
  
  df <- df_all %>% 
    mutate(link = str_extract(link_text, "http[s]?://[^\\s]+"),
           # TRUE if there is a mention of future online event
           link_to_be = (str_detect(link_text, "online") | str_detect(tolower(link_text), "verkkovälitteinen")
                         | str_detect(link_text, "http") | str_detect(link_text, "zoom")))
  
  df_res <- df %>% 
    
    pmap_dfr(function(...) {
      current <- tibble(...)
      url <- current$id
      page <- nod(session, url)
      
      content <- scrape(page) %>%
        html_node(xpath = "descendant::article") %>%
        html_text()
      
      content_tidy_quotes <- gsub('[”“]', '"', content)
      
      # https://stackoverflow.com/a/35804434
      # Constraint width lookbehind: 'väitöskir' precedes a quoted string (=title) 
      # within a distance of max 20 chars
      title_long <- content_tidy_quotes %>% 
        str_extract(., '(?<=väitöskir[^"]{1,20}")[^"]+')
  
      # Parsing all links on the page
      links <- scrape(page) %>%
        html_nodes(xpath = "descendant::a") %>%
        html_text()
      
      # These seem to be the top two
      hy_video <- match(1, str_detect(links, "http://video[^\\s]+"))
      jyu_video <- match(1, str_detect(links, 'https://r.jyu.fi/[^\\s]+'))
      
      current %>% 
        mutate(title_long = ifelse(!is.na(title_long), title_long, NA),
               link = case_when(!is.na(link) ~ link,
                                is.na(link) & !is.na(jyu_video) ~ links[jyu_video],
                                is.na(link) & !is.na(hy_video) ~ links[hy_video],
                                is.na(link) & is.na(hy_video) & is.na(jyu_video) & link_to_be == TRUE ~ "https://to.be.announced"))
      
    })
  
  
  df_tidy <- df_res %>% 
    mutate(title = gsub("^Väitös: [0-9\\.\\:]+", "", title),
           person_title = ifelse(!is.na(title_long), paste0(title, " : ", title_long), title),
           person_title = gsub('["]', '', person_title),
           person_title = gsub('VERKKOVÄLITTEINEN', '', person_title),
           time_from_date = paste0(str_extract(date, "\\s[0-9]*\\.[0-9]*"), ":00"),
           time_from_date = gsub("\\.", ":", time_from_date),
           date_from_date = as.Date(str_extract(date, "^[^\\s]+"), "%d.%m.%Y"),
           datetime = as.POSIXct(paste(date_from_date, time_from_date), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-title, -date) %>% 
    rename(date = datetime,
           title = person_title) %>% 
    filter(!is.na(link)) %>% 
    select(university, id, link, title, date)
  
  
  post_it(df_tidy)
  
}

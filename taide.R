write_taide_event_records <- function() {
  
  taide <- events %>% 
    filter(fields.Yliopisto == 'Taideyliopisto') %>% 
    select(fields.URL)
  
  taide <- as.character(taide)
  
  session <- bow(taide,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//li[@class='search-result-item listing-item']")
  
  df_all <- map_df(nodes, function(item) {
    
    data.frame(university = "Taideyliopisto",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::a") %>% 
                                 html_attr("href")),
               # Title includes also the person
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::a") %>% 
                                    html_text()),
               link = NA,
               date = NA,
               stringsAsFactors=FALSE)
  })

  
  df_res <- df_all %>% 
    pmap_dfr(function(...) {
      current <- tibble(...)
      url <- current$id
      page <- nod(session, url) 
      
      date_scraped <- scrape(page) %>%
        html_node(xpath = "descendant::article/descendant::div[@class='event-sidebar__event-date']/descendant::p") %>%
        html_text()
      
      link_scraped <- scrape(page) %>% 
        html_node(xpath = "descendant::article/descendant::a[contains(@href, 'youtube')]") %>%
        html_attr("href")
      
      current %>% 
        mutate(date = date_scraped,
               link = ifelse(!is.na(link_scraped), link_scraped, NA))
      
    })
  
  
  df_tidy <- df_res %>% 
    mutate(time = paste0(str_extract(date, "klo [^\\s]+"), ":00"),
           time = gsub("klo ", "", time),
           date = as.Date(str_extract(date, "^[^\\s]+"), "%d.%m.%Y"),
           datetime = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-date) %>% 
    rename(date = datetime) %>% 
    select(university, id, title, date, link) %>% 
    filter(date >= Sys.Date() & !is.na(link))
  
  
  post_it(df_tidy)
  
}

write_lut_event_records <- function() {
  
  lut <- events %>% 
    filter(fields.Yliopisto == 'Lappeenrannan–Lahden teknillinen yliopisto LUT') %>% 
    select(fields.URL)
  
  lut <- as.character(lut)
  
  session <- bow(lut,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//div[starts-with(@class, 'asset-abstract upcoming-event')]") # whitespace at the end
  
  
  df_all <- map_df(nodes, function(item) {
    
    data.frame(university = "Lappeenrannan–Lahden teknillinen yliopisto LUT",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::h3/a") %>% 
                                 html_attr("href")),
               person = str_squish(item %>% 
                                    html_node(xpath = "descendant::h3/a") %>% 
                                    html_text()),
               link = NA,
               date = str_squish(item %>% 
                                   html_node(xpath = "descendant::div[@class='metadata-entry metadata-time-location']/span") %>% 
                                   html_text()),
               stringsAsFactors=FALSE)
    
  })
  
  df <- df_all %>% 
    filter(str_detect(person, "Väitöstilaisuus"))
  
  df_res <- df %>% 
    pmap_dfr(function(...){
      current <- tibble(...)
      url <- current$id
      
      page <- nod(session, url)
      
      link_scraped <- scrape(page) %>% 
        html_node(xpath = "descendant::a[contains(@href, 'zoom')]") %>% 
        html_attr("href")
      
      title_scraped <- scrape(page) %>% 
        html_node(xpath = "descendant::em") %>% 
        html_text()
      
      current %>% 
        mutate(link = ifelse(is.na(link_scraped) | length(link_scraped) == 0, NA, link_scraped),
               title = title_scraped)
      
      
    })
  
  
  df_tidy <- df_res %>% 
    mutate(
      time_from_date = str_extract(date, "[0-9][0-9]\\.[0-9][0-9]\\-"),
      time_from_date = gsub("\\.", ":", time_from_date),
      time_from_date = gsub("\\-", "", time_from_date),
      time_from_date = paste0(time_from_date, ":00"),
      date_from_date = as.Date(str_extract(date, "^[^\\s]+"), "%d.%m.%Y"),
      datetime = as.POSIXct(paste(date_from_date, time_from_date), format="%Y-%m-%d %H:%M:%S"),
      datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-date) %>% 
    rename(date = datetime) %>% 
    filter(!is.na(link)) %>% 
    select(university, id, link, title, date)
  
  
  post_it(df_tidy)
  
}

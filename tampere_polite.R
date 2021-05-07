write_tampere_event_records <- function() {
  
  tre <- events %>% 
    filter(fields.Yliopisto == 'Tampereen yliopisto') %>% 
    select(fields.URL)
  
  tre <- as.character(tre)
  
  session <- bow(tre,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//div[@class = 'grid__content']")
  
  
  df_all <- map_df(nodes, function(item) {
    
    data.frame(university = "Tampereen yliopisto",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::h3/a") %>% 
                                 html_attr("href")),
               # Unless person's name is part of the title, it cannot securely be parsed
               #person = NA,
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::h3/a") %>% 
                                    html_text()),
               link = NA,
               # If this contains 'etäyhteydellä', parse the link from the event page
               link_future = str_squish(item %>% 
                                          html_node('.field__value.field__value--field-event-location') %>% 
                                          html_text()),
               date = NA,
               time = NA,
               stringsAsFactors=FALSE)
    
  })
  
  df <- df_all %>% 
    filter(str_detect(link_future, "[Ee]täyhtey"))

  
  df_res <- df %>% 
    pmap_dfr(function(...){
      current <- tibble(...)
      url <- paste0("https://www.tuni.fi", current$id)
      
      page <- nod(session, url)
      
      place_scraped <- scrape(page) %>%
        html_node(xpath = "descendant::a[contains(@href, 'zoom') or contains(@href, 'teams')
                  or contains(@href, 'panopto')]") %>%
        html_attr("href")
      
      remDr$navigate(url)

      datetime_scraped <- remDr$findElement(using = "xpath", "//div[@class='ep--date-item']")
      datetime_scraped_char <- unlist(datetime_scraped$getElementText()) # 12.05.2021 12.00 - 16.00

      date_scraped <- as.Date(str_extract(datetime_scraped_char, "^[^\\s]+"), "%d.%m.%Y")
      time_scraped <- str_extract(datetime_scraped_char, "\\s[^\\.]+") %>% 
        str_squish() %>% 
        paste0(., ":00:00")
      
      current %>%
        mutate(link = ifelse(is.na(place_scraped) | length(place_scraped) == 0, "https://to.be.announced",
                             place_scraped),
               date = date_scraped,
               time = time_scraped)
  
    })
  
  df_tidy <- df_res %>% 
    mutate(title = str_squish(title),
           id = paste0("https://www.tuni.fi", id),
           datetime = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-date) %>% 
    rename(date = datetime) %>%
    select(university, id, link, title, date) 

  post_it(df_tidy)
  
}

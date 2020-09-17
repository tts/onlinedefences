write_uef_event_records <- function() {
  
  uef <- events %>% 
    filter(fields.Yliopisto == 'Itä-Suomen yliopisto') %>% 
    select(fields.URL)
  
  uef <- as.character(uef)
  
  session <- bow(uef,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//div[@role='article']")
  
  
  df <- map_df(nodes, function(item) {
    
    data.frame(university = "Itä-Suomen yliopisto",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::a") %>% 
                                 html_attr("href")),
               person = NA,
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::h2[@class='liftup__title']") %>% 
                                    html_text()),
               link = NA,
               date_day = str_squish(item %>% 
                                       html_node(xpath = "descendant::p[@class='date__day']") %>% 
                                       html_text()),
               date_month = str_squish(item %>% 
                                         html_node(xpath = "descendant::p[@class='date__month']") %>% 
                                         html_text()),
               time = NA,
               stringsAsFactors=FALSE)
    
  })
  
  df <- df %>% 
    filter(str_detect(title, "[vV]äitös")) %>% 
    mutate(link_to_be = str_detect(title, "[vV]erkossa"))
  
  df_res <- df %>% 
    pmap_dfr(function(...) {
      current <- tibble(...)
      url <- paste0("https://www.uef.fi", current$id)
      page <- nod(session, url) 
      
      content <- scrape(page) %>%
        html_node(xpath = "descendant::article") %>%
        html_text()
      
      content_tidy_quotes <- gsub('[”“]', '"', content)
      
      # Title is either within quotes
      title_long_quotes <- content_tidy_quotes %>% 
        str_extract(., '(?<=väitöskir[^"]{1,20}")[^"]+')
      
      # or inside em element
      title_long_em <- scrape(page) %>% 
        html_node(xpath = "descendant::em") %>% 
        html_text()
      
      # Parsing all links on the page
      links <- scrape(page) %>%
        html_nodes(xpath = "descendant::a") %>%
        html_attr("href") 
      
      # These seem to be the top two
      uef_video <- match(1, str_detect(links, "www.uef.fi/live[^\\s]+"))
      lsc_video <- match(1, str_detect(links, 'stream.lifesizecloud[^\\s]+'))
      
      event <- scrape(page) %>%
        html_node(xpath = "descendant::div[@class='grid__item']")

      time_scraped <- event %>%
        html_node(xpath = "dl/dt[contains(text(), 'Aika:')]/following-sibling::dd") %>%
        html_text()
   
      current %>% 
        mutate(title_long = ifelse(!is.na(title_long_quotes), title_long_quotes, 
                                   ifelse(is.na(title_long_quotes) & !is.na(title_long_em), title_long_em,
                                                NA)),
               time = time_scraped,
               link = ifelse(!is.na(link), link,
                             ifelse(is.na(link) & !is.na(uef_video), links[uef_video],
                                    ifelse(is.na(link) & is.na(uef_video) & !is.na(lsc_video), links[lsc_video],
                                           ifelse(is.na(link) & is.na(uef_video) & is.na(lsc_video) & link_to_be == TRUE, "https://to.be.announced",
                                                  NA)))))
      
    })
  
  df <- df_res %>% 
    filter(!is.na(link))
  
  df_tidy <- df %>% 
    mutate(id = paste0("https://www.uef.fi", id),
           title_long = ifelse(is.na(title_long), "-", title_long),
           title_person = paste0(title, " : ", title_long),
           time = str_squish(time),
           time = paste0(str_extract(time, "^[^\\-]+"), ":00"),
           date_day = ifelse(nchar(date_day)==1, paste0("0", date_day), date_day), 
           month_from_date_month = case_when(date_month == "Syys" ~ "09",
                                             date_month == "Loka" ~ "10",
                                             date_month == "Marras" ~ "11",
                                             date_month == "Joulu" ~ "12",
                                             date_month == "Tammi" ~ "01",
                                             date_month == "Helmi" ~ "02",
                                             date_month == "Maalis" ~ "03",
                                             date_month == "Huhti" ~ "04",
                                             date_month == "Touko" ~ "05",
                                             date_month == "Kesä" ~ "06",
                                             date_month == "Heinä" ~ "07",
                                             date_month == "Elo" ~ "08",
                                             TRUE ~ "other"),
           date = paste(date_day, month_from_date_month, year_now, sep = "."),
           date = as.Date(date, "%d.%m.%Y"),
           datetime = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-title, -date) %>% 
    rename(title = title_person,
           date = datetime) %>% 
    mutate(title = gsub("\\(myös verkossa\\)", "", title)) %>% 
    select(university, id, title, date, link) %>% 
    filter(date >= Sys.Date())
  
  
  post_it(df_tidy)
  
}

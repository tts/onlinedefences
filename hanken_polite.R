write_hanken_event_records <- function() {
  
  hanken <- events %>% 
    filter(fields.Yliopisto == 'Svenska handelshögskolan') %>% 
    select(fields.URL)
  
  hanken <- as.character(hanken)
  
  session <- bow(hanken,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//div[@class='row bs-2col node node--type-calendar-item node--view-mode-teaser']")
  
  
  df <- map_df(nodes, function(item) {
    
    data.frame(university = "Svenska handelshögskolan",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::a") %>% 
                                 html_attr("href")),
               person = NA,
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::div[@class='p-header p-header-h6']") %>% 
                                    html_text()),
               link = NA,
               date_day = str_squish(item %>% 
                                       html_node(xpath = "descendant::div[@class='date-day']") %>% 
                                       html_text()),
               date_month = str_squish(item %>% 
                                         html_node(xpath = "descendant::div[@class='date-month']") %>% 
                                         html_text()),
               time = str_squish(item %>% 
                                   html_node(xpath = "descendant::div[@class='inline-block field--name-field-time']") %>% 
                                   html_text()),
               stringsAsFactors=FALSE)
    
  })
  
  df <- df %>% 
    filter(str_detect(title, "[dD]isputation"))
  
  df_res <- df %>% 
    
    pmap_dfr(function(...) {
      current <- tibble(...)
      url <- paste0("https://www.hanken.fi", current$id)
      
      page <- nod(session, url) 
      
      # TO DO check this when there are more cases
      person_scraped <- scrape(page) %>% 
        html_node(xpath = "descendant::p/descendant::strong") %>% 
        html_text()
      
      content <- scrape(page) %>% 
        html_text()
      
      title_scraped <- str_match(content, "Avhandlingsmanuskriptets titel: (.*)")[1]
      
      current %>% 
        mutate(title_long = title_scraped,
               link = "https://to.be.announced",
               person = person_scraped)
      
      
    })
  
  df_tidy <- df_res %>% 
    mutate(id = paste0("https://www.hanken.fi", id),
           title_long = gsub("Avhandlingsmanuskriptets titel: ", "", title_long),
           title_person = paste0(title, " : ", title_long),
           time = paste0(time, ":00"),
           # Changing switch to case-when. Switch works in this case because there is only one item
           month_from_date_month = case_when(date_month == "Sep" ~ "09",
                                             date_month == "Oct" ~ "10",
                                             date_month == "Nov" ~ "11",
                                             date_month == "Dec" ~ "12",
                                             date_month == "Jan" ~ "01",
                                             date_month == "Feb" ~ "02",
                                             date_month == "Mar" ~ "03",
                                             date_month == "Apr" ~ "04",
                                             date_month == "May" ~ "05",
                                             date_month == "Jun" ~ "06",
                                             date_month == "Jul" ~ "07",
                                             date_month == "Aug" ~ "08",
                                             TRUE ~ "other"),
           date = paste(date_day, month_from_date_month, year_now, sep = "."),
           date = as.Date(date, "%d.%m.%Y"),
           datetime = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S"),
           datetime = as_datetime(datetime, tz = "UTC")) %>% 
    select(-title, -date) %>% 
    rename(title = title_person,
           date = datetime) %>% 
    select(university, id, title, date, link)
  
  
  post_it(df_tidy)
  
  
}

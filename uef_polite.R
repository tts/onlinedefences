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
    filter(str_detect(title, "[vV]äitös"))
  
  
  for (i in 1:nrow(df)) {
    
    url <- paste0("https://www.uef.fi", df[i, "id"])
    
    page <- nod(session, url) 

    title <- scrape(page) %>% 
      html_node(xpath = "descendant::p/descendant::em") %>% 
      html_text()
    
    # If not found, can be just in quotes
    # Note that for some reason the title of the following is not captured within the loop
    # https://www.uef.fi/fi/tapahtuma/ytm-iiris-lehdon-vaitostilaisuus-yhteiskuntapolitiikka-joensuu-myos-verkossa
    if (is.na(title)) {
      
      content <- scrape(page) %>% 
        html_nodes(xpath = "descendant::p") %>% 
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
      
    }
    
    event <- scrape(page) %>% 
      html_node(xpath = "descendant::div[@class='grid__item']")
    
    time <- event %>% 
      html_node(xpath = "dl/dt[contains(text(), 'Aika:')]/following-sibling::dd") %>% 
      html_text()
    
    link <- event %>% 
      html_node(xpath = "a") %>% 
      html_attr("href")
    
    # Is there a mention about a coming stream?
    if (is.na(link)) {
      link_text <- event %>% 
        html_node(xpath = "dl/dt[contains(text(), 'Tapahtumapaikka:')]/following-sibling::dd/div") %>% 
        html_text()
      
      if(str_detect(link_text, "[vV]erkossa"))
        link <- "https://to.be.announced"
    }
    
    if (!is.na(link)) {
      df[i, "link"] <- link
    } 
    
    df[i, "title_long"] <- title
    df[i, "time"] <- time
    
  }
  
  
  df <- df %>% 
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
    select(-title, -title_long, -person, -date_day, -date, -date_month, -time, -month_from_date_month) %>% 
    rename(title = title_person,
           date = datetime) 
  
  
  post_it(df_tidy)
  
}

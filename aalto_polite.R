write_aalto_event_records <- function() {
  
  aalto <- events %>% 
    filter(fields.Yliopisto == 'Aalto-yliopisto') %>% 
    select(fields.URL)
  
  aalto <- as.character(aalto)
  
  session <- bow(aalto,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//div[@class='aalto-listing-row__content-column']")
  
  
  df <- map_df(nodes, function(item) {
    
    data.frame(university = "Aalto-yliopisto",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::a[@class='aalto-listing-row__link']") %>% 
                                 html_attr("href")),
               person = str_squish(item %>% 
                                     html_node(xpath = "descendant::a[@class='aalto-listing-row__link']/span") %>% 
                                     html_text()),
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::div[@class='aalto-listing-row__summary']") %>% 
                                    html_text()),
               link = str_squish(item %>% 
                                   html_node(xpath = "descendant::div[@class='aalto-listing-row__meta-item aalto-listing-row__meta-item--location']//div[contains(., 'http')]") %>%
                                   html_text()),
               date = str_squish(item %>% 
                                   html_node(xpath = "descendant::time") %>% 
                                   html_attr("datetime")),
               stringsAsFactors=FALSE)
    
  })
  
  df_nolink <- df %>% 
    filter(is.na(link))
  
  df_withlink <- df %>% 
    filter(!is.na(link))
  
  # If the link is not given on the start page, look for it from the page of the event
  # ATM, all Aalto defences will be online
  for (i in 1:nrow(df_nolink)) {
    
    url <- paste0("https://www.aalto.fi", df_nolink[i, "id"])
    
    page <- nod(session, url) 
  
    l <- scrape(page) %>% 
      html_nodes(xpath = "//div[@class='paragraph paragraph--type--text-text paragraph--view-mode--default']//descendant::a[contains(@href, 'zoom')]") %>% 
      html_attr("href")
    
    if(length(l) > 0) {
      df_nolink[i, "link"] <- l
    } else {
      df_nolink[i, "link"] <- "https://to.be.announced"
    }
    
  }
  
  df_links <- rbind(df_nolink, df_withlink)
  
  df_links_tidy <- df_links %>% 
    mutate(title = gsub("The title of the dissertation is ", "", title),
           title = gsub("Title of the dissertation is ", "", title),
           title = gsub("The title of the thesis is ", "", title),
           title = gsub("Title of the thesis is ", "", title),
           person = gsub("Defen[sc]e? of dissertation in the field of ", "", person),
           person = gsub("Defence in the field of ", "", person),
           person = stringr::str_to_title(person),
           title_person = paste0(person, " : ", title),
           id = paste0("https://www.aalto.fi", id),
           link = gsub("^//", "https://", link),
           link = str_squish(str_extract(link, "https.*"))) %>% 
    select(-title, -person) %>% 
    rename(title = title_person) %>% 
    mutate(title = gsub('["”“]', '', title))
  
  
  for(i in 1:nrow(df_links_tidy)) {
    
    httr::POST(
      
      url = "https://api.airtable.com/v0/appd2NiVv18KsG49j/Events",
      
      httr::add_headers(
        `authorization` = sprintf("Bearer %s", key)
      ),
      
      encode = "json",
      
      httr::content_type_json(), 
      
      body = make_body(df_links_tidy[i, "title"], 
                       df_links_tidy[i, "link"], 
                       df_links_tidy[i, "date"],
                       df_links_tidy[i, "university"],
                       df_links_tidy[i, "id"]),
      
      httr::verbose()
      
    )
    
  }
  
}

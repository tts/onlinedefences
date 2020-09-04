write_tampere_event_records <- function() {
  
  tre <- events %>% 
    filter(fields.Yliopisto == 'Tampereen yliopisto') %>% 
    select(fields.URL)
  
  tre <- as.character(tre)
  
  session <- bow(tre,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//div[@class = 'views-row listing__item']")
  
  
  df_all <- map_df(nodes, function(item) {
    
    data.frame(university = "Tampereen yliopisto",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::article") %>% 
                                 html_attr("about")),
               # Unless person's name is part of the title, it cannot securely be parsed
               person = NA,
               title = str_squish(item %>% 
                                    html_node(xpath = "descendant::h3/a") %>% 
                                    html_text()),
               link = NA,
               # If this contains 'etäyhteydellä', parse the link from the event page
               link_future = str_squish(item %>% 
                                          html_node('.field__value.field__value--field-event-location') %>% 
                                          html_text()),
               date = str_squish(item %>% 
                                   html_node(xpath = "descendant::time") %>% 
                                   html_attr("datetime")),
               stringsAsFactors=FALSE)
    
  })
  
  df <- df_all %>% 
    filter(str_detect(link_future, "[Ee]täyhtey"))
  
  
  for (i in 1:nrow(df)) {
    
    url <- paste0("https://www.tuni.fi", df[i, "id"])
    
    page <- nod(session, url)
    
    place <- scrape(page) %>% 
      html_node(xpath = "descendant::a[contains(@href, 'zoom') or contains(@href, 'teams')]") %>% 
      html_attr("href")
    
    if(is.na(place) | length(place) == 0) {
      df[i, "link"] <-  "https://to.be.announced"
    } else { 
      df[i, "link"] <- place
    }
    
  }
  
  
  df_tidy <- df %>% 
    mutate(title = str_squish(title),
           id = paste0("https://www.tuni.fi", id)) %>% 
    select(-person, -link_future)
  
  
  post_it(df_tidy)
  
}

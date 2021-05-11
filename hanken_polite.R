write_hanken_event_records <- function() {
  
  hanken <- events %>% 
    filter(fields.Yliopisto == 'Svenska handelshögskolan') %>% 
    select(fields.URL)
  
  hanken <- as.character(hanken)
  
  session <- bow(hanken,
                 user_agent = "sonkkilat@gmail.com")
  
  nodes <- scrape(session) %>% 
    html_nodes(xpath = "//div[@class='p-header p-header-h5']")
  
  df <- map_df(nodes, function(item) {
    
    data.frame(university = "Svenska handelshögskolan",
               id = str_squish(item %>% 
                                 html_node(xpath = "descendant::a") %>% 
                                 html_attr("href")),
               person = NA,
               title = str_squish(item %>% 
                                    html_node(xpath = "ancestor::div[@class='inline-block'][1]/div[2]") %>% 
                                    html_text()),
               link = NA,
               date = str_squish(item %>% 
                                   html_node(xpath = "preceding::div[@class='inline-block'][1]/div") %>% 
                                   html_text()),
               time = NA,
               stringsAsFactors=FALSE)
    
  })

  
  df_res <- df %>% 
    
    pmap_dfr(function(...) {
      current <- tibble(...)
      url <- paste0("https://www.hanken.fi", current$id)
      
      page <- nod(session, url) 
      
      date_time <- scrape(page) %>% 
        html_node(xpath = "descendant::time") %>% 
        html_attr("datetime")
      
      video <- scrape(page) %>% 
        html_node(xpath = "descendant::strong/a") %>% 
        html_attr("href")

      current %>% 
        mutate(link = ifelse(!is.na(video) & grepl("https://go.hanken.fi", video), video, "https://to.be.announced"),
               date_time = date_time)
      
    })
  
  df_tidy <- df_res %>% 
    mutate(id = paste0("https://www.hanken.fi", id),
           deftime = as.POSIXct(date_time, format="%Y-%m-%dT%H:%M:%SZ")) %>% 
    select(-date, -time) %>% 
    rename(date = deftime) %>% 
    filter(date >= Sys.time()) %>% 
    select(university, id, link, title, date)
  
  
  post_it(df_tidy)
  
  
}

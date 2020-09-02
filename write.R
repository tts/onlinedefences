library(httr)
library(rvest)
library(tidyverse)
library(lubridate)

source("list.R")
source("aalto.R")
source("hy.R")
source("oulu.R")
source("tampere.R")
source("turku.R")
source("jyu.R")
source("lappi.R")
source("abo.R")

key <- Sys.getenv("airtableKey")

# POST args
make_body <- function(title, link, date, university, id){
  
  list(
    records = list(
      list(
        fields = list(
          Title = title,
          URL = link,
          Date = date,
          University = university,
          Info = id
        )
      )
    )
  ) -> body
  
  return(body)
  
}

# 1. Delete all old event records from the base (TO DO),
# 2. Populate it with new ones generated next

events <- list_event_pages()
write_aalto_event_records()
write_hy_event_records()
write_oulu_event_records()
write_tampere_event_records()
write_turku_event_records()
write_jyu_event_records()
write_lappi_event_records()
write_abo_event_records()




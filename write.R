library(httr)
library(polite)
library(rvest)
library(tidyverse)
library(lubridate)

source("list.R")

# Taideyliopisto, Lappeenrannan–Lahden teknillinen yliopisto LUT
# and Maanpuolustuskorkeakoulu do not have future online defences ATM
source("aalto_polite.R")
source("hy.R") # RSS feed
source("oulu_polite.R")
source("tampere_polite.R")
source("turku_polite.R")
source("jyu_polite.R")
source("lappi_polite.R")
source("abo_polite.R")
source("hanken_polite.R")
source("uef_polite.R")

key <- Sys.getenv("airtableKey")
year_now <- year(Sys.Date())

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
write_hanken_event_records()
write_uef_event_records()

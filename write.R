library(httr)
library(polite)
library(rvest)
library(tidyverse)
library(lubridate)
library(RSelenium)

source("functions.R")

# Maanpuolustuskorkeakoulu does not have future online defences ATM
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
source("taide_polite.R")
source("lut_polite.R")

key <- Sys.getenv("airtableKey")
airtablebase_events <- "https://api.airtable.com/v0/appd2NiVv18KsG49j/Events"
airtablebase_sites <- "https://api.airtable.com/v0/appZhwU09LO6Hcr8F/Imported%20table?fields%5B%5D=Yliopisto&fields%5B%5D=URL&view=Grid%20view"
year_now <- year(Sys.Date())

# Fetch event page URL's
events <- list_event_pages()

# Tampere: date and time with RSelenium
# https://docs.ropensci.org/RSelenium/articles/basics.html#java-binary

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)
remDr$open()

# 1. Delete all old event records from the base (TO DO),
#    either via GUI or programmatically
# 2. Populate the base with new records generated below

print("Starting Aalto")
write_aalto_event_records()
print("Starting Hy")
write_hy_event_records()
print("Starting Oulu")
write_oulu_event_records()
print("Starting Tampere")
write_tampere_event_records()
print("Starting Turku")
write_turku_event_records()
print("Starting Jyu")
write_jyu_event_records()
print("Starting Lappi")
write_lappi_event_records()
print("Starting Åbo")
write_abo_event_records()
print("Starting Hanken")
write_hanken_event_records()
print("Starting UEF")
write_uef_event_records()
print("Starting Taide")
write_taide_event_records()
print("Starting LUT")
write_lut_event_records()

remDr$close()
# and ctrl+c in the command prompt

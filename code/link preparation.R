rm(list=ls())
library(dplyr)
library(rvest)
library(tidyverse)

# ambil dbase links TOKPED
link = readLines('Tokopedia Link.txt')
link = unique(link)
dummy = data.frame(id = c(1:length(link)),
                   url = link)

dummy = 
  dummy %>% 
  filter(!grepl('promo|search|iklan',url,ignore.case = T)) %>% 
  filter(grepl('nutri|luwak|kof|cof|whi|tora|top|good|god|caf|loka|kaf',url,ignore.case = T))

url = as.character(dummy$url)

save(url,file='all data tokped.rda')

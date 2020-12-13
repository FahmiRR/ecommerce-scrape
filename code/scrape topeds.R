rm(list=ls())
library(rvest)
library(dplyr)

# ambil dbase links 
load('all data tokped.rda')

scrap = function(url){
  data = 
    read_html(url) %>% {
      tibble(
        nama = html_nodes(.,'.css-x7lc0h') %>% html_text(),
        harga = html_nodes(.,".css-c820vl") %>% html_text(),
        seller = html_nodes(.,'.css-xmjuvc') %>% html_text(),
        terjual = html_nodes(.,'b') %>% html_text(),
        link = url
      )
    }
  return(data)
}

i = 1
data = scrap(url[i])
counter = 0

for(i in 2:length(url)){
  flag = TRUE
  while(flag){
    if(counter < 5) {
      tryCatch(
        {
          temp = scrap(url[i])
          data = rbind(data,temp)
          print(paste0('ambil data ke ',i,' done'))
          counter = 0
          flag = FALSE
        },error = function(e){
          counter=counter + 1
          assign("counter", counter, env=globalenv())
          Sys.sleep(1)
          print(counter)
        }
      )
    }
    else {
      print(paste0('ambil data ke ',i,' ERROR DAN DISKIP'))
      flag = FALSE
      counter = 0
    }
  }
}

data$waktu.scrape = Sys.time()
data = distinct(data)
data = data %>% mutate(Marketplace = "Tokopedia")
raw = rbind(raw,data)

save(list = c("url", "raw"), file = 'all data tokped.rda')


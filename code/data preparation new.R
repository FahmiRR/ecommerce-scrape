rm(list=ls())
library(tidyverse)
library(lubridate)

load('all data tokped.rda')

cleaned = 
  raw %>% 
  filter(!grepl('Stok|Tulis|Chat|Harga|tersisa',terjual, ignore.case = TRUE)) %>%
  mutate(kategori = case_when(
    grepl('Terjual', terjual) ~ "Jumlah Terjual",
    grepl('x', terjual) ~ "Jumlah Dilihat"
  )) %>% 
  spread(kategori, terjual) %>% 
  select(-9)

data =
  data %>% 
  filter(Marketplace != "Tokopedia")

url <- 
  url %>% 
  rename(
    nama = nama_item,
    seller = nama_toko
  )

data = merge(x = data, y = url[ , c("api", "seller", "nama")], by = c("nama", "seller"), all.x=TRUE) 
data = data[,c(1,3,2,8,4,7,5,6)]
data = 
  data %>% 
  rename(link = api)

cleaned = 
  rbind(cleaned, data)

cleaned_loka = 
  cleaned %>% 
  filter(grepl("loka", nama, ignore.case = TRUE))

cleaned =
  cleaned %>% 
  filter(!grepl("hilo|slim|tropicana|kompeni|hoodie|case|jaket
                |dalgona|woca|nescafe|sisters|sendok madu|alat|maker|bellman
                |ceret|stove|pembuat|fire|gelas|quality|grinder|steel|mesin|browny|biji|dress|satin|
                |kong|macehat|rogos|kucing|ecam|swing|sifon|drip|bean|tanpak|sidikalang|papua|vietnam|bandung|javaco|good deh
                |ariel|press|plunger|kapsul|miniatur|tempelan", nama, ignore.case = TRUE))

cleaned =
  rbind(cleaned, cleaned_loka)

cleaned$harga = as.integer(gsub('Rp(\\d+).(\\d+)','\\1\\2',cleaned$harga))
cleaned$`Jumlah Dilihat` = as.integer(gsub('x','',cleaned$`Jumlah Dilihat`))
cleaned$`Jumlah Terjual` = gsub('Terjual (\\d+) Produk','\\1\\2', cleaned$`Jumlah Terjual`)
cleaned$`Jumlah Terjual` = as.integer(gsub('Terjual (\\d+).(\\d+) Produk','\\1\\2', cleaned$`Jumlah Terjual`))

cleaned =
  cleaned %>% 
  group_by(Marketplace, seller, nama) %>% 
  arrange(Marketplace, seller, nama, waktu.scrape) %>% 
  mutate(waktu.scrape = format.POSIXct(as.POSIXct(waktu.scrape), "%Y-%m-%d") ) %>% 
  ungroup()

cleaned =
  cleaned %>% 
  filter(waktu.scrape > '2020-09-06') %>% 
  mutate(waktu.scrape = as.POSIXct(waktu.scrape))

#Mengisi Hari yang Kosong dan Isi dengan Hari yang sebelumnya
cleaned <-
  cleaned %>% 
  group_by(Marketplace, seller, nama) %>% 
  complete(waktu.scrape = seq.POSIXt(as.POSIXct("2020-09-07"), as.POSIXct("2020-10-07"), by = "day")) %>% 
  fill(harga, link, `Jumlah Dilihat`, `Jumlah Terjual`, .direction = "down") %>% 
  fill(harga, link, `Jumlah Dilihat`, `Jumlah Terjual`, .direction = "up")

cleaned <-
  cleaned %>% 
  group_by(Marketplace, seller, nama) %>%
  drop_na(`Jumlah Dilihat`, `Jumlah Terjual`)
  

for (i in 2:nrow(cleaned)) {
  if((cleaned$nama[i] == cleaned$nama[i-1]) & (cleaned$seller[i] == cleaned$seller[i-1]) & (cleaned$Marketplace[i] == cleaned$Marketplace[i-1])) {
    if((cleaned$`Jumlah Terjual`[i] - cleaned$`Jumlah Terjual`[i-1]) < 0) {
      cleaned$`Jumlah Terjual`[i] <- cleaned$`Jumlah Terjual`[i-1]
    }
    else (
      cleaned$`Jumlah Terjual`[i] <- cleaned$`Jumlah Terjual`[i]
    )
  }
  else (
    cleaned$`Jumlah Terjual`[i] <- cleaned$`Jumlah Terjual`[i]
  )
}

cleaned = 
  cleaned %>% 
  group_by(Marketplace, seller, nama) %>% 
  mutate(Sales = `Jumlah Terjual` - lag(`Jumlah Terjual`, default = `Jumlah Terjual`[1])) %>% 
  ungroup() 

cleaned = 
  cleaned %>% 
  mutate(skor = 
           ifelse(grepl('nutri|nuti',nama,ignore.case=T),1,0) + 
           ifelse(grepl('luwak',nama,ignore.case=T),1,0) +
           ifelse(grepl('torabika|tora bika',nama,ignore.case=T),1,0) +
           ifelse(grepl('caffi|cafi',nama,ignore.case=T),1,0) +
           ifelse(grepl('good',nama,ignore.case=T),1,0) +
           ifelse(grepl('loka',nama,ignore.case=T),1,0) +
           ifelse(grepl('top',nama,ignore.case=T),1,0) +
           ifelse(grepl('kapal',nama,ignore.case=T),1,0)
  ) %>% 
  mutate(Merk = case_when(
    grepl("nutri|nuti", nama, ignore.case = TRUE) & skor == 1 ~ "Nutrisari",
    grepl("luwak", nama, ignore.case = TRUE) & skor == 1 ~ "Luwak White Coffee",
    grepl("torabika|tora bika", nama, ignore.case = TRUE) & skor == 1 ~ "Torabika",
    grepl("caffi|cafi", nama, ignore.case = TRUE) & skor == 1 ~ "Caffino",
    grepl("good|day", nama, ignore.case = TRUE) & skor == 1 ~ "Good Day",
    grepl("loka", nama, ignore.case = TRUE) & skor == 1 ~ "Lokalate",
    grepl("top", nama, ignore.case = TRUE) & skor == 1 ~ "Top Coffee",
    grepl("kapal", nama, ignore.case = TRUE) & skor == 1 ~ "Kapal Api",
    skor > 1 ~ "Mixed",
    TRUE ~ "NA"
  )) %>% 
  mutate(Toko = case_when(
    grepl("nutrimart|mayora|kopiluwak|Kopi Luwak Official|kapal|wings", seller, ignore.case = TRUE) ~ "OS Brand",
    TRUE ~ "Lainnya"
  )) %>% 
  mutate(waktu.scrape = date(waktu.scrape)) %>% 
  mutate(sku = case_when(
    Merk == "Nutrisari" &harga < 5000 ~ "Satuan",
    Merk == "Nutrisari" & grepl("5", nama) & harga < 15000 ~ "5 Sachet",
    Merk == "Nutrisari" &!grepl("ml|dus", nama, ignore.case = TRUE) & harga > 9000 & harga < 30000 ~ "10 Sachet",
    Merk == "Nutrisari" &!grepl("500|750", nama) & harga > 40000 & harga < 60000 ~ "40 Sachet",
    TRUE ~ "Lainnya"
  )) %>% 
  mutate(sku = case_when(
    Merk == "Nutrisari" & harga < 5000 ~ "Satuan",
    Merk == "Nutrisari" & grepl("5", nama) & harga < 15000 ~ "5 Sachet",
    Merk == "Nutrisari" & !grepl("ml|dus", nama, ignore.case = TRUE) & harga > 9000 & harga < 30000 ~ "10 Sachet",
    Merk == "Nutrisari" & !grepl("500|750", nama) & harga > 40000 & harga < 60000 ~ "40 Sachet",
    Merk == "Luwak White Coffee" & harga < 2500 ~ "Satuan",
    Merk == "Luwak White Coffee" & harga >= 5000 & harga < 9000 & !grepl("botol|PACK|btl|265gr|220ml", nama, ignore.case = TRUE) ~ "5 Sachet",
    Merk == "Luwak White Coffee" & harga > 9000 & harga <= 17000 & !grepl("botol|PACK|btl|265gr|220ml|165gr|isi 6|5 x|8pcs|isi 5", nama, ignore.case = TRUE) ~ "10 Sachet",
    Merk == "Luwak White Coffee" & harga >= 44000 & harga < 53000 ~ "40 Sachet",
    Merk == "Torabika" & harga <= 2500 ~ "Satuan",
    Merk == "Torabika" & grepl("5 Sachet|6.5|6,5|5 Saset", nama, ignore.case = TRUE) ~ "5 Sachet",
    Merk == "Torabika" & harga >= 5000 & harga <= 8400 & !grepl("6.5|6,5|gilus|10", nama, ignore.case = TRUE) ~ "5 Sachet",
    Merk == "Torabika" & harga >= 8000 & harga < 15000 ~ "10 Sachet",
    Merk == "Torabika" & harga >= 8000 & harga <= 20000 & !grepl("gilus", nama, ignore.case = TRUE) ~ "10 Sachet",
    Merk == "Torabika" & harga >= 58800 & grepl("40", nama, ignore.case = TRUE)~ "40 Sachet",
    Merk == "Caffino" & harga < 2500 ~ "Satuan",
    Merk == "Caffino" & harga >= 5000 & harga <= 23500 & !grepl("5pcs|5 Sachet|6 pcs|pouch|5 x", nama, ignore.case = TRUE) ~ "10 Sachet",
    Merk == "Good Day" & harga <= 2750 ~ "Satuan",
    Merk == "Good Day" & grepl("5 sachet|5 s|5 x|5x|5pcs|x 5", nama, ignore.case = TRUE) ~ "5 Sachet",
    Merk == "Good Day" & harga >= 5000 & harga <= 9500 & !grepl("ml", nama, ignore.case = TRUE) ~ "5 Sachet",
    Merk == "Good Day" & grepl("10 sachet|10 s|10 x|10x|10pcs|x 10", nama, ignore.case = TRUE) ~ "10 Sachet",
    Merk == "Good Day" & harga > 9000 & harga <= 24000 & !grepl("freeze|5 sachet|5 s|5 x|5x|5pcs|box|ml", nama, ignore.case = TRUE) ~ "10 Sachet",
    Merk == "Lokalate" & harga < 3000 ~ "Satuan",
    Merk == "Lokalate" & harga >= 5000 & harga < 26000 ~ "10 Sachet",
    Merk == "Lokalate" & grepl("10", nama, ignore.case = TRUE) ~ "10 Sachet",
    Merk == "Lokalate" & harga >= 50000 & grepl("40", nama, ignore.case = TRUE) ~ "40 Sachet",
    Merk == "Top Coffee" & harga < 1300 ~ "Satuan",
    Merk == "Top Coffee" & harga >= 4400 & harga <= 7000 & grepl("x 5|5s|5sachet|5x", nama, ignore.case = TRUE) ~ "5 Sachet",
    Merk == "Top Coffee" & harga >= 5000 & harga <= 12000 & !grepl("11|12|165|9|6+3|158|bag|pouch|15|6", nama, ignore.case = TRUE) ~ "10 Sachet",
    Merk == "Kapal Api" & harga <= 3500 & !grepl("ml", nama, ignore.case = TRUE) ~ "Satuan",
    Merk == "Kapal Api" & harga > 3960 & harga <= 19000 & !grepl("box|dus|5x|20x|5s|185|165", nama, ignore.case = TRUE) ~ "10 Sachet",
    Merk == "Kapal Api" & harga > 3960 & harga <= 19000 & !grepl("185|165", nama, ignore.case = TRUE) ~ "5 Sachet",
    TRUE ~ "Lainnya"
  )) %>% 
  mutate(
    jenis_kopi = 
      case_when(
        Merk == "Caffino" ~ "3 in 1",
        Merk == "Good Day" ~ "3 in 1",
        Merk == "Luwak White Coffee" ~ "3 in 1",
        Merk == "Lokalate" ~ "3 in 1",
        Merk == "Torabika" & grepl("6.5|6,5|10", nama, ignore.case = T) ~ "Kopi Hitam",
        Merk == "Torabika" & grepl("choco|volcano|gilus|gillus|kafe|cafe|creamy|latte|moka|moca|capuccino|capucino|cappucino|cappuccino|capucinno|cupu|jahe|3in1|3 in 1|3inOne|diety|susu|aren", nama, ignore.case = T) ~ "3 in 1",
        Merk == "Torabika" & grepl("duo|tubruk", nama, ignore.case = T) ~ "2 in 1",
        Merk == "Top Coffee" ~ "3 in 1",
        Merk == "Top Coffee" & grepl("mini|murni", nama, ignore.case = T) ~ "Kopi Hitam",
        Merk == "Top Coffee" & grepl("toraja|robusta|arabica", nama, ignore.case = T) ~ "2 in 1",
        Merk == "Kapal Api" & grepl("spesial|special|merah|blue|blend|silver|hitam|165|380|signature|super|65|6.5|6,5|mini|tanpa", nama, ignore.case = T) ~ "Kopi Hitam",
        Merk == "Kapal Api" & grepl("spesial mix|special mix|mix|susu|2in1|2 in 1|mantap|gula", nama, ignore.case = T) ~ "2 in 1",
        Merk == "Kapal Api" & grepl("white|grande|3in1|3 in 1|kafe|fresco|mocha", nama, ignore.case = T) ~ "3 in 1",
        TRUE ~ "Lainnya"
      )
  ) %>% 
  mutate(
    rasa_kopi = 
      case_when(
        grepl("Cool|freez", nama, ignore.case = T) ~ "Coolin/Freeze Good Day",
        grepl("latte|classic", nama, ignore.case = T) ~ "Latte / Kopi Susu",
        grepl("mocca|moka|moca|mocha", nama, ignore.case = T) ~ "Mochaccino",
        grepl("cap", nama, ignore.case = T) ~ "Cappuccino",
        grepl("aren|gilus|loka", nama, ignore.case = T) ~ "Gula Aren",
        grepl("cara", nama, ignore.case = T) ~ "Caramel",
        grepl("coco|choco", nama, ignore.case = T) ~ "Choco",
        grepl("vani", nama, ignore.case = T) ~ "Vanilla",
        grepl("whi|grande", nama, ignore.case = T) ~ "White Coffee",
        grepl("java", nama, ignore.case = T) ~ "Latte / Kopi Susu",
        grepl("tira", nama, ignore.case = T) ~ "Tiramisu",
        grepl("nut", nama, ignore.case = T) ~ "Hazelnut",
        grepl("alpu|avo", nama, ignore.case = T) ~ "Alpukat",
        grepl("duri", nama, ignore.case = T) ~ "Durian",
        grepl("malak", nama, ignore.case = T) ~ "Tarik Malaka",
        grepl("less|sugar", nama, ignore.case = T) ~ "Less Sugar",
        grepl("pandan", nama, ignore.case = T) ~ "Pandan",
        grepl("Jahe", nama, ignore.case = T) ~ "Jahe",
        TRUE ~ "Latte / Kopi Susu"
      )
  )

cleaned$sku <- factor(cleaned$sku, levels = c("Satuan", "5 Sachet", "10 Sachet", "40 Sachet", "Lainnya"))
cleaned$jenis_kopi <- factor(cleaned$jenis_kopi, levels = c("Kopi Hitam", "2 in 1", "3 in 1", "Lainnya"))

save(cleaned, file = "cleaned data.rda")

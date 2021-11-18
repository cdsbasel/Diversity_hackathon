require(tidyverse)

pub_data <- read_csv("1_data/unibas_diversity.csv")

authors = str_split(pub_data$author, ';')
authors = sapply(authors, function(x) str_replace(x, ",$", ""))
firsts = sapply(authors, function(x) {
  sapply(str_split(x, ','), function(x)  {str_extract(str_trim(x[length(x)]), "[^[:blank:]]+")})
  })

first_tab = firsts %>% unlist() %>% table()
first_tab = first_tab[nchar(names(first_tab))>2]

top = first_tab %>% sort(decreasing = TRUE) %>% `[`(1:5000)

genders = read_csv("1_data/genders.csv")

gender_prob = genders %>% pull(gender, name)

top_prob = gender_prob[names(top)]
top_prob = top_prob[!is.na(top_prob)]

top = top[names(top_prob)]

cols = c("female" = "white", "male" = "#4ADABF")

cols = c("female" = "#4E4BD8", "male" = "#31424B")

png("3_figures/wordcloud.png",res = 300, width=20,height=8, units = "in")
par(mar=c(.2,.2,.2,0), mfrow=c(1, 2))
sel = which(top_prob == "female")[1:1000]
wordcloud::wordcloud(names(top[sel]), freq = (top[sel])**.4, scale = c(3, .5), min.freq = 0, 
                     fixed.asp = F, rot.per = 0, col = cols[top_prob[sel]], random.order = F)
par(mar=c(.2,0,.2,.2))
sel = which(top_prob != "female")[1:1000]
wordcloud::wordcloud(names(top[sel]), freq = (top[sel])**.4, scale = c(3, .5), min.freq = 0, 
                     fixed.asp = F, rot.per = 0, col = cols[top_prob[sel]], random.order = F)
print(par()$usr)
dev.off()


cols = c("female" = "#4ADABF", "male" = "#4E4BD8")

png("3_figures/wordcloud_2.png",res = 300, width=20,height=8, units = "in")
par(mar=c(.2,.2,.2,0), mfrow=c(1, 2))
sel = which(top_prob == "female")[1:1000]
wordcloud::wordcloud(names(top[sel]), freq = (top[sel])**.4, scale = c(3, .5), min.freq = 0, 
                     fixed.asp = F, rot.per = 0, col = cols[top_prob[sel]], random.order = F)
par(mar=c(.2,0,.2,.2))
sel = which(top_prob != "female")[1:1000]
wordcloud::wordcloud(names(top[sel]), freq = (top[sel])**.4, scale = c(3, .5), min.freq = 0, 
                     fixed.asp = F, rot.per = 0, col = cols[top_prob[sel]], random.order = F)
print(par()$usr)
dev.off()

cols = c("female" = "#4ADABF", "male" = "#4E4BD8")

png("3_figures/wordcloud_3.png",res = 300, width=20,height=8, units = "in")
par(mar=c(.2,.2,.2,0), mfrow=c(1, 2))
sel = which(top_prob != "female")[1:1000]
wordcloud::wordcloud(names(top[sel]), freq = (top[sel])**.4, scale = c(3, .5), min.freq = 0, 
                     fixed.asp = F, rot.per = 0, col = cols[top_prob[sel]], random.order = F)
par(mar=c(.2,0,.2,.2))
sel = which(top_prob == "female")[1:1000]
wordcloud::wordcloud(names(top[sel]), freq = (top[sel])**.4, scale = c(3, .5), min.freq = 0, 
                     fixed.asp = F, rot.per = 0, col = cols[top_prob[sel]], random.order = F)
print(par()$usr)
dev.off()

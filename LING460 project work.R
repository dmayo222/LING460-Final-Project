#Begining of Tyler's Work:
#I used rvest and stringr to scrape the html of the main body of each of the following articles
library("rvest")
library("stringr")
library(tibble)
library(tidytext)
library(dplyr)

#1st Jack Hughes Article
url <- "https://www.espn.com/nhl/story/_/id/48280406/nhl-2025-26-olympics-team-usa-jack-hughes-gold-medal-goal-puck-controversy"

webpage <- read_html(url)

web_data <- webpage %>%
  html_nodes("div.article-body")

print(web_data)

html_text2(web_data)

jack1 <- webpage %>%
  html_elements("div.article-body p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()
#2nd Jack Hughes Article
url1 <- "https://www.cbssports.com/olympics/news/jack-hughes-2026-olympics-golden-goal-hockey-hall-of-fame/"

webpage1 <- read_html(url1)

web_data1 <- webpage1 %>%
  html_nodes("div.Article-bodyContent")

print(web_data1)

html_text2(web_data1)

jack2 <- webpage1 %>%
  html_elements("div.Article-bodyContent p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(jack2)
#3rd Jack Hughes Article
url2 <- "https://www.nbcnews.com/sports/olympics/jack-hughes-devils-panthers-rcna261563"

webpage2 <- read_html(url2)

web_data2 <- webpage2 %>%
  html_nodes("div.article-body__content")

print(web_data2)

html_text2(web_data2)

jack3 <- webpage2 %>%
  html_elements("div.article-body__content p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(jack3)
#4th Jack Hughes Article
url3 <- "https://www.foxnews.com/sports/team-usas-jack-hughes-shares-patriotic-message-after-olympic-thriller"

webpage3 <- read_html(url3)

web_data3 <- webpage3 %>%
  html_nodes("div.article-body")

print(web_data3)

html_text2(web_data3)

jack4 <- webpage3 %>%
  html_elements("div.article-body p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(jack4)

#1st Hillary Knight Article
url4 <- "https://www.cbssports.com/olympics/news/usa-hockey-hilary-knight-president-donald-trump-joke-2026-olympics/"

webpage4 <- read_html(url4)

web_data4 <- webpage4 %>%
  html_nodes("div#Article-body.article.article-main-body.Article-body")

print(web_data4)

html_text2(web_data3)

hillary1 <- webpage4 %>%
  html_elements("div#Article-body.article.article-main-body.Article-body p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(hillary1)
#2nd Hillary
url5 <- "https://www.nbcchicago.com/olympics/2026-milan-cortina/hilary-knight-addresses-trumps-comment-about-womens-hockey-team-mens-response/3900508/"

webpage5 <- read_html(url5)

web_data5 <- webpage5 %>%
  html_nodes("div.article-content.rich-text")

print(web_data5)

html_text2(web_data5)

hillary2 <- webpage5 %>%
  html_elements("div.article-content.rich-text p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(hillary2)
#3rd Hillary
url6 <- "https://sports.yahoo.com/olympics/article/usa-womens-ice-hockey-captain-hilary-knight-reveals-she-played-2026-winter-olympics-with-torn-mcl-in-knee-212254458.html?guccounter=1"

webpage6 <- read_html(url6)

web_data6 <- webpage6 %>%
  html_nodes("div.body-wrapper")

print(web_data6)

html_text2(web_data6)

hillary3 <- webpage6 %>%
  html_elements("div.body-wrapper p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(hillary3)
#1st Henrik
url7 <- "https://www.nbcolympics.com/news/henrik-kristoffersen-wins-difficult-schladming-slalom-ahead-2026-winter-olympics"

webpage7 <- read_html(url7)

web_data7 <- webpage7 %>%
  html_nodes("div")

print(web_data7)

html_text2(web_data7)

henrik1 <- webpage7 %>%
  html_elements("div p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(henrik1)
#2nd henrik
url8 <- "https://www.espn.in/olympics/skiing/story/_/id/47758338/henrik-kristoffersen-ends-drought-wins-last-world-cup-slalom-olympics"

webpage8 <- read_html(url8)

web_data8 <- webpage8 %>%
  html_nodes("div.article-body")

print(web_data8)

html_text2(web_data8)

henrik2 <- webpage8 %>%
  html_elements("div.article-body p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(henrik2)
#3rd Henrik
url9 <- "https://sports.yahoo.com/articles/kristoffersen-wants-continue-skiing-until-124335987.html"

webpage9 <- read_html(url9)

web_data9 <- webpage9 %>%
  html_nodes("div.body-wrapper")

print(web_data9)

html_text2(web_data9)

henrik3 <- webpage9 %>%
  html_elements("div.body-wrapper p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()
print(henrik3)
#1st Mikaela
url10 <- "https://www.nbcolympics.com/news/mikaela-shiffrin-won-more-ways-one-2026-milan-cortina-winter-olympics"

webpage10 <- read_html(url10)

web_data10 <- webpage10 %>%
  html_nodes("div")

print(web_data10)

html_text2(web_data10)

mikaela1 <- webpage10 %>%
  html_elements("div p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(mikaela1)
#2nd Mikaela
url11 <- "https://www.cnn.com/2026/02/18/sport/mikaela-shiffrin-olympics-golden-peace"

webpage11 <- read_html(url11)

web_data11 <- webpage11 %>%
  html_nodes("div.article__content")

print(web_data11)

html_text2(web_data11)

mikaela2 <- webpage11 %>%
  html_elements("div.article__content p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(mikaela2)
#1st Jake
url12 <- "https://www.nbcchicago.com/olympics/2026-milan-cortina/for-snowboarder-jake-canter-an-olympic-medal-is-the-prize-after-near-death-journey/3896226/"

webpage12 <- read_html(url12)

web_data12 <- webpage12 %>%
  html_nodes("div.article-content.rich-text")

print(web_data12)

html_text2(web_data12)

jake1 <- webpage12 %>%
  html_elements("div.article-content.rich-text p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(jake1)
#2nd jake
url13 <- "https://www.themanual.com/outdoors/jake-canter-interview/"

webpage13 <- read_html(url13)

web_data13 <- webpage13 %>%
  html_nodes("div#h-maincontent.b-page")

print(web_data13)

html_text2(web_data13)

jake2 <- webpage13 %>%
  html_elements("div#h-maincontent.b-page p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()

print(jake2)
#3rd Jake
url14 <- "https://www.summitdaily.com/news/olympic-snowboarding-slopestyle-bronze-colorado-canter-gerard-silverthorne-2026/"

webpage14 <- read_html(url14)

web_data14 <- webpage14 %>%
  html_nodes("div.container")

print(web_data14)

html_text2(web_data14)

jake3 <- webpage14 %>%
  html_elements("div.container p") %>%
  html_text2() %>%
  paste(collapse = " ") %>%
  str_squish()
print(jake3)
#End of Tyler's work

#Beginning of Dante's Work:
#First Ilia Malinin Article
page1_malinin <- read_html("https://www.espn.com/olympics/figureskating/story/_/id/47922785/quad-god-ilia-malinin-falls-twice-olympic-men-figure-skating-disaster-allowing-mikhail-shaidorov-claim-gold")
paras1_malinin <- page |>
  html_elements("article p") |>
  html_text2()|>
  paste(collapse = " ") |>
  str_squish()

#Second Ilia Malinin Article
page2_malinin <- read_html("https://www.espn.com/olympics/figureskating/story/_/id/47890597/us-star-ilia-malinin-leads-men-figure-skating-olympics")
paras2_malinin <- page2_malinin |>
  html_elements("article p") |>
  html_text2() |>
  paste(collapse = " ") |>
  str_squish()

#Third Ilia Malinin Article
page3_malinin <- read_html("https://www.nbcnews.com/sports/olympics/ilia-malinin-admits-not-ready-handle-olympic-pressure-rcna259316")

paras3_malinin <- page3_malinin |>
  html_elements("article p") |>
  html_text2()
paras3_malinin <- paras3_malinin[-13] #Removing advertisements/unnecessary text
paras3_malinin <- paras3_malinin |>
  paste(collapse = " ") |>
  str_squish()


#First Alyssa Liu Article
page1_liu <- read_html("https://www.si.com/winter-olympics/alysa-liu-radiates-joy-as-figure-skating-champion-gold")

paras1_liu <- page1_liu |>
  html_elements("article p") |>
  html_text2()
paras1_liu <- paras1_liu[-26] #Removing advertisements/unnecessary text
paras1_liu <- paras1_liu |>
  paste(collapse = " ") |>
  str_squish()

#Second Alyssa Liu Article
page2_liu <- read_html("https://www.cbssports.com/olympics/news/2026-winter-olympics-american-figure-skater-alysa-liu/")

paras2_liu <- page2_liu |>
  html_elements("article p") |>
  html_text2()|>
  paste(collapse = " ") |>
  str_squish()

#Third Alyssa Liu Article
page3_liu <- read_html("https://www.elitedaily.com/wellness/alysa-liu-2026-olympics-figure-skating-interview")

paras3_liu <- page3_liu |>
  html_elements("article p") |>
  html_text2()
paras3_liu <- paras3_liu[-c(21,22)] #Removing advertisements/unnecessary text
paras3_liu <- paras3_liu |>
  paste(collapse = " ") |>
  str_squish()


#First Chloe Kim Article
page1_kim <- read_html("https://www.cbsnews.com/news/chloe-kim-injury-snowboarding-2026-winter-olympics-italy/")

paras1_kim <- page1_kim |>
  html_elements("article p") |>
  html_text2()
paras1_kim <- paras1_kim[-c(1,19,20)]  #Removing advertisements/unnecessary text

paras1_kim <- paras1_kim |>
  paste(collapse = " ") |>
  str_squish()

#Second Chloe Kim Article
page2_kim <- read_html("https://www.nbcolympics.com/news/chloe-kim-unfazed-injury-crushes-halfpipe-qualifier-roar-final-mastro-3rd")

paras2_kim <- page2_kim |>
  html_elements("p") |>
  html_text2()
paras2_kim <- paras2_kim[-c(17:21)]  #Removing advertisements/unnecessary text

paras2_kim <- paras2_kim |>
  paste(collapse = " ") |>
  str_squish()

#First Jordan Stolz Article
page1_stolz <- read_html("https://www.nbcolympics.com/news/amid-dramatic-olympics-jordan-stolz-quietly-becomes-us-star-and-hes-just-getting-started")

paras1_stolz <- page1_stolz |>
  html_elements("p") |>
  html_text2()
paras1_stolz <- paras1_stolz[-c(14:18)] #Removing advertisements/unnecessary text

paras1_stolz <- paras1_stolz |>
  paste(collapse = " ") |>
  str_squish()
#Second Jordan Stolz Article
page2_stolz <- read_html("https://www.nytimes.com/athletic/7060476/2026/02/21/jordan-stolz-milan-olympics-speedskating-fame-three-medals/")

paras2_stolz <- page2_stolz |>
  html_elements("p") |>
  html_text2()
paras2_stolz <- paras2_stolz[-c(31:34)]  #Removing advertisements/unnecessary text

paras2_stolz <- paras2_stolz |>
  paste(collapse = " ") |>
  str_squish()

#Third Jordan Stolz Article
page3_stolz <- read_html("https://www.espn.com/olympics/speedskating/story/_/id/47928388/american-jordan-stolz-wins-2nd-speedskating-gold-olympics")

paras3_stolz <- page3_stolz |>
  html_elements("article p") |>
  html_text2() |>
  paste(collapse = " ") |>
  str_squish()


#First Jutta Leerdam Article
page1_leerdam <- read_html("https://edition.cnn.com/2026/02/10/sport/jutta-leerdam-dutch-speed-skater-olympics", encoding = "UTF-8")

paras1_leerdam <- page1_leerdam |>
  html_elements("p") |>
  html_text2()
paras1_leerdam <- paras1_leerdam[-c(22:24)]  #Removing advertisements/unnecessary text
paras1_leerdam <- paras1_leerdam |>
  paste(collapse = " ") |>
  str_squish()

#Second Jutta Leerdam Article
page2_leerdam <- read_html("https://sports.yahoo.com/articles/watch-jutta-leerdam-react-fianc-140000105.html")

paras2_leerdam <- page2_leerdam |>
  html_elements("p") |>
  html_text2()
paras2_leerdam <- paras2_leerdam[-c(1,12,13,14)] #Removing advertisements/unnecessary text

paras2_leerdam <- paras2_leerdam |>
  paste(collapse = " ") |>
  str_squish()

#Third Jutta Leerdam Article
page3_leerdam <- read_html("https://www.nytimes.com/athletic/7033765/2026/02/09/jutta-leerdam-olympic-speed-skating-record-1000-meter/")

paras3_leerdam <- page3_leerdam |>
  html_elements("p") |>
  html_text2()
paras3_leerdam <- paras3_leerdam[-c(4,13,22)]  #Removing advertisements/unnecessary text
paras3_leerdam <- paras3_leerdam |>
  paste(collapse = " ") |>
  str_squish()
jack_corpus <- tibble(
  athlete = "Jack Hughes",
  article = c("article1", "article2", "article3", "article4"),
  text = c(jack1, jack2, jack3, jack4)
)
hilary_corpus <- tibble(
  athlete = "Hilary Knight",
  article = c("article1", "article2", "article3"),
  text = c(hillary1, hillary2, hillary3)
)
henrik_corpus <- tibble(
  athlete = "Henrik Kristoffersen",
  article = c("article1", "article2", "article3"),
  text = c(henrik1, henrik2, henrik3)
)
mikaela_corpus <- tibble(
  athlete = "Mikaela Shiffrin",
  article = c("article1", "article2"),
  text = c(mikaela1, mikaela2)
)
jake_corpus <- tibble(
  athlete = "Jake Canter",
  article = c("article1", "article2", "article3"),
  text = c(jake1, jake2, jake3)
)
malinin_corpus <- tibble(
  athlete = "Ilia Malinin",
  article = c("article1", "article2", "article3"),
  text = c(paras1_malinin, paras2_malinin, paras3_malinin)
)
liu_corpus <- tibble(
  athlete = "Alyssa Liu",
  article = c("article1", "article2", "article3"),
  text = c(paras1_liu, paras2_liu, paras3_liu)
)
kim_corpus <- tibble(
  athlete = "Chloe Kim",
  article = c("article1", "article2"),
  text = c(paras1_kim, paras2_kim)
)
stolz_corpus <- tibble(
  athlete = "Jordan Stolz",
  article = c("article1", "article2", "article3"),
  text = c(paras1_stolz, paras2_stolz, paras3_stolz)
)
jutta_corpus <- tibble(
  athlete = "Jutta Leerdam",
  article = c("article1", "article2", "article3"),
  text = c(paras1_leerdam, paras2_leerdam, paras3_leerdam)
)
all_articles <- jack_corpus
all_articles <- bind_rows(all_articles, 
                          hilary_corpus,
                          malinin_corpus,
                          liu_corpus,
                          henrik_corpus,
                          mikaela_corpus,
                          jake_corpus,
                          kim_corpus,
                          stolz_corpus,
                          jutta_corpus)

all_collapsed <- all_articles %>%
  group_by(athlete) %>%
  summarise(
    text = paste(text, collapse = " "),
    .groups = "drop"
  )
#End of Dante's Work

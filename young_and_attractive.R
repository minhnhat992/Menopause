library(rvest)
library(httr)
library(plyr)
library(dplyr)
library(tidyr)
library(ReporteRs)

# get the login url
url2 <- "http://forum.menopausechitchat.com/main/authorization/signIn?target=http%3A%2F%2Fforum.menopausechitchat.com%2F"

# Initiate session using rvest

session <- html_session(url = url2)

# initiate login, input email and password

login <- session %>% 
  html_node(xpath = '//*[@id="xg_body"]/div/div/form') %>% 
  html_form() %>% 
  set_values(
    emailAddress = 'lisakurnit1970@gmail.com',
    password = 'R3df!!sh'
  )

#submit login
logged_in <- session %>% 
  submit_form(login)

# test jumpt to 1 disscussion
test <- logged_in %>% 
  jump_to('http://forum.menopausechitchat.com/forum/topics/young-and-attractive')

topic <- test %>% 
  html_nodes(xpath = '//*[@id="xg_body"]/div[1]/div[1]/div[1]/div[2]/h1') %>% 
  html_text() %>% 
  as.character()

content <- test %>% 
  html_nodes(xpath = '//*[@id="xg_body"]/div[1]/div[1]/div[2]/div[1]/div/div') %>% 
  html_text() %>% 
  as.character()

# test create a word file
mydoc <- docx()

mydoc <- addTitle(mydoc, topic, level = 1) %>% 
  addParagraph(content) 

writeDoc(mydoc, file = "E:/R/Menopause/test.docx")

testing <- test %>% 
  html_nodes(xpath = '//*[@id="discussionReplies"]/div[1]') %>% 
  html_children()



substr(as.character(html_children(testing[2])[1] %>% 
                      html_text()),
       29,1000)


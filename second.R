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
  jump_to('http://forum.menopausechitchat.com/forum/topics/anxiety-2?id=3579759%3ATopic%3A34034&page=1#comments')

# Topic and main content name never changes
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
  addParagraph(content) %>% 
  addTitle("Replies")

# set paragraph properties

setting  <- parProperties(padding.bottom = 40)

# Get the replies
for (n in 1 :200){
  
  assign(paste("page_",n,sep = ""),
         logged_in %>% 
           jump_to(paste('http://forum.menopausechitchat.com/forum/topics/anxiety-2?id=3579759%3ATopic%3A34034&page=',n,'#comments',sep="")))
  
  testing <- get(paste("page_",n,sep = "")) %>% 
    html_nodes(xpath = '//*[@id="discussionReplies"]/div[1]') %>% 
    html_children()
  
  # Add comment:
  
  for (i in 2:13){
    assign(paste("comment_",n,"_",i,sep=""),
           html_children(html_children(html_children(testing[i])[2])[1]) %>% 
             html_text())
    save(list = paste("comment_",n,"_",i,sep=""), file = paste("comment_",n,"_",i,sep=""))
    
    mydoc <- addParagraph(mydoc, get(paste("comment_",n,"_",i,sep="")), par.properties = setting)
    
    print(paste(paste("comment_",n,"_",i,sep=""),"has been added",sep=" "))
    
    rm(list = paste("comment_",n,"_",i,sep=""))

  }
  
  
}


# loop to add comment


#writeDoc(mydoc, file = "E:/R/Menopause/topics/Menopause_symptomps_whats_your_story.docx")


writeDoc(mydoc, file = paste("E:/R/Menopause/topics/",topic,".docx",sep=""))

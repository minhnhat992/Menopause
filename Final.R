library(rvest)
library(httr)
library(plyr)
library(dplyr)
library(tidyr)
library(ReporteRs)

# the script is made to scrape data from forum.menopausechitchat.com. The process is simple: write a loop that jump from page to page,
# thread to thread and get each comments from each topics. The raw texts are then put into word documents.




# get the login url ------------
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

# Loop ----------------

# Create empty document
mydoc <- docx()

# jump to the first site that is sorted by most recent (there are 207 sites)
for (m in 1 : 210)
{
  
  diss <- logged_in %>% 
    jump_to(paste('http://forum.menopausechitchat.com/forum?sort=mostRecentDiscussions&page=',m,sep=""))
  
  # jump to a thread in a site (9 threads/site)
  
  for (h in 1: 9) 
  { 
    if(m == 1){
      link <- diss %>% 
        html_nodes(xpath=paste('//*[@id="xg_body"]/div[1]/div[2]/div[4]/table/tbody/tr[',h,']/td[1]/h3/a',sep="")) %>% 
        html_attr("href")
    } else {
      
      link <- diss %>% 
        html_nodes(xpath=paste('//*[@id="xg_body"]/div[1]/div[2]/div[2]/table/tbody/tr[',h,']/td[1]/h3/a',sep="")) %>% 
        html_attr("href")
    }
    
    print(link)
    
   
    test <- logged_in %>% 
      jump_to(paste(link,'?page=1#comments',sep=""))
    
    # Topic and main content name never changes
    
    ## Get Topic Name
    
    assign(paste("topic_",h,sep = ""),
           test %>% 
             html_nodes(xpath = '//*[@id="xg_body"]/div[1]/div[1]/div[1]/div[2]/h1') %>% 
             html_text() %>% 
             as.character())
       
    ## Get who posted first
    assign(paste("OP_",h,sep = ""),
           test %>% 
             html_nodes(xpath = '//*[@id="xg_body"]/div[1]/div[1]/div[1]/div[2]/ul/li[1]') %>% 
             html_text())
    
    ## Get Content of Original Post
    
    assign(paste("content_",h,sep=""),
           test %>%
             html_nodes(xpath = '//*[@id="xg_body"]/div[1]/div[1]/div[2]/div[1]/div/div') %>%
             html_text() %>%
             as.character())
    
    # set paragraph properties
    
    setting  <- parProperties(padding.bottom = 20)
    
    OP <- pot(get(paste("OP_",h,sep="")), textBold())
    
    content <- pot(get(paste("content_",h,sep="")))
    

    mydoc <- addTitle(mydoc, get(paste("topic_",h,sep = "")), level = 1) %>% 
      addTitle(value = "Main", level = 2) %>% 
      addParagraph(set_of_paragraphs(OP,content), par.properties = setting) %>% 
      addTitle("Replies", level = 2)
    
    # set paragraph properties
    
    
    # find out the last page in that thread to put into loop
    last_page <- test %>% 
      html_node(xpath='//*[@id="discussionReplies"]/div[1]/ul') %>% 
      html_children() %>% 
      tail(n=1) %>% 
      html_children() %>% 
      html_attr("_maxpage") %>% 
      as.numeric()
    
    # if thread only has 1 page
    if (length(last_page) == 0){
      
      testing <- test %>% 
        html_nodes(xpath = '//*[@id="discussionReplies"]/div[1]') %>% 
        html_children()
      
      # Add comment in one page
      tryCatch({
        for (i in 2:13){
          assign(paste("commenter_",1,"_",i,sep=""),
                 substr(as.character(html_children(testing[2])[1] %>% 
                                       html_text()),
                        29,1000))
          
          assign(paste("comment_",1,"_",i,sep=""),
                 html_children(html_children(html_children(testing[i])[2])[1]) %>% 
                   html_text())
          
          full_comment <- pot(get(paste("commenter_",1,"_",i,sep="")), textBold()) + " " + pot(get(paste("comment_",1,"_",i,sep="")))
          
          mydoc <- addParagraph(mydoc, full_comment, par.properties = setting)
          
          print(paste(paste("comment_",1,"_",i,sep=""),"in topic",get(paste("topic_",h,sep = "")),"has been added",sep=" "))
          
          rm(list = paste("comment_",1,"_",i,sep=""))
          
          rm(list = paste("commenter_",1,"_",i,sep=""))
          
        }}, error=function(e){})
      
    } else {
      # go through pages in a discussion - if thread has more than 1 pages
      for (n in 1 : last_page){
        
        assign(paste("page_",n,sep = ""),
               logged_in %>% 
                 jump_to(paste(link,'?page=',n,'#comments',sep="")))
        
        testing <- get(paste("page_",n,sep = "")) %>% 
          html_nodes(xpath = '//*[@id="discussionReplies"]/div[1]') %>% 
          html_children()
        
        # Add comment in one page
        tryCatch({
          for (i in 2:13){
            
            assign(paste("commenter_",n,"_",i,sep=""),
                   substr(as.character(html_children(testing[2])[1] %>% 
                                         html_text()),
                          29,1000))
            
            assign(paste("comment_",n,"_",i,sep=""),
                   html_children(html_children(html_children(testing[i])[2])[1]) %>% 
                     html_text())
            full_comment <- pot(get(paste("commenter_",n,"_",i,sep="")), textBold()) + " " + pot(get(paste("comment_",n,"_",i,sep="")))
            
            mydoc <- addParagraph(mydoc, full_comment, par.properties = setting)
            
            print(paste(paste("comment_",n,"_",i,sep=""),"in topic",get(paste("topic_",h,sep = "")),"has been added",sep=" "))
            
            rm(list = paste("comment_",n,"_",i,sep=""))
            
            rm(list = paste("commenter_",n,"_",i,sep=""))
            
          }}, error=function(e){})
        
        
      }}

  }
  
  
  tryCatch(
    {
      # For each 30 scraped sites, raw text files will be saved into a word document.
      if (m %in% seq(from = 30, to = 210, by = 30) ) 
        {
        writeDoc(mydoc, file = paste(m,".docx", sep = ""))
        
        print(paste(m,"sites have been put into a word file", sep = " "))
        }
    }, 
    
    error = function(e){"emoticon topic"}
  )
  
  print(paste("site",m,"has been scraped",sep = " "))
  
}





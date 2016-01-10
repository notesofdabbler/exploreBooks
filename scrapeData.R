

# load libraries
library(rvest)
library(dplyr)
library(stringr)

# link to Use R! titles at Springer site

useRlink = "http://www.springer.com/new+%26+forthcoming+titles+%28default%29?SGWID=4-40356-404-173624787-6991"

userPg = useRlink %>% read_html()

userPgLinks = userPg %>% html_nodes(".pager a") %>% html_attr("href")
userPgLinks = unique(userPgLinks)
userPgLinks

# how to print the page?

booklistL = list()

for(i in 1:length(userPgLinks)){
    
    print(i)
    userPg = userPgLinks[i] %>% read_html()
    # little trial error involved, for example just using img gets couple of irrelevant items
    # so need to go up one level and use class = productGraphic tag also for extraction
    booktitles = userPg %>% html_nodes(".productGraphic img") %>% html_attr("alt")
    booklinklist = userPg %>% html_nodes(".productGraphic a") %>% html_attr("href")
    idlist = seq(id+1,id+length(booktitles))
    booklistL[[i]] = data.frame(id = idlist, title = booktitles, booklink = booklinklist)
}

booklistdf = bind_rows(booklistL)
booklistdf$id = seq(1,nrow(booklistdf))

# Get details for a single book

booklink = "http://www.springer.com/us/book/9783319242750"


getbookInfo(booklink)



# load libraries
library(rvest)
library(dplyr)
library(stringr)

# source functions
source("scrapeFns.R")

# link to Use R! titles at Springer site

srchR_link = "http://www.springer.com/generic/search/results?SGWID=4-40109-24-653415-0&sortOrder=relevance&searchType=ADVANCED_CDA&title=R&language=en&searchScope=editions&queryText=R&resultStart=xx&media=book"
srchUseR_link = "http://www.springer.com/generic/search/results?SGWID=4-40109-24-653415-0&series=Use+R&sortOrder=relevance&searchType=ADVANCED_CDA&searchScope=editions&queryText=Use+R&resultStart=xx&media=book"

srchR_numpgs = 15
srchUseR_numpgs = 6

srchR_seq = c(1,seq(1,srchR_numpgs-1)*10 + 1)
srchR_seq

srchUseR_seq = c(1,seq(1,srchUseR_numpgs-1)*10 + 1)
srchUseR_seq


srchR_PgLinks = sapply(srchR_seq,function(x) gsub("xx",x,srchR_link))
srchR_PgLinks

srchUseR_PgLinks = sapply(srchUseR_seq,function(x) gsub("xx",x,srchUseR_link))
srchUseR_PgLinks

PgLinks = c(srchR_PgLinks,srchUseR_PgLinks)

# how to print the page?

booklistL = list()

for(i in 1:length(PgLinks)){
    
    print(i)
    Pg = PgLinks[i] %>% read_html()
    # little trial error involved, for example just using img gets couple of irrelevant items
    # so need to go up one level and use class = productGraphic tag also for extraction
    booktitles = Pg %>% html_nodes(".productGraphic img") %>% html_attr("alt")
    booklinklist = Pg %>% html_nodes(".productGraphic a") %>% html_attr("href")
    booklistL[[i]] = data.frame(title = booktitles, booklink = booklinklist)
}

booklistdf = bind_rows(booklistL)
# Remove duplicates
dupbooks = duplicated(booklistdf$booklink)
booklistdf = booklistdf[!dupbooks,]

booklistdf$id = seq(1,nrow(booklistdf))

# Get details for a single book

bookinfoL = list()
recobookinfoL = list()
for(i in 1:nrow(booklistdf)){
    print(i)
    bookInfo = getbookInfo(booklistdf$booklink[i])
    bookinfoL[[i]] = bookInfo$bookinfo
    bookinfoL[[i]]$id = booklistdf$id[i]
    recobookinfoL[[i]] = bookInfo$recobookinfo
    recobookinfoL[[i]]$id = booklistdf$id[i]
}

bookinfodf = bind_rows(bookinfoL)
recobookinfodf = bind_rows(recobookinfoL)

save(bookinfodf,file = "bookinfodf.Rda")
save(recobookinfodf,file = "recobookinfodf.Rda")

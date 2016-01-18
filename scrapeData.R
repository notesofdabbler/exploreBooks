

# 
# Get list of books in Springer related to R
# Got the list by doing advanced search for R in title or Use R in series
#

# load libraries
library(rvest)
library(dplyr)
library(stringr)

# source function to get details for a single book
source("scrapeFns.R")

# link to Use R! titles at Springer site

srchR_link = "http://www.springer.com/generic/search/results?SGWID=4-40109-24-653415-0&sortOrder=relevance&searchType=ADVANCED_CDA&title=R&language=en&searchScope=editions&queryText=R&resultStart=xx&media=book"
srchUseR_link = "http://www.springer.com/generic/search/results?SGWID=4-40109-24-653415-0&series=Use+R&sortOrder=relevance&searchType=ADVANCED_CDA&searchScope=editions&queryText=Use+R&resultStart=xx&media=book"

# number of pages of results
srchR_numpgs = 15 # search for R in title (there are more than 15 but restricted to 15 here)
srchUseR_numpgs = 6 # search for Use R in series

# sequence to use in link for accessing different pages of search results
srchR_seq = c(1,seq(1,srchR_numpgs-1)*10 + 1)  # search for R in title
srchR_seq

srchUseR_seq = c(1,seq(1,srchUseR_numpgs-1)*10 + 1) # search for Use R in series
srchUseR_seq


srchR_PgLinks = sapply(srchR_seq,function(x) gsub("xx",x,srchR_link))
srchR_PgLinks

srchUseR_PgLinks = sapply(srchUseR_seq,function(x) gsub("xx",x,srchUseR_link))
srchUseR_PgLinks

# Combine links from both searches
# Note that it is possible that same book could appear in both searches. We will dedup it later
PgLinks = c(srchR_PgLinks,srchUseR_PgLinks)

booklistL = list()

for(i in 1:length(PgLinks)){
    
    print(i)
    Pg = PgLinks[i] %>% read_html()

    booktitles = Pg %>% html_nodes(".productGraphic img") %>% html_attr("alt")
    booklinklist = Pg %>% html_nodes(".productGraphic a") %>% html_attr("href")
    booklistL[[i]] = data.frame(title = booktitles, booklink = booklinklist)

}

booklistdf = bind_rows(booklistL)
# Remove duplicates
dupbooks = duplicated(booklistdf$booklink)
booklistdf = booklistdf[!dupbooks,]

booklistdf$id = seq(1,nrow(booklistdf))

# Get details for each book

bookinfoL = list()
recobookinfoL = list()
for(i in 1:nrow(booklistdf)){
    print(i)
    bookInfo = getbookInfo(booklistdf$booklink[i])
    # info about book
    bookinfoL[[i]] = bookInfo$bookinfo
    bookinfoL[[i]]$id = booklistdf$id[i]
    # info about recommended books
    recobookinfoL[[i]] = bookInfo$recobookinfo
    recobookinfoL[[i]]$id = booklistdf$id[i]
}

bookinfodf = bind_rows(bookinfoL)
recobookinfodf = bind_rows(recobookinfoL)

# save book info to R datasets
save(bookinfodf,file = "bookinfodf.Rda")
save(recobookinfodf,file = "recobookinfodf.Rda")

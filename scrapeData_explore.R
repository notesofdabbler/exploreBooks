

# load libraries
library(rvest)
library(dplyr)
library(stringr)

# link to Use R! titles at Springer site
useRlink = "http://www.springer.com/?SGWID=0-102-24-0-0&series=Use+R&sortOrder=relevance&searchType=ADVANCED_CDA&searchScope=editions&queryText=Use+R"

# Read the page
userPg = useRlink %>% read_html()

## Get info of books displayed on the page

# title
booktitles = userPg %>% html_nodes(".productGraphic img") %>% html_attr("alt")
booktitles
# link to book details
booklinklist = userPg %>% html_nodes(".productGraphic a") %>% html_attr("href")
booklinklist
# book publication year
bookyr = userPg %>% html_nodes(xpath = "//span[contains(@class,'renditionDescription')]") %>% html_text()
bookyr
# book author
bookauth = userPg %>% html_nodes("span[class = 'displayBlock']") %>% html_text()
# bookauth = userPg %>% html_nodes(xpath = "//span[@class = 'displayBlock']") %>% html_text()  ## will give same result
bookauth
# book price
bookprice = userPg %>% html_nodes(xpath = "//div[@class = 'bookListPriceContainer']//span[2]") %>% html_text()
bookprice

pgdf = data.frame(title = booktitles, link = booklinklist, pubyr = bookyr,
                  auth = bookauth, price = bookprice)
pgdf

##  Get details for a single book 

booklink = "http://www.springer.com/us/book/9781461478997"
bookpg = booklink %>% read_html()

# Get title
booktitle = bookpg %>% html_nodes(".cms-col dd[itemprop = 'name']") %>% html_text()
#bookpg %>% html_nodes(xpath = "//dl[@class = 'cms-col']//dd[@itemprop = 'name']") %>% html_text()
booktitle

# Get authors
bookauth = bookpg %>% html_nodes(".cms-col li[itemprop = 'author']") %>% html_text()
bookauth

# Get price currency and price
priceCurr = bookpg %>% html_nodes("meta[itemprop = 'priceCurrency']") %>% html_attr("content")
priceCurr
price = bookpg %>% html_nodes("meta[itemprop = 'price']") %>% html_attr("content")
price

# Get citations and downloads
metricsurl = bookpg %>% html_nodes(".book-metrics") %>% html_attr("data-bookmetrix-ajax-url")
metricsurl = paste0("http://www.springer.com",metricsurl)
metricsurl

metricspg = metricsurl %>% read_html()
citations = metricspg %>% html_nodes(".citations .book-metric-item-value") %>% html_text() 
citations
downloads = metricspg %>% html_nodes(".downloads .book-metric-item-value") %>% html_text()
downloads

# Get recommended books
recomurl = bookpg %>% html_nodes(".product-recommendation") %>% html_attr("data-baynote-service-url")
recomurl = paste0("http://www.springer.com",recomurl)
recomurl

recompg = recomurl %>% read_html()
recobooks = recompg %>% html_nodes(".product-information p a") %>% html_text()
recobooks

# Analysis done with RStudio 0.99.489
sessionInfo()


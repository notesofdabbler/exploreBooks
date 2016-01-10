

# load libraries
library(rvest)
library(dplyr)
library(stringr)

# link to Use R! titles at Springer site

useRlink = "http://www.springer.com/new+%26+forthcoming+titles+%28default%29?SGWID=4-40356-404-173624787-6991"

userPg = useRlink %>% read_html()

# how to print the page?

# little trial error involved, for example just using img gets couple of irrelevant items
# so need to go up one level and use class = productGraphic tag also for extraction
booktitles = userPg %>% html_nodes(".productGraphic img") %>% html_attr("alt")
booklinklist = userPg %>% html_nodes(".productGraphic a") %>% html_attr("href")

# Get details for a single book

booklink = "http://www.springer.com/us/book/9781461478997"
bookpg = booklink %>% read_html()

# Get title
bookpg %>% html_nodes(".cms-col dd[itemprop = 'name']") %>% html_text()
#bookpg %>% html_nodes(xpath = "//dl[@class = 'cms-col']//dd[@itemprop = 'name']") %>% html_text()

# Get authors
bookpg %>% html_nodes(".cms-col li[itemprop = 'author']") %>% html_text()

# Get price currency and price
bookpg %>% html_nodes("meta[itemprop = 'priceCurrency']") %>% html_attr("content")
bookpg %>% html_nodes("meta[itemprop = 'price']") %>% html_attr("content")

# Get citations and downloads
metricsurl = bookpg %>% html_nodes(".book-metrics") %>% html_attr("data-bookmetrix-ajax-url")
metricsurl = paste0("http://www.springer.com",metricsurl)

metricspg = metricsurl %>% read_html()
citations = metricspg %>% html_nodes(".citations .book-metric-item-value") %>% html_text() 
downloads = metricspg %>% html_nodes(".downloads .book-metric-item-value") %>% html_text()

# Get recommended books
recomurl = bookpg %>% html_nodes(".product-recommendation") %>% html_attr("data-baynote-service-url")
recomurl = paste0("http://www.springer.com",recomurl)

recompg = recomurl %>% read_html()
recobooks = recompg %>% html_nodes(".product-information p a") %>% html_text()

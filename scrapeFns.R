# Get info for a single book

getbookInfo = function(booklink){
  
    # Get book page
    bookpg = booklink %>% read_html()
  
    # Get title
    title = bookpg %>% html_nodes(".cms-col dd[itemprop = 'name']") %>% html_text()
    #bookpg %>% html_nodes(xpath = "//dl[@class = 'cms-col']//dd[@itemprop = 'name']") %>% html_text()
  
    # Get authors
    authors = bookpg %>% html_nodes(".cms-col li[itemprop = 'author']") %>% html_text()
    authors = str_trim(authors)
    authors = paste(authors,collapse = ";")
  
    # Get price currency and price
    currency = bookpg %>% html_nodes("meta[itemprop = 'priceCurrency']") %>% html_attr("content")
    price = bookpg %>% html_nodes("meta[itemprop = 'price']") %>% html_attr("content")
    price = as.numeric(price)
  
    # Get citations and downloads
    metricsurl = bookpg %>% html_nodes(".book-metrics") %>% html_attr("data-bookmetrix-ajax-url")
    metricsurl = paste0("http://www.springer.com",metricsurl)
  
    metricspg = metricsurl %>% read_html()
  
    citations = metricspg %>% html_nodes(".citations .book-metric-item-value") %>% html_text() 
    citations = as.numeric(citations)
    if(length(citations) == 0) citations = 0
  
    downloads = metricspg %>% html_nodes(".downloads .book-metric-item-value") %>% html_text()
    downloads = as.numeric(downloads)
    if(length(downloads) == 0) downloads = 0
  
    # Get recommended books
    recomurl = bookpg %>% html_nodes(".product-recommendation") %>% html_attr("data-baynote-service-url")
    recomurl = paste0("http://www.springer.com",recomurl)
  
    recompg = recomurl %>% read_html()
    recobooks = recompg %>% html_nodes(".product-information p a") %>% html_text()
  
    bookinfo = data.frame(title = title,
                        authors = authors,
                        price1 = price[1],
                        price1curr = currency[1],
                        price2 = price[2],
                        price2curr = currency[2],
                        citations = citations,
                        downloads = downloads)
  
    recobookinfo = data.frame(title = title, recobooks = recobooks, rnk = seq(1,length(recobooks)))
  
    return(list(bookinfo = bookinfo, recobookinfo = recobookinfo))
  
}

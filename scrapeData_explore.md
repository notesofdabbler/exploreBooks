

```r
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
```

```
##  [1] "R by Example"               "Biostatistics with R"      
##  [3] "Numerical Ecology with R"   "Chemometrics with R"       
##  [5] "Meta-Analysis with R"       "Quality Control with R"    
##  [7] "Graphical Models with R"    "Bayesian Networks in R"    
##  [9] "Modern Optimization with R" "Six Sigma with  R"
```

```r
# link to book details
booklinklist = userPg %>% html_nodes(".productGraphic a") %>% html_attr("href")
booklinklist
```

```
##  [1] "http://www.springer.com/us/book/9781461413646"
##  [2] "http://www.springer.com/us/book/9781461413011"
##  [3] "http://www.springer.com/us/book/9781441979759"
##  [4] "http://www.springer.com/us/book/9783642178405"
##  [5] "http://www.springer.com/us/book/9783319214153"
##  [6] "http://www.springer.com/us/book/9783319240442"
##  [7] "http://www.springer.com/us/book/9781461422983"
##  [8] "http://www.springer.com/us/book/9781461464457"
##  [9] "http://www.springer.com/us/book/9783319082622"
## [10] "http://www.springer.com/us/book/9781461436515"
```

```r
# book publication year
bookyr = userPg %>% html_nodes(xpath = "//span[contains(@class,'renditionDescription')]") %>% html_text()
bookyr
```

```
##  [1] "2012" "2012" "2011" "2011" "2015" "2015" "2012" "2013" "2014" "2012"
```

```r
# book author
bookauth = userPg %>% html_nodes("span[class = 'displayBlock']") %>% html_text()
# bookauth = userPg %>% html_nodes(xpath = "//span[@class = 'displayBlock']") %>% html_text()  ## will give same result
bookauth
```

```
##  [1] "Albert, Jim, Rizzo, Maria"                                    
##  [2] "Shahbaba, Babak"                                              
##  [3] "Borcard, Daniel, Gillet, Francois, Legendre, Pierre"          
##  [4] "Wehrens, Ron"                                                 
##  [5] "Schwarzer, Guido, Carpenter, James R, Rücker, Gerta"          
##  [6] "Cano, Emilio L., M. Moguerza, Javier, Prieto Corcoba, Mariano"
##  [7] "Højsgaard, Søren, Edwards, David, Lauritzen, Steffen"         
##  [8] "Nagarajan, Radhakrishnan, Scutari, Marco, Lèbre, Sophie"      
##  [9] "Cortez, Paulo"                                                
## [10] "Cano, Emilio L., M. Moguerza, Javier, Redchuk, Andrés"
```

```r
# book price
bookprice = userPg %>% html_nodes(xpath = "//div[@class = 'bookListPriceContainer']//span[2]") %>% html_text()
bookprice
```

```
##  [1] "$59.99" "$59.99" "$59.99" "$59.99" "$39.99" "$49.99" "$54.99"
##  [8] "$29.99" "$29.99" "$59.99"
```

```r
pgdf = data.frame(title = booktitles, link = booklinklist, pubyr = bookyr,
                  auth = bookauth, price = bookprice)
pgdf
```

```
##                         title
## 1                R by Example
## 2        Biostatistics with R
## 3    Numerical Ecology with R
## 4         Chemometrics with R
## 5        Meta-Analysis with R
## 6      Quality Control with R
## 7     Graphical Models with R
## 8      Bayesian Networks in R
## 9  Modern Optimization with R
## 10          Six Sigma with  R
##                                             link pubyr
## 1  http://www.springer.com/us/book/9781461413646  2012
## 2  http://www.springer.com/us/book/9781461413011  2012
## 3  http://www.springer.com/us/book/9781441979759  2011
## 4  http://www.springer.com/us/book/9783642178405  2011
## 5  http://www.springer.com/us/book/9783319214153  2015
## 6  http://www.springer.com/us/book/9783319240442  2015
## 7  http://www.springer.com/us/book/9781461422983  2012
## 8  http://www.springer.com/us/book/9781461464457  2013
## 9  http://www.springer.com/us/book/9783319082622  2014
## 10 http://www.springer.com/us/book/9781461436515  2012
##                                                             auth  price
## 1                                      Albert, Jim, Rizzo, Maria $59.99
## 2                                                Shahbaba, Babak $59.99
## 3            Borcard, Daniel, Gillet, Francois, Legendre, Pierre $59.99
## 4                                                   Wehrens, Ron $59.99
## 5            Schwarzer, Guido, Carpenter, James R, Rücker, Gerta $39.99
## 6  Cano, Emilio L., M. Moguerza, Javier, Prieto Corcoba, Mariano $49.99
## 7           Højsgaard, Søren, Edwards, David, Lauritzen, Steffen $54.99
## 8        Nagarajan, Radhakrishnan, Scutari, Marco, Lèbre, Sophie $29.99
## 9                                                  Cortez, Paulo $29.99
## 10         Cano, Emilio L., M. Moguerza, Javier, Redchuk, Andrés $59.99
```

```r
##  Get details for a single book 

booklink = "http://www.springer.com/us/book/9781461478997"
bookpg = booklink %>% read_html()

# Get title
booktitle = bookpg %>% html_nodes(".cms-col dd[itemprop = 'name']") %>% html_text()
#bookpg %>% html_nodes(xpath = "//dl[@class = 'cms-col']//dd[@itemprop = 'name']") %>% html_text()
booktitle
```

```
## [1] "XML and Web Technologies for Data Sciences with R"
```

```r
# Get authors
bookauth = bookpg %>% html_nodes(".cms-col li[itemprop = 'author']") %>% html_text()
bookauth
```

```
## [1] "\n                                    Deborah Nolan\n                                "     
## [2] "\n                                    Duncan Temple Lang\n                                "
```

```r
# Get price currency and price
priceCurr = bookpg %>% html_nodes("meta[itemprop = 'priceCurrency']") %>% html_attr("content")
priceCurr
```

```
## [1] "USD" "USD"
```

```r
price = bookpg %>% html_nodes("meta[itemprop = 'price']") %>% html_attr("content")
price
```

```
## [1] "49.99" "69.99"
```

```r
# Get citations and downloads
metricsurl = bookpg %>% html_nodes(".book-metrics") %>% html_attr("data-bookmetrix-ajax-url")
metricsurl = paste0("http://www.springer.com",metricsurl)
metricsurl
```

```
## [1] "http://www.springer.com/app-pp/bookmetrix?doi=10.1007%2F978-1-4614-7900-0"
```

```r
metricspg = metricsurl %>% read_html()
citations = metricspg %>% html_nodes(".citations .book-metric-item-value") %>% html_text() 
citations
```

```
## [1] "2"
```

```r
downloads = metricspg %>% html_nodes(".downloads .book-metric-item-value") %>% html_text()
downloads
```

```
## [1] "146204"
```

```r
# Get recommended books
recomurl = bookpg %>% html_nodes(".product-recommendation") %>% html_attr("data-baynote-service-url")
recomurl = paste0("http://www.springer.com",recomurl)
recomurl
```

```
## [1] "http://www.springer.com/app-pp/recommendation/us/978-1-4614-7899-7"
```

```r
recompg = recomurl %>% read_html()
recobooks = recompg %>% html_nodes(".product-information p a") %>% html_text()
recobooks
```

```
## [1] "R for Marketing Research and Analytics"  
## [2] "Regression Modeling Strategies"          
## [3] "ggplot2"                                 
## [4] "Political Analysis Using R"              
## [5] "Principal Component Analysis"            
## [6] "A Beginner's Guide to R"                 
## [7] "Monte Carlo Statistical Methods"         
## [8] "Model Selection and Multimodel Inference"
```

```r
# Analysis done with RStudio 0.99.489
sessionInfo()
```

```
## R version 3.2.3 (2015-12-10)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.11.1 (El Capitan)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.11      stringr_1.0.0   dplyr_0.4.3     rmarkdown_0.8.1
## [5] rvest_0.3.1     xml2_0.1.2     
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.2     XML_3.98-1.3    assertthat_0.1  digest_0.6.8   
##  [5] R6_2.1.1        DBI_0.3.1       formatR_1.2.1   magrittr_1.5   
##  [9] evaluate_0.8    httr_1.0.0      stringi_1.0-1   curl_0.9.4     
## [13] tools_3.2.3     selectr_0.2-3   markdown_0.7.7  parallel_3.2.3 
## [17] htmltools_0.2.6
```


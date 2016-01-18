

```r
#
#  scraping a simple html page
#  Page has 4 people with contact information. We want to get them into a dataframe
#

library(rvest)

# Read the page
pg = read_html("simplePage.html")

# Get list of names
# attribute id can be extracted with prefixing #
name = pg %>% html_nodes("#name") %>% html_text()
name
```

```
## [1] "Alexander, Jones" "Liz, Phillips"    "Joe, Whoisit"    
## [4] "Hello, Howareyou"
```

```r
# extracting names with xpath
names2 = pg %>% html_nodes(xpath = "//li//div[@id = 'name']") %>% html_text()
names2
```

```
## [1] "Alexander, Jones" "Liz, Phillips"    "Joe, Whoisit"    
## [4] "Hello, Howareyou"
```

```r
# get links from href attribute
link = pg %>% html_nodes("a") %>% html_attr("href")
link
```

```
## [1] "http:/www.meetup.com" "http:/www.google.com" "http:/www.google.com"
## [4] "http:/www.google.com"
```

```r
# Get list of addresses and cities
# attribute class can be extracted with prefixing period (.)
addr = pg %>% html_nodes(".add") %>% html_text()
addr
```

```
## [1] "123, Maple Drive"   "123, Walnut Drive"  "999, Icecream Blvd"
## [4] "843, Crazy Drive"
```

```r
city = pg %>% html_nodes("li .city span") %>% html_text() 
city
```

```
## [1] "Indianapolis"  "Chicago"       "New York"      "San Francisco"
```

```r
# Get state and zip code info
state = pg %>% html_nodes("p[geo = 'state']") %>% html_text()
state
```

```
## [1] "IN" "IL" "NY" "CA"
```

```r
zipcode = pg %>% html_nodes("p[geocode = 'zip']") %>% html_text()
zipcode
```

```
## [1] "12345" "67809" "54321" "11111"
```

```r
# What if geo and geocode attributes are not there in html
state2 = pg %>% html_nodes("div p") %>% html_text() # won't work since it gets both state and zip code
state2
```

```
## [1] "IN"    "12345" "IL"    "67809" "NY"    "54321" "CA"    "11111"
```

```r
# directly using xpath handy since it is able to just extract the first element for state and second element for zip
state2 = pg %>% html_nodes(xpath = "//div[@class = 'city']/p[1]") %>% html_text() 
state2
```

```
## [1] "IN" "IL" "NY" "CA"
```

```r
zip2 = pg %>% html_nodes(xpath = "//div[@class = 'city']/p[2]") %>% html_text() 
zip2
```

```
## [1] "12345" "67809" "54321" "11111"
```

```r
# Get phone numbers
phone = pg %>% html_nodes(".comm p[type = 'phone']") %>% html_text() 
phone # problem since only 2 addresses have phone numbers and it will be hard to align with other fields
```

```
## [1] "317-123-4567" "123-999-4567"
```

```r
# workaround is to force using all nodes
addrnodes = pg %>% html_nodes("li")
addrnodes
```

```
## {xml_nodeset (4)}
## [1] <li>\n           <div id="name"><a href="http:/www.meetup.com">Alexa ...
## [2] <li>\n           <div id="name"><a href="http:/www.google.com">Liz,  ...
## [3] <li>\n           <div id="name"><a href="http:/www.google.com">Joe,  ...
## [4] <li>\n           <div id="name"><a href="http:/www.google.com">Hello ...
```

```r
phone = sapply(addrnodes, function(x) x %>% html_nodes(".comm p[type = 'phone']") %>% html_text())
phone # has length 0 fields when phone doesn't exist
```

```
## [[1]]
## [1] "317-123-4567"
## 
## [[2]]
## character(0)
## 
## [[3]]
## [1] "123-999-4567"
## 
## [[4]]
## character(0)
```

```r
phone = sapply(phone, function(x) if(length(x) == 0) NA else x) # set NA to missing phone numbers
phone
```

```
## [1] "317-123-4567" NA             "123-999-4567" NA
```

```r
# Get email addresses
email = sapply(addrnodes, function(x) x %>% html_nodes(".comm p[type = 'email']") %>% html_text())
email = sapply(email, function(x) if(length(x) == 0) NA else x) 
email
```

```
## [1] "jabc@gmail.com"  NA                "jwhst@yahoo.com" "hhru@yahoo.com"
```

```r
# create a dataframe with address data
addrdf = data.frame(name = name, addr = addr, city = city, state = state, zipcode = zipcode,
                    phone = phone, email = email, link = link)
addrdf
```

```
##               name               addr          city state zipcode
## 1 Alexander, Jones   123, Maple Drive  Indianapolis    IN   12345
## 2    Liz, Phillips  123, Walnut Drive       Chicago    IL   67809
## 3     Joe, Whoisit 999, Icecream Blvd      New York    NY   54321
## 4 Hello, Howareyou   843, Crazy Drive San Francisco    CA   11111
##          phone           email                 link
## 1 317-123-4567  jabc@gmail.com http:/www.meetup.com
## 2         <NA>            <NA> http:/www.google.com
## 3 123-999-4567 jwhst@yahoo.com http:/www.google.com
## 4         <NA>  hhru@yahoo.com http:/www.google.com
```

```r
#
#  Scraping a html table
#  scraping list of packages from CRAN site
#
url = "http://cran.us.r-project.org/web/packages/available_packages_by_date.html"
cranpg = read_html(url)
cranlist = cranpg %>% html_nodes("table") %>% html_table()
head(cranlist[[1]])
```

```
##         Date          Package
## 1 2016-01-17              DJL
## 2 2016-01-17     freqparcoord
## 3 2016-01-17             nlme
## 4 2016-01-17             OECD
## 5 2016-01-17       proportion
## 6 2016-01-17 RcmdrPlugin.NMBU
##                                                                Title
## 1                       Distance Measure Based Judgment and Learning
## 2                             Novel Methods for Parallel Coordinates
## 3                          Linear and Nonlinear Mixed Effects Models
## 4                              Search and Extract Data from the OECD
## 5 Inference on Single Binomial Proportion and Bayesian\nComputations
## 6        R Commander Plug-in for University Level Applied Statistics
```

```r
# Used RStudio 0.99.489
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
## [1] rmarkdown_0.8.1 rvest_0.3.1     xml2_0.1.2     
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.2     XML_3.98-1.3    digest_0.6.8    R6_2.1.1       
##  [5] magrittr_1.5    evaluate_0.8    httr_1.0.0      stringi_1.0-1  
##  [9] curl_0.9.4      tools_3.2.3     stringr_1.0.0   selectr_0.2-3  
## [13] markdown_0.7.7  htmltools_0.2.6 knitr_1.11
```


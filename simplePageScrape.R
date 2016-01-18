
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

# extracting names with xpath
names2 = pg %>% html_nodes(xpath = "//li//div[@id = 'name']") %>% html_text()
names2

# get links from href attribute
link = pg %>% html_nodes("a") %>% html_attr("href")
link

# Get list of addresses and cities
# attribute class can be extracted with prefixing period (.)
addr = pg %>% html_nodes(".add") %>% html_text()
addr

city = pg %>% html_nodes("li .city span") %>% html_text() 
city

# Get state and zip code info
state = pg %>% html_nodes("p[geo = 'state']") %>% html_text()
state

zipcode = pg %>% html_nodes("p[geocode = 'zip']") %>% html_text()
zipcode

# What if geo and geocode attributes are not there in html
state2 = pg %>% html_nodes("div p") %>% html_text() # won't work since it gets both state and zip code
state2

# directly using xpath handy since it is able to just extract the first element for state and second element for zip
state2 = pg %>% html_nodes(xpath = "//div[@class = 'city']/p[1]") %>% html_text() 
state2

zip2 = pg %>% html_nodes(xpath = "//div[@class = 'city']/p[2]") %>% html_text() 
zip2

# Get phone numbers
phone = pg %>% html_nodes(".comm p[type = 'phone']") %>% html_text() 
phone # problem since only 2 addresses have phone numbers and it will be hard to align with other fields

# workaround is to force using all nodes
addrnodes = pg %>% html_nodes("li")
addrnodes

phone = sapply(addrnodes, function(x) x %>% html_nodes(".comm p[type = 'phone']") %>% html_text())
phone # has length 0 fields when phone doesn't exist
phone = sapply(phone, function(x) if(length(x) == 0) NA else x) # set NA to missing phone numbers
phone

# Get email addresses
email = sapply(addrnodes, function(x) x %>% html_nodes(".comm p[type = 'email']") %>% html_text())
email = sapply(email, function(x) if(length(x) == 0) NA else x) 
email

# create a dataframe with address data
addrdf = data.frame(name = name, addr = addr, city = city, state = state, zipcode = zipcode,
                    phone = phone, email = email, link = link)
addrdf

#
#  Scraping a html table
#  scraping list of packages from CRAN site
#
url = "http://cran.us.r-project.org/web/packages/available_packages_by_date.html"
cranpg = read_html(url)
cranlist = cranpg %>% html_nodes("table") %>% html_table()
head(cranlist[[1]])

# Used RStudio 0.99.489
sessionInfo()

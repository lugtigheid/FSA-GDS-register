library(rvest)
library(dplyr)
library(readr)
library(jsonlite)
library(httr)

# --------------------------------------------------------------

# sneaky but quite effective: let's use wikipedia for backup links
page <- 'https://en.wikipedia.org/wiki/List_of_English_districts'

# get the full table of LA's
table.counties <- read_html(page) %>% 
  html_node('.wikitable') %>% 
  html_table()

# get the links to their websites
table.links <- read_html(page) %>% 
  html_node('.wikitable') %>% 
  html_nodes('tr') %>%
  html_node('.external') %>%
  html_attr('href')

# remove the first row
tmp <- data.frame(
  links = table.links
) %>% filter(!is.na(links))

# rename multi-word columns and select just the important columns
data <- cbind(table.counties, tmp) %>%
  rename(county = `Ceremonial County`) %>%
  select(Name, county, links)

# --------------------------------------------------------------

# now get the list of local authorities from GDS
url <- paste0('https://local-authority-eng.register.gov.uk/',
                'records.csv?page-size=5000&page-index=1')

# read in the CSV
gds.list <- read_csv(url)

# this function gets the link from google
get_link <- function(url) {
  
  link <- read_html(url) %>% 
    html_node('h3.r a') %>% 
    html_attr('href')
  
  return(link)
}

qs <- 'https://www.google.co.uk/search?q='

# create the search string and get the link
# this will take a while as it goes off to google and gets links
p <- gds.list %>% 
  mutate(query=gsub(" ", "+", `official-name`, fixed=TRUE)) %>%
  mutate(search = paste0(qs,query,'&ie=utf-8&oe=utf-8')) %>%
  rowwise() %>% mutate(link = get_link(search))

# the link needs parsing, so get rid of query string and prefix
p <-  p %>% rowwise() %>%
  mutate(link = gsub( "&.*", "", link)) %>%
  mutate(link = substring(link, 8, nchar(link)))

# rebuild the URL without the directories
p <- p %>% rowwise() %>%
  mutate(
    link = ifelse(
      substring(link, 0, 4) == 'http', 
      paste(
        parse_url(link)$scheme,
        parse_url(link)$hostname,
      sep = '://'),
      NA
    )
  )

### 
#-- Need to change city of Lincoln to Lincoln. 
###

p <- p %>% mutate(link = ifelse(is.na(link), links, link)

# link in data from wikipedia
p <- p %>% left_join(data, by=c('name'='Name'))

# forest dean doesn't have their website as first result, so instead
# we use the link that is in the wikipedia entry (assume it is correct)
p <- p %>% mutate(link = ifelse(is.na(link), links, link)

districts <- p %>% filter(`local-authority-type` != 'CTY')
counties <- p %>% filter(`local-authority-type` == 'CTY')

# this now links the counties to each district
all.data <- districts %>% left_join(counties, by=c('county'='name'))

# determine who does Trading Standards and who does Environmental Health
# for each Local Authority - only Non-Metropolitan Districts do not do TS. 
all.data <- all.data %>% 
  mutate(eh.name = `official-name.x`) %>%
  mutate(eh.link = link.x) %>%
  mutate(ts.name = ifelse(`local-authority-type.x` == 'NMD', 
                          `official-name.y`, `official-name.x`)) %>%
  mutate(ts.link = ifelse(`local-authority-type.x` == 'NMD', 
                          `link.y`, `link.x`))

p %>% write_csv('draft-register.csv')


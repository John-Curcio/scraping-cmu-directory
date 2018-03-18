####### Stuff local to my machine ##########

# dsc-general-body.csv is the default name when you download data from the google group
all_members <- read.csv("dsc-general-body.csv")
####################################

# subsetting dataframe based on whether they've got an active cmu email
all_andrews <- subset(all_members, 
                      grepl("cmu.edu", all_members$Email.address))
curr_andrews <- subset(all_andrews, 
                       all_andrews$Email.status != "bouncing")
curr_andrews$Class.Level <- ""
curr_andrews$Departments <- ""

print(head(curr_andrews))

# Libraries for scraping CMU online directory
library("RSelenium")
library("rvest")
#initialize remote driver
rD <- rsDriver()
remDr <- rD[["client"]]

remDr$navigate("https://directory.andrew.cmu.edu/index.cgi")

# Check that the person is a student here and that we've got the right andrew
# - Requires html_source to be an object of the type returned by read_html (XML document)
# - Returns a boolean
is_student <- function(html_source){
  # html_source <- read_html(html_source[[1]])
  bold_headers <- html_source %>%
    html_nodes("h1>b") %>%
    html_text()
  # Take advantage of short-circuit evaluation
  return(length(bold_headers) == 1 && 
           grepl("Student", bold_headers[1]))
}

# Obtain list of affiliated departments as strings
# - Requires raw_html_text to be a string
# - Returns a list of strings
get_departments <- function(raw_html_text){
  pattern <- "Department with which this person is affiliated:</b><br />"
  start_pos <- regexpr(pattern, raw_html_text) + nchar(pattern)
  temp <- substr( raw_html_text, start = start_pos, stop = start_pos + 100)
  stop_pos <- regexpr("<p>", temp) + start_pos - 2
  majors <- substr(raw_html_text, start = start_pos, stop = stop_pos)
  return(strsplit(majors, "<br />"))
}

# Obtain class level of student - Freshman, Senior, Masters, PhD
# - Requries html_source to be an object of the type returned by read_html (XML Document)
# - Returns a string
get_year <- function(html_source){
  # Super super hack-y
  paragraphs <- html_source %>%
    html_nodes("body>div>div>p") %>%
    html_text()
  paragraph <- paragraphs[3]
  pos <- nchar("Student Class Level:")
  year <- substr(paragraph, start = pos+1, stop = 100)
  return(year)
}

# Loop over all the names in our dataframe and get the person's year and department(s)
for(i in 1:nrow(curr_andrews)){
  andrew <- curr_andrews$Email.address[i]
  
  # Enter andrew email into the search box and hit enter
  basic_search <- remDr$findElement(using = 'id', value = "basicsearch")
  basic_search$clearElement()
  basic_search$sendKeysToElement(list(andrew, key = "enter"))
  # sleep for a random amount of time - don't want to get kicked off the frail connection
  Sys.sleep(rexp(n=1, rate=0.5))
  
  page_source <- remDr$getPageSource()
  page_source_cleaner <- read_html(page_source[[1]])
  if(is_student(page_source_cleaner)){
    curr_andrews$Class.Level[i] <- get_year(page_source_cleaner)
    curr_andrews$Departments[i] <- paste(get_departments(page_source[[1]]), collapse = ", ")
    # Standard output keeps the madness at bay
    cat(andrew, curr_andrews$Class.Level[i], curr_andrews$Departments[i], "\n")
  }
}

# Record this hard-won data for later
write.csv(curr_andrews, file="dsc-demographics.csv")

# The polite thing to do
remDr$close()
rD$server$stop()
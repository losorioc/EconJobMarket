## Scrapes job postings from econjobmarket.org
## uses the code from http://freigeist.devmag.net

## Clear out memory
rm(list = ls())
gc()

## Load required packages
require(gdata)
require(XML)
require(rvest)
require(stringr)
options(stringsAsFactors=FALSE)
require(tidyr)
require(data.table)
require(dplyr)
require(plyr)

setwd("C:/Dropbox/PhD/Job_Market/econJobMarket/Rscripts")
## Function to grab nth through last components of a string:
substrEnd <- function(x, n){
  substr(x, n, nchar(x))
}

## Number of pages of listings (user needs to adjust each time)
nwebpages <- 7
warning("Make sure you change nwebpages in line 26")

login <- "https://econjobmarket.org/login"
pgsession <- html_session(login)
pgform <- html_form(pgsession)[[1]]
filled_form <- set_values(pgform, email = "xxxxx", password ="xxxxx")
submit_form(pgsession, filled_form)

for (j in 1:nwebpages) {
    url<-"https://econjobmarket.org/positions?page="
    url<-paste0(url, j)
    page<-jump_to(pgsession, url)
    temp     <- read_html(page)
    alldata  <- temp %>% html_nodes("div.panels")

    alldata_title_uni  <- alldata %>% html_nodes("div.col-md-4") %>% as.character() #returns listings as character vector

    alldata_uni <- alldata %>% html_nodes("div.media-body") %>% as.character()
    department <- gsub('<div class="media-body">\n', "", alldata_uni)
    department <- gsub('<br>\n.*', "", department)
    department <- gsub('\n', "", department)
    department <- gsub('  ', "", department)
        
    university <- gsub('<div class="media-body">', "", alldata_uni)
    university <- gsub('.*<br>\n', "", university)
    university <- gsub('  ', "", university)
    university <- gsub('</div>', "", university)
    university <- gsub('\n', "", university)

    
    ## Capture listing titles

    listings <- sapply(getNodeSet(htmlParse(alldata_title_uni), "//a"), xmlValue) %>% as.character()
    listings <- gsub('\n', "", listings)
    listings <- gsub('  ', "", listings)


    alldata_field <- list()
    
    for (t in c(3:(length(university)+2))) {
     alldata_field_prel <- temp %>% html_nodes(paste('#app > div > div.panels > div:nth-child(',t, ') > div:nth-child(1) > div:nth-child(3)')) %>% as.character()
     n <- t-2
     alldata_field[n] <- c(alldata_field_prel)
     }

    alldata_field <- sapply(alldata_field, '[[', 1)
    
    position_type <- gsub('<hr class="type-field-separator">.*', "", alldata_field)
    position_type <- gsub('<div class="col-md-2">', "", position_type)
    position_type <- gsub('  ', "", position_type)
    position_type <- gsub('\n', "", position_type)
    position_type <- gsub(pattern = '\U00A0', "", position_type)
    position_type <- gsub(pattern = '\U2022', ", ", position_type)  
    
    field <- gsub('.*<hr class="type-field-separator">', "", alldata_field)
    field <- gsub('  ', "", field)
    field <- gsub('\n', "", field)
    field <- gsub('<[^>]+>', "", field)
    field <- gsub(pattern = '\U00A0', "", field)
    field <- gsub(pattern = '\U2022', ", ", field)  

    alldata_dates <- list()
    
    for (t in c(3:(length(university)+2))) {
     alldata_dates_prel <- temp %>% html_nodes(paste('#app > div > div.panels > div:nth-child(',t, ') > div:nth-child(1) > div:nth-child(4)')) %>% as.character()
     n <- t-2
     alldata_dates[n] <- c(alldata_dates_prel)
     }

    alldata_dates <- sapply(alldata_dates, '[[', 1)
    
    posted <- gsub('<br>.*', "", alldata_dates)
    posted <- gsub('<div class=\"col-md-2\">\n', "", posted)
    posted <- gsub('<span class=\"title\">Posted:</span>\n', "", posted)
    posted <- gsub('\n', "", posted)
    posted <- gsub('  ', "", posted)
    
    deadline <- gsub('.*<br>', "", alldata_dates)
    deadline <- gsub('<span class=\"title\">Deadline:</span>\n', "", deadline)
    deadline <- gsub('</div>', "", deadline)
    deadline <- gsub('\n', "", deadline)
    deadline <- gsub('  ', "", deadline)
    
    #############################
    
    fulltext  <- alldata %>% html_nodes("div.row.end") %>% as.character()
    strip_html <- function(s) {
      html_text(read_html(s))
    }

    # fulltext1 <- strip_html(fulltext[[1]])
    
    fulltext_final <- list()

    for (i in c(1:length(university))) {
      fulltext1 <- strip_html(fulltext[[i]])
      fulltext_final[i] <- c(fulltext1)
    }
    
    fulltext_final <- sapply(fulltext_final, '[[', 1)
    fulltext_final <- gsub('  ', "", fulltext_final)
    for (i in c('\n', '\r', '\U00A0', '\U201C', '\U2013', '\U2019', '\U2013', '\U2010', '\U02BC')) {
      fulltext_final <- gsub(i, "", fulltext_final)
    }

    ###################################
    
    conference1 <- list()
    for (t in c(3:(length(university)+2))) {
      prel <- temp %>% html_nodes(paste('#app > div > div.panels > div:nth-child(',t, ') > div.row.end > div.col-md-12'))
      prel <- prel %>% html_nodes('div')
      prel <- prel %>% html_nodes('ul')
      conference <- data.frame(link = html_attr(html_nodes(prel, "a"), 'href'))
      conference <- conference %>% gather(link) %>% filter(grepl("https", link))
      n <- t-2
      if (length(conference)==0) {
        conference1[n] <- 0 
      } else {
        conference1[n] <-  conference      
      }
      conference1[[n]] <- conference
    }
    
    conference1 <- sapply(conference1, '[[', 1)
    n.obs <- sapply(conference1, length)
    seq.max <- seq_len(max(n.obs))
    conference_page <- t(sapply(conference1, "[", i = seq.max))

    conf_title <- c(paste0("conference", 1:ncol(conference_page)))
    colnames(conference_page) <- conf_title
    
    if (dim(conference_page)[1] == 1) {
      conference_page <- gather(data.frame(conference_page))[2]
    } else {
      conference_page <- conference_page
    }
    
    emails <- list()
    for (t in c(3:(length(university)+2))) {
      prel <- temp %>% html_nodes(paste('#app > div > div.panels > div:nth-child(',t, ') > div.row.end'))
      emails1 <- data.frame(link = html_attr(html_nodes(prel, "a"), 'href'))
      emails1 <- emails1 %>% gather(link) %>% filter(grepl("mailto", link))
      n <- t-2
      if (length(emails1)==0) {
        emails[n] <- 0 
      } else {
        emails[n] <-  emails1      
      }
      emails[[n]] <- emails1
    }
    
    emails <- sapply(emails, '[[', 1)
    n.obs <- sapply(emails, length)
    seq.max <- seq_len(max(n.obs))

    emails_page <- t(sapply(emails, "[", i = seq.max))
    emails_title <- c(paste0("email", 1:ncol(emails_page)))
    colnames(emails_page) <- emails_title
    
    if (dim(emails_page)[1] == 1) {
      emails_page <- gather(data.frame(emails_page))[2]
    } else {
      emails_page <- emails_page
    }

    other_links <- list()
    for (t in c(3:(length(university)+2))) {
      prel <- temp %>% html_nodes(paste('#app > div > div.panels > div:nth-child(',t, ') > div.row.end > div.col-md-12'))
      prel <- prel %>% html_nodes('div')
      prel <- prel %>% html_nodes('p')
      other <- data.frame(link = html_attr(html_nodes(prel, "a"), 'href'))
      other <- other %>% gather(link) %>% filter(grepl("https", link))
      n <- t-2
      if (length(other)==0) {
        other_links[n] <- 0 
      } else {
        other_links[n] <-  other      
      }
      other_links[[n]] <- other
    }
    
    other_links <- sapply(other_links, '[[', 1)
    n.obs <- sapply(other_links, length)
    seq.max <- seq_len(max(n.obs))
    links_page <- t(sapply(other_links, "[", i = seq.max))
    
    links_title <- c(paste0("link", 1:ncol(links_page)))
    colnames(links_page) <- links_title
    
    if (dim(links_page)[1] == 1) {
      links_page <- gather(data.frame(links_page))[2]
    } else {
      links_page <- links_page
    }
    
    ## Concatenate into final data frame
    app       <- paste0("application_", j)
    full_text <- paste0("fulltext_", j)
    link <- paste0("link_", j)
    conf <- paste0("conference_", j)
    email <- paste0("email_",j)
    nam <- paste0("EJM", j)
    # assign(nam, data.frame(employers,listings,deadlines,urlfinal,fulltext))
    assign(full_text, data.frame(fulltext_final))
    assign(link, data.frame(links_page))
    assign(email, data.frame(emails_page))
    assign(conf, data.frame(conference_page))
    assign(nam, data.frame(university, department, listings, position_type, field, posted, deadline))

}

full_text   <- do.call(rbind.fill, lapply( paste0("fulltext_", 1:nwebpages) , get))
links       <- do.call(rbind.fill, lapply( paste0("link_", 1:nwebpages) , get))
emails      <- do.call(rbind.fill, lapply( paste0("email_", 1:nwebpages) , get))
conference  <- do.call(rbind.fill, lapply( paste0("conference_", 1:nwebpages) , get))
EJM         <- do.call(rbind, lapply( paste0("EJM", 1:nwebpages) , get))


colnames(EJM) <- c("Institution","Department","Position","Position type", "Field", "Posted", "Deadline")

EJM <- cbind(EJM, conference, emails, links, full_text)

## Export to CSV

write.csv(EJM, paste0('../Output/All_EJM_jobs_',Sys.Date(), '_raw.csv'))

##--------------------------------------------------------------------------------------------------------------------------
## User should fill in the next section of the script with his or her preferences. I give examples of the many possibilities
##--------------------------------------------------------------------------------------------------------------------------

# Drop non-full-time/non-qualifying listings
EJM <- EJM[!grepl(paste(c("distinguished","lecture","chair","professorship"), collapse="|"),EJM$Position, ignore.case=TRUE, perl=TRUE, useBytes=TRUE),]
# EJM <- EJM[grepl("assistant",EJM$Position, ignore.case=TRUE, perl=TRUE, useBytes=TRUE) & !grepl(paste(c("associate","full"), collapse="|"),EJM$Position, ignore.case=TRUE, perl=TRUE, useBytes=TRUE) ,]

# Drop listings from specific institutions
EJM <- EJM[!grepl(paste(c("dhabi","dalian","shenzhen","shanghai","tokyo","dubai"), collapse="|"),
                          EJM$Institution, ignore.case=TRUE, perl=TRUE, useBytes=TRUE),]

##--------------------------------------------------------------------------------------------------------------------------
## End of user customization section
##--------------------------------------------------------------------------------------------------------------------------

write.csv(EJM, paste0('../Output/All_EJM_jobs_',Sys.Date(), '_processed.csv'))


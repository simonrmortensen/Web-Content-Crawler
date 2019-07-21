if (!require(rvest)) install.packages("rvest")
if (!require(purrr)) install.packages("purrr")
if (!require(dplyr)) install.packages("dplyr")
library(rvest)
library(purrr)
library(dplyr)

#Manual input
hostname <- "iform.dk"                                            #Input the hostname (mind if there is www redirects)
exclude_links_containing <- "index, users, facebook, jpg, jpeg"   #Input words of links you'd like to exclude
article_css <- "h1, article"                                      #Input the CSS-selector of the content you'd like to scrape
exclude_article_css <- ""                                         #Input the CSS-selector of the content you'd like to NOT scrape


#Automated Variables and Concatenations
url <- paste("https://", hostname, sep="")

excluded_links <- ""
for (i in strsplit(exclude_links_containing, ", ")) {
    excluded_links <- paste(excluded_links, ":not(a[href*=", i,"])", sep = "")
}

exclude_link_css <- if (exclude_links_containing != "") {
    paste(excluded_links, collapse = "")
}

exclude_article_css_concat <- if (exclude_article_css != "") {paste(":not(", exclude_article_css, ")", sep="")}
link_css_selector <- paste('a[href^="', url, '"]', exclude_link_css, sep="")
pagecontent_css_selector <- paste(article_css, exclude_article_css_concat, sep="")

regex_remove_from_pagecontent <- "[\r\n]|  +" 
# End of Automated Variables and Concatenations


sitemap <- read_html(url) %>%
  html_nodes(link_css_selector) %>%
  html_attr('href') 

sitemap <- sitemap[!duplicated(sitemap)]
ignore_pages <- c()

#CREATE GLOBAL SITEMAP
for (i in sitemap) {
    print(paste("Started crawling for sitemap: ", i, sep=""))
    print(match(i, sitemap))
    
    pageLinks <- try(read_html(i) %>%
      html_nodes(link_css_selector) %>%
      html_attr('href'), 
      silent = TRUE
      )
    
    if (is.null(pageLinks)) {
      ignore_pages <- c(ignore_pages, i)
      next
    }

    sitemap <- c(sitemap, pageLinks)
    sitemap <- sitemap[!duplicated(sitemap)]
    
    print(paste("Successfully crawled for sitemap: ", i, sep=""))
}

sitemap <- sitemap[! sitemap %in% ignore_pages]


# SCRAPE WEB PAGE CONTENT
all_pages_content <- c()

for (i in sitemap) {
  if (i %in% ignore_pages){
    next
  }
  
  print(paste("Started crawling for content: ", i, sep=""))
  
  page_content <- try(read_html(i) %>%
    html_nodes(pagecontent_css_selector) %>%
    html_text() %>%
    toString(), 
    silent = TRUE)
    
  if (!is.null(page_content)){
    all_pages_content <- c(all_pages_content, gsub(regex_remove_from_pagecontent, "", page_content))
  } else{
    all_pages_content <- c(all_pages_content, "Error Accessing page")
  }
  
  print(paste("Successfully crawled for content: ", i, sep=""))
}

names(all_pages_content) <- sitemap

print(paste("Successfully crawled ", hostname, " for article content.", sep=""))

data_table <- data.frame(all_pages_content)
View(data_table)




# A pros' solution, revisit
    # r <- read_html(url) %>%
    #   html_nodes(link_css_selector) %>%
    #   html_attr('href') %>%
    #   Filter(function(f) !is.na(f) & !grepl(x = f, pattern = '#|facebook|linkedin|twitter|youtube'), .) %>%
    #   map(~{
    #     print(.x)
    #     html_session(url) %>%
    #       jump_to(.x) %>%
    #       read_html() %>%
    #       html_nodes(pagecontent_css_selector) %>%
    #       html_text() %>%
    #       toString()
    #   })



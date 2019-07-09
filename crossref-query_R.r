#!/bin/R

library(curl)
library(rjson)
library(stringr)

## PARAMETERS TO CHANGE
queries <- c('systematic review', 'meta analysis', 'search strategy', 'narrative review')
years = 2009:2010
user_email = 'chris@libscie.org'
## read in the downloaded copy of the Google Spreadsheet
issns <- read.csv('cr-query-issn.csv', stringsAsFactors = FALSE)

## Do Not CHANGE BELOW
## OUTPUT in working folder

base_url = 'https://api.crossref.org/works'
page_size = 10

queries <- str_replace_all(string = queries, pattern = '\\s', replacement = '+')
query_txt <- NULL
for ( i in 1:length(queries) ) {
    query_txt <- paste0(query_txt, '&query.title=', queries[i])
}

write('year,doi,journal', 'doi-datafile.csv', append = FALSE)
write('total,url,time', 'doi-logfile.csv', append = FALSE)

for ( i in 1:dim(issns)[1] ) {

    for ( y in years ) {
        obj <- sprintf('issn:%s', issns[i,2:3])
        sel <- grepl(x = obj, pattern = '(issn:)$')
        if ( sum(sel) > 0 ) {
            obj <- obj[1]
        } else {
            obj <- paste(obj[1], obj[2], sep = ',')
        }
        
        issn_filters <- obj

        url_main <- sprintf("%s?filter=type:%s,from-pub-date:%s,until-pub-date:%s,%s&select=DOI&rows=%s&mailto=%s%s",
                            base_url,
                            'journal-article',
                            y, y,
                            issn_filters,
                            page_size,
                            user_email,
                            query_txt)

        url <- sprintf('%s&cursor=*', url_main)

        obj_len <- page_size
        while ( obj_len == page_size ) {
            if ( exists('cursor') ) {
                url <- sprintf('%s&cursor=%s', url_main, cursor)
            }
            
            obj <- curl_fetch_memory(url)

            headers <- parse_headers(obj$headers)
            rate_limit_txt <- headers[grepl(x = headers, pattern = 'X-Rate-Limit-Interval')]
            rate_limit <- stringr::str_extract(rate_limit_txt, pattern = '\\d+')

            content <- fromJSON(rawToChar(obj$content))
            items <- unlist(content)
            dois <- as.vector(items[grepl(x = names(items), pattern = 'message.items.DOI')])

            txt <- sprintf('%s,%s,%s',
                           y,
                           dois,
                           issns$journal[i])
           
            write(txt, 'doi-datafile.csv', append = TRUE)
            
            txt <- sprintf('"%s","%s","%s"',
                           content$message$`total-results`,
                           url,
                           Sys.time())
            write(txt, 'doi-logfile.csv', append = TRUE)
            
            cursor <- content$message$`next-cursor`
            obj_len <- length(dois)
            
            cat(sprintf('Got %s results for %s in year %s \n', length(dois), issns$journal[i], y))
            cat(sprintf('Being nice and adhering to the rate limit of %s seconds \n', rate_limit))
            cat('Going to next page or next query\n')
            ## add sleep
            Sys.sleep(rate_limit)
        }
        
    }
}

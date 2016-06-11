GetLastCompletedAuctionNumberFromActive <- function(){
    
    ### While this works, it might be best to scrape all the completed 
    ###  auction numbers instead
    
    ## This function will get the index page from SixBid and determine
    ##   the lowest number of an auction on that page.  From that you 
    ##   can determine the last completed auction (which is one less 
    ##   than the lowest value)
    
    suppressWarnings(library(rvest, quietly = TRUE, warn.conflicts = FALSE))
    suppressWarnings(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
    
    urlSearch <- "https://www.sixbid.com/index.html"
    
    # Get the web page
    html <- read_html(urlSearch, verbose = TRUE)
    # Get the <li> tags
    nodes <- html_nodes(x = html, css = ".list_title")
    df<- bind_rows(lapply(xml_attrs(nodes), 
                          function(x) data.frame(as.list(x), 
                                                 stringsAsFactors=FALSE)))
    auctions <- df$href[!(is.na(df$href))]
    auctions <- auctions[grepl(pattern = "browse.html\\?auction=[0-9]{1,4}",
                               x = auctions)]
    auctions <- gsub(pattern = "browse.html\\?auction=",
                     replacement = "",
                     x = auctions)
    minAuction <- min(as.numeric(auctions))
    return(minAuction - 1)
}

GetCompletedAuctionNumbers <- function(){
    
    ## This will get all the completed auction numbers
    
    suppressWarnings(library(rvest, quietly = TRUE, warn.conflicts = FALSE))
    suppressWarnings(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
    
    urlSearchStart <- "https://www.sixbid.com/index.html?v=completed"
    
    # Read in the completed auction numbers
    done <- read.csv(file = "CompletedAuctions.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE,
                     colClasses = "character")
    i <- 1
    page <- NULL
    repeat{
        if(i > 1){page <- paste("&page=", i, sep="")}
        urlSearch <- paste(urlSearchStart, page, sep = "")
        
        # Get the web page
        html <- read_html(urlSearch, verbose = TRUE)
        # Get the <li> tags
        nodes <- html_nodes(x = html, css = ".list_title")
        df<- bind_rows(lapply(xml_attrs(nodes), 
                              function(x) data.frame(as.list(x), 
                                                     stringsAsFactors=FALSE)))
        auctions <- df$href[!(is.na(df$href))]
        auctions <- auctions[grepl(pattern = "browse.html\\?auction=[0-9]{1,4}",
                                   x = auctions)]
        auctions <- gsub(pattern = "browse.html\\?auction=",
                         replacement = "",
                         x = auctions)
        auctions <- setdiff(auctions, done[, 1])
        if(length(auctions) > 0){
            done <- rbind(done, list(auctions))
            i <- i + 1
        }
        if(length(auctions) == 0){
            break
        }
    }
    
    # Write the list of completed auctions to file
    write.csv(x = done, file = "CompletedAuctions.csv", row.names = FALSE)
}
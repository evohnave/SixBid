### getWebPage and getDfFromNodes are generics for the main workhorse for
###   scraping

# Generic constants
baseUrl <- "https://www.sixbid.com/"
auctionUrl <- "browse.html?auction="
imageUrlStart <- "images/auction_images/"
imageUrlEnd <- "l.jpg"
srchUrl <- "&search="

getWebPage <- function(url){
    ## Gets a web page, returns the html
    suppressWarnings(library(rvest, quietly = TRUE, warn.conflicts = FALSE))
    return(read_html(x = url, verbose = TRUE))
}

getDfFromNodes <- function(html, css){
    ## returns a data frame with the nodes extracted according to the
    ##  css provided
    
    suppressWarnings(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
    
    nodes <- html_nodes(x = html, css = css)
    df<- bind_rows(lapply(xml_attrs(nodes), 
                          function(x) data.frame(as.list(x), 
                                                 stringsAsFactors=FALSE)))
    
    return(df)
}

getDfFromNodesXpath <- function(html, xpath){
    ## returns a data frame with the nodes extracted according to the
    ##  css provided
    
    suppressWarnings(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
    
    nodes <- html_nodes(x = html, xpath = xpath)
    df<- bind_rows(lapply(xml_attrs(nodes), 
                          function(x) data.frame(as.list(x), 
                                                 stringsAsFactors=FALSE)))
    
    return(df)
}

createAuctionSearchUrl <- function(auction, searchTerm) {
    ## Creates the url for searching an auction
    
    return(paste(baseUrl, auctionUrl, auction, srchUrl, searchTerm, sep = ""))
}

getAuctionLots <- function(auction, searchTerm){
    
    ## Gets the auction lots and returns them
    
    searchURL <- createAuctionSearchUrl(auction = auction,
                                        searchTerm = searchTerm)
    
    html <- getWebPage(url = searchURL)
    
    css <- "img"
    
    df <- getDfFromNodes(html = html,
                         css = css)
    patternL <- imageUrlStart
    patternR <- "/[0-9]{1,10}[l,m,s].jpg"
    pattern <- paste(patternL, auction, patternR, sep = "")
    
    lots <- df$src[!(is.na(df$src))]
    lots <- lots[grepl(pattern = pattern,x = lots)]
    if(length(lots) == 0){
        # no lots returned
    } else {
        # lots returned
        # get rid of the left part
        lots <- gsub(pattern = paste(patternL, auction, "/", sep = ""),
                     replacement = "", x = lots)
        # get rid of the right part
        lots <- gsub(pattern = "[l,m,s].jpg",
                     replacement = "",
                     x = lots)
    }
    return(lots)
}

buildLotUrl <- function(auction, lot){
    return(paste(baseUrl, auctionUrl, auction, "&lot=", lot, sep =""))
}
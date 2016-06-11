GetUsedNumbers <- function(){
    
    # This function will get item numbers from eBay that have been used.  The
    #   thought is that eBay will not reuse numbers, so it should be safe to
    #   use these "used" numbers for controling items in the database that 
    #   don't come from eBay
    
    suppressWarnings(library(rvest, quietly = TRUE, warn.conflicts = FALSE))
    suppressWarnings(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
    
    # Search String, split over several lines for readability
    urlSta <- "http://www.ebay.com/sch/i.html?_from=R40&_nkw="
    urlSch <- "lawn"   # Change to find others
    # urlSch <- "garden"
    urlMid <- "&_in_kw=1&_ex_kw=&_sacat=11700&LH_Sold=1&LH_Auction=1"
    urlEnd <- "&_sadis=15&_salic=1&_sop=10&_dmd=1&_ipg=200&LH_Complete=1"
    urlPge <- "&_pgn="
    # paste together search string
    urlSearch <- paste(urlSta, urlSch, urlMid, urlEnd, sep = "")
    
    artNums <- NULL
    # Loop 10 times - should get 2000 item numbers
    for(i in 1:10){
        urlSearch <- paste(urlSearch, urlPge, i, sep = "")
        # Get the web page
        html <- read_html(urlSearch, verbose = TRUE)
        # Get the <li> tags
        nodes <- html_nodes(x = html, xpath = "//li")
        df<- bind_rows(lapply(xml_attrs(nodes), 
                              function(x) data.frame(as.list(x), 
                                                     stringsAsFactors=FALSE)))
        artNums <- rbind(artNums, df$listingid[!(is.na(df$listingid))])
    }
    return(artNums)
}

### First run, only
## Make sure you don't uncomment stuff like this...
##  it will run it when you source the file!
# To setup the csv file
# nums <- GetUsedNumbers()
# write.csv(x = as.vector(nums),
#           file = "AvailableNumbers.csv",
#           row.names = FALSE)
#
###

updateNums <- function(){
    
    ## This function will update the available item numbers in the
    ##   CSV file _if_necessary_ and returns the available numbers
    ##   as a vector
    
    # Read in the available item numbers
    nums <- read.csv(file = "AvailableNumbers.csv",
                     header = TRUE, stringsAsFactors = FALSE,
                     colClasses = "character")
    # Check that you have at least 1000 item numbers available
    if(dim(nums)[1] < 1000){
        # Get 2000 new numbers
        newNums <- GetUsedNumbers()
        # Bind the new numbers to the old, keep only the unique
        #  hopefully, you now have more than 1000 numbers
        nums <- unique(rbind(nums, as.vector(newNums)))
        # Write to CSV
        write.csv(x = as.vector(nums),
                  file = "AvailableNumbers.csv",
                  row.names = FALSE)
    }
    return(nums)
}

writeNums <- function(nums){
    
    ## This function writes nums to CSV.  Execute after using a number
    
    write.csv(x = as.vector(nums),
              file = "AvailableNumbers.csv",
              row.names = FALSE)
    
}
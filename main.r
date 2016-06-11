source(file = "SixBidGenerics.r")
require(dplyr)
require(stringi)

## OK, this works, but need to make it more generic
##  also, put in a loop to get all of the lots etc

# image is images/auction_images/####/##########m.jpg

auction <- 2699
searchTerm <- "seljuq"

lots <- getAuctionLots(auction = auction, searchTerm = searchTerm)

# a lot is https://www.sixbid.com/browse.html?auction=____&lot=______

if(length(lots) > 0){print("not null")}

lot <- lots[1]
# Image data
imageUrl <- paste(baseUrl, imageUrlStart, auction, "/", lot, "l.jpg", sep = "")
imageFileName <- paste("J:/Sixbid/images/", lot, ".jpg", sep = "")
# Save image to directory
download.file(url = imageUrl, destfile = imageFileName, mode = "wb")

# Lot data
lotUrl <- buildLotUrl(auction = auction, lot = lot)
lotFileName <- paste("J:/Sixbid/html/", lot, ".htm", sep = "")
# Saves file to directory
download.file(url = lotUrl, destfile = lotFileName, mode = "wb")

# Since we downloaded the file, we don't need to get the web page too
#html <- getWebPage(lotUrl)
html <- getWebPage(lotFileName)

theTable <- html_table(html, trim = TRUE, fill = TRUE)[2][[1]]
# Note: I could use X3 for all, but then there's lots of stuff to get rid of
# allData <- iconv(theTable$X10[1], from = "UTF-8", to = "latin1")
allData <- theTable$X10[1]
auctionId <- iconv(theTable$X3[1], from = "UTF-8", to = "latin1")

# get estimate
estimate <- allData %>% 
    gsub(pattern = "\\r\\n", replacement = "") %>%  # remove \r\n
    stri_extract_first(regex = "(?<=Estimate: )([0-9]{1,5} USD\\s{0,5})") %>%
    stri_trim()

# get starting price
startPrice <- allData %>% 
    gsub(pattern = "\\r\\n", replacement = "") %>%  # remove \r\n
    stri_extract_first(regex = "(?<=Starting price: )([0-9]{1,5} USD\\s{0,5})") %>%
    stri_trim()

# get price realized
priceRealized <- allData %>% 
    gsub(pattern = "\\r\\n", replacement = "") %>%  # remove \r\n
    stri_extract_first(regex = "(?<=Price realized: )([0-9]{1,5} USD\\s{0,5})") %>% 
    stri_trim()

## get description
descPattern <- "(?<=\\* Date: [0-9]{4}-[0-9]{2}-[0-9]{2})(.{1,1000})" 
description <- allData %>% 
    gsub(pattern = "\\r\\n", replacement = "") %>%  # remove \r\n
    stri_extract_last(regex = descPattern) %>%      # find description
    stri_trim()                                     # trim

# get lot # in the auction
lot <- auctionId %>% stri_extract(regex = "(?<=Lot )[0-9]{1,4}")
regMonths <- "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|(Nov|Dec)(?:ember)?)"
regDate <- paste("([0-9]{2} ", regMonths, " [0-9]{4})", sep = "")
# get auction date
auctionDate <- auctionId %>% stri_extract_first(regex = regDate)

# get auction 
whr <- stri_locate(str = auctionId, regex = "\\|")[1] - 1
regexAuct <- paste("(?<!\\|)\\D{1,", whr, "}", sep = "")
auctioneer <- auctionId %>% stri_extract(regex = regexAuct) %>% stri_trim()

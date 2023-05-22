library(lubridate)
library(purrr)
library(dplyr)
library(stringr)
library(httr2)

# Load key and token API secrets from Github
Key <- Sys.getenv("TRELLOKEY")
Token <- Sys.getenv("TRELLOTOKEN")
Board <- "6438b578e8f2790b2f230085"

# Load boards - not needed
# BoardTest <- httr::GET(paste0("https://api.trello.com/1/members/me/boards?key=", Key, "&token=", Token))
# BoardData <- httr::content(BoardTest)



# Load lists
ListTest <- httr2::request(paste0("https://api.trello.com/1/boards/", Board,
                             "/lists?fields=id,name&key=", Key, 
                             "&token=", Token)) %>%
  httr2::req_perform()

ListData <- httr2::resp_body_json(ListTest)

ListDF <- purrr::map_dfr(ListData,
                         function(x) tibble::tibble(id = x$id, name = x$name))



# Load cards
CardTest <- httr2::request(paste0("https://api.trello.com/1/boards/", Board, 
                             "/cards?fields=name,labels,idList&key=", Key,
                             "&token=", Token)) %>%
  httr2::req_perform()

CardData <- httr2::resp_body_json(CardTest)

CardDF <- purrr::map_dfr(CardData,
                         function(x) tibble::tibble(id = x$id, 
                                                    name = x$name, 
                                                    label = x$labels[[1]]$name, 
                                                    idList = x$idList)) %>%
  
  # Join list name to cards
  dplyr::left_join(ListDF %>%
                     dplyr::rename(ListName = name),
                   by = c("idList" = "id"))



# Convert into README table
## Create table columns - generate columns as character vector where each element 
## is a new line
ListTitles <- dplyr::pull(ListDF, name)
ListOfColumns <- purrr::map(ListTitles,
                            function(ListNameToFilter, CardDF) {
                              
                              CardsInList <- CardDF %>%
                                
                                dplyr::filter(ListName == ListNameToFilter) %>%
                                
                                dplyr::mutate(FullCardDetails = paste0(label,
                                                                       ": ",
                                                                       name)) %>%
                                
                                dplyr::pull(FullCardDetails)
                              
                            }, CardDF = CardDF)


## Extend list of columns to be equal and add piping
MaxListLength <- max(purrr::map_dbl(ListOfColumns, length))
EqualListOfColumns <- purrr::map(ListOfColumns,
                                 function(x, MaxLength) {
                                   
                                   c(paste0("| ", x, " "), rep("|   ", 
                                                               MaxListLength - length(x)))
                                   
                                 }, MaxLength = MaxListLength)


## Header row
HeaderRow <- purrr::map(ListTitles, 
                        function(ListName) {
                          
                          c(paste0("| ", ListName, " "),
                            "| --- ")
                          
                        })


## Combine together
### Header part
HeaderTableComponent <- paste0(paste0(purrr::map_chr(HeaderRow, 1), collapse = ""), 
                               "|\n", 
                               paste0(purrr::map_chr(HeaderRow, 2), collapse = ""), 
                               "|\n", 
                               collapse = "")


## Table part
RowsTableComponent <- purrr::map_chr(1:length(EqualListOfColumns[[1]]),
                                     function(RowNumber, RowTableData) {
                                       
                                       paste0(purrr::map_chr(RowTableData, RowNumber), 
                                              collapse = "") %>%
                                         paste0("|\n", collapse = "")
                                       
                                     }, RowTableData = EqualListOfColumns) %>%
  paste0(collapse = "")


## Combine into final output
FinalTableOutput <- paste0("**Personal board**\n", 
                           HeaderTableComponent, 
                           RowsTableComponent, 
                           "\n*Last updated: ", 
                           as.character(Sys.time()) %>%
                             lubridate::as_datetime(tz = Sys.timezone()) %>%
                             lubridate::with_tz(tzone = "Australia/Sydney") %>%
                             format(format = "%Y-%m-%d %H:%M %Z"),
                           "*",
                           collapse = "")



# Insert into README
## Read README
READMEString <- readLines("README.md") %>%
  paste0(collapse = "\n")


## Insert into README
READMEString <- gsub(pattern = "<!--TrelloBoard-->.*<!--END-->",
                     replacement = paste0("<!--TrelloBoard-->\n", FinalTableOutput, "\n<!--END-->"),
                     x = READMEString) %>%
  stringr::str_split(pattern = "\n") %>%
  unlist()

writeLines(READMEString, con = file("README.md"))

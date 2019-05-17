list.of.packages <- (c("httr","jsonlite","stringr","dplyr","tidyr","lubridate"))
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages,library, character.only = TRUE))

#insert access key here between the quotes
accesskey<-""

tokenurl <- "https://www.ura.gov.sg/uraDataService/insertNewToken.action"
get_token <- GET(tokenurl, add_headers(AccessKey = accesskey))
get_token_text <- content(get_token,"text", encoding = "UTF-8")
token <- strsplit(get_token_text, split = '["]')[[1]][4]

#get current system year and month
year<-as.character(format(Sys.Date(), '%y'))
month <- as.numeric(format(Sys.Date(),'%m'))

#converting month to quarter
if (month <= 1) {
  quarter = "q4"
} else if (month <=4) {
  quarter = "q1"
} else if (month <=7) {
  quarter = "q2"
} else if (month <=10) {
  quarter = "q3"
} else if (month <=12) {
  quarter = "q4"
} else {
  ""
}

#if in january, draw last year's data
if (month <= 1){
  year<-as.character(as.numeric(format(Sys.Date(), '%y'))-1)
}

#override automatic year quarter selection here. eg: refPeriod <- "19q1"
refPeriod <- paste(year,quarter, sep = "")

PRPRCurl <- paste("https://www.ura.gov.sg/uraDataService/invokeUraDS?service=PMI_Resi_Rental&refPeriod=",refPeriod, sep = "")
PvtResiPropRentCont <- GET(PRPRCurl, add_headers(AccessKey = accesskey, Token = token))
PvtResiPropRentCont_text <-content(PvtResiPropRentCont, "text", encoding = "UTF-8")

PRPRC_json <- fromJSON(PvtResiPropRentCont_text, flatten = TRUE)
df_prprc <- unnest(PRPRC_json$Result,rental)

#getting only the latest month instead of entire quarter
df_prprc$leaseDate <- as.POSIXct(fast_strptime(df_prprc$leaseDate, "%m%y"))
df_prprc %>%
  select(everything()) %>%
  filter(leaseDate == max(leaseDate)) -> prprc_export
prprc_export$leaseDate <-format(as.Date(prprc_export$leaseDate), "%m%y")

#reorder columns
prprc_export <- prprc_export[c(8,7,10,5,11,6,9,1,2,3,4)]

#export to csv
write.csv(prprc_export,paste("URA", max(prprc_export$leaseDate), ".csv"), na="", quote = FALSE,row.names=FALSE)
paste("You can find your output file in", getwd())

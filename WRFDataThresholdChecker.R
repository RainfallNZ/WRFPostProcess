#R script to check for WRF output variables that have crossed a threshold.
#Prepared by Tim Kerr
#Initial version 20th January 2020
#Copyright 2020 Rainfall.NZ

#Load libraries
{
if (!require(rdrop2)) install.packages('rdrop2'); library(rdrop2) #Package to enable DropBox access
if (!require(httpuv)) install.packages('httpuv'); library(httpuv) #Package to enable authenticating DropBox access via drop2
##Note, on Sabalcore, before I could install the blastula package I had to install Rcpp package.
#if (!require(Rcpp)) install.packages('Rcpp'); library(Rcpp) #Required to ensure a correct version of GLIBXX_3.4.20 is available, apparently if it was compiled with a different c++ compiler then it is problematic. See http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2016-March/009148.html
#Sabalcore also required the "stringi" package to be installed
  if (!require(blastula)) install.packages('blastula'); library(blastula) #Package to enable emailing. 
  #if (!require(stringi)) install.packages('stringi'); library(stringi) #Required to ensure a correct version of ABXX_3.4.20 is available.
  
if (!require(keyring)) install.packages('keyring'); library(keyring) #Package to enable emailing. I needed to install libsodium-dev on my Ubuntu laptop for this to install, and libsecret-1-dev to make it work properly. On Sabalcore it requires loading the "libsodium" module prior to using R and running this script. But it still doesn't work. I've requested installation of libsecret-devel
if (!require(knitr)) install.packages('knitr'); library(knitr) #Package to enable nice formatting of tables
  if (!require(kableExtra)) install.packages('kableExtra'); library(kableExtra) #Package to enable nice formatting of tables
  }

#A handy function
minpositive = function(x) min(x[x > 0])

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#' A function to send an alert email
#'
#'This function sends an email with a preset sender and overall template given the subject, email message, and data table to be sent.
#'
#'@param EmailSubject Subject for the email. A character vector. Defaults to "Alert Email".
#'@param EmailMessage The message body for the email. A character vector. Defaults to "Alert test".
#'@param EmailAlertTable A table to be sent with the email. An html-formatted data frame. Defaults to the first 4 lines of the cars data frame.
#'@param to The email addresses that the email is to be sent to. A vector of character strings. Defaults to "timkerr37@hotmail.com"
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return None
#'@keywords Email alert
#'@examples
#'AlertEmail()
#'@export
AlertEmail <- function(EmailSubject="Alert Email",EmailMessage="Alert test",EmailAlertTable=kable(cars[1:4,],"html"),to=c("timkerr37@hotmail.com")){
  #Find which rows are the first rows for each date
  
  FormattedTable <- EmailAlertTable
  email <-
    compose_email(
      body = md(c(EmailMessage,"***",
      {FormattedTable},"

***      
This is an automated alert Email sent on behalf of:

Mike Green  
Engineering Meteorologist  
Meteorology Solutions Ltd.  
T:021 249 9248  

mike.green@metsolutions.co.nz
")),
      footer = md(
        "
[metsolutions.co.nz](https://metsolutions.co.nz)
")
    )
  
  # The email message can always be
  # previewed by calling the object
  if (interactive()) email
  
  ##One time setup of a key pair to enable automated email sending
  ##Note that there is a bug in the Blatsula package so that only one smtp server key can be held at a time
  ##seehttps://github.com/rich-iannone/blastula/issues/118
  ##keys can be listed using key_list()
  ##keys can be deleted using key_delete(service="sfsdfd",username="sdfsdfsd")
  #create_smtp_creds_key(id="TimsAutoEmailKey",user="tim.kerr@rainfall.nz",host="mailx.freeparking.co.nz",port=465, use_ssl = TRUE)
  #create_smtp_creds_key(id="MikesAutoEmailKey",user="noreply@metsolutions.co.nz",host="mail.1stdomains.co.nz",port=465, use_ssl = TRUE)
  #create_smtp_creds_file(file="EmailCreds.txt",user="noreply@metsolutions.co.nz",host="mail.1stdomains.co.nz",port=465, use_ssl = TRUE)
  
    
  email %>%
    smtp_send(
      #from =       "tim.kerr@rainfall.nz",
      from =        "noreply@metsolutions.co.nz",
      to =          to,
     # bcc =        "tim.kerr@rainfall.nz","mike.green@metsolutions.co.nz"
      subject =     EmailSubject,
      #credentials = creds_key("MikesAutoEmailKey")
      credentials = creds_file("EmailCreds.txt")
    )
  return()
}

#A function to test for an Alert condition
#'
#'This function tests whether an alert condition exists in a WRF output file.
#'@param SiteName The name of the site for which the data is to be tested. Must match the names used in the WRF data file. A character vector. Defaults to "Arthurs_Pass".
#'@param Parameter The forecast parameter to be checked for threshold crossing. Must match the parameters in the WRF data file. A character vector. Defaults to "Temp" (for temperature). 
#'@param Threshold The value with which the parameter is to be threholded against. Numeric. Defaults to 2.
#'@param Above Whether the email is sent out when the parameter is above (TRUE) or below (FALSE) the threshold value. Defaults to TRUE
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return Returns a list of the alert status, a list of alert metadata, and a data frame of the times and values when the threshold condition was met.
#'@keywords Email alert WrF
#'@examples
#'TestForAlert()
#'@export
#'
TestForAlert <- function(SiteName = "Arthurs_Pass",Parameter="Temp",Threshold=2,Above=FALSE){
  
  #Figure out the parameter long name and the units to be used
  ParameterAttributes <- data.frame(Parameters = c("Temp","WindSpd","WindDir","Rain","RH","SWDOWN"),
                                    ParameterLongNames = c("temperature", "wind speed", "wind direction", "rain", "relative humidity", "solar radiation"),
                                    Units = c("degrees C","metres per second", "degrees from North", "millimetres", "percent", "joules per square metre"),
                                    stringsAsFactors = FALSE)
  ParameterIndex <- which(ParameterAttributes$Parameters == Parameter)
  
  #Figure out the site long name and the units to be used
  SiteAttributes <- data.frame(SiteNames = c("Arthurs_Pass","Castle_Hill","Lewis_Pass","Desert_Road"),
                               SiteLongNames = c("Arthur's Pass", "Poerters Pass", "Lewis Pass", "Central North Island"),
                               stringsAsFactors = FALSE)
  SiteIndex <- which(SiteAttributes$SiteNames == SiteName)
  
  ##Create an authentication token for later use
  #token <- drop_auth()
  #saveRDS(token, )
  
  #Get the latest csv file from DropBox
  {
    #Figure out the filename of the latest file of interest
    
    #Get a list of all the files
    DropBoxFiles <- drop_dir(path="Rainfall.NZ/WRF_data/")
    
    #Find which ones have a csv suffix
    FilesOfInterest <- which(endsWith(DropBoxFiles$path_display,".csv"))
    
    #Get the metadata for all the csv files
    MetadataOfFilesOfInterest <- lapply(DropBoxFiles$path_lower[FilesOfInterest], drop_get_metadata)
    
    #Get the date-times of the csv files from the metadata
    DatesOfFilesOfInterest <- sapply(MetadataOfFilesOfInterest, "[[", "client_modified")
    
    #Set the datetimes as date-time objects
    Dates <- as.POSIXct(DatesOfFilesOfInterest, format="%Y-%m-%dT%H:%M:%SZ",tz="utc")
    
    #Find which of the csv files has the latest date-time
    MaxDateIndex <- which(Dates == max(Dates))
    
    #Find the filename of the csv file with the latest date
    LatestFile <- DropBoxFiles$path_display[FilesOfInterest[MaxDateIndex]]
    
    #Download the latest csv file
    drop_download(path=LatestFile, file.path(tempdir(),"wrf_file"),overwrite=TRUE)
  }
  
  #Read in the file
  WRFData <- readLines(file.path(tempdir(),"wrf_file"))
  
  #Find all the lines that start different site sections
  AllStartLines <- which(endsWith(WRFData, "(Date/Time in UTC),"))
  
  #Find the line that relates to the site of interest
  StartLine <- which(startsWith(WRFData, SiteName))
  
  #Find the last line for the site of interest
  NoOfLines <- minpositive(AllStartLines - StartLine) - 2
  
  #Read in the data for the site of interest
  SiteSpecificData <- read.table(file=file.path(tempdir(),"wrf_file"),skip = StartLine, nrows = NoOfLines, sep=",", header=TRUE, stringsAsFactors = FALSE)
  
  #Find what times the threshold was crossed
  if (Above) ThresholdedIndices <- which(SiteSpecificData[,Parameter] > Threshold) else
    ThresholdedIndices <- which(SiteSpecificData[,Parameter] < Threshold)
  if (length(ThresholdedIndices) > 0) {
    Status = TRUE 
  
  #Convert the data to NZ time zone
  ThresholdedDateTimes <- as.POSIXct(paste(SiteSpecificData[ThresholdedIndices,'Date'],SiteSpecificData[ThresholdedIndices,'Time']),tz="utc")
  attr(ThresholdedDateTimes, "tzone") <- "nz"
  #ThresholdedDateTimes <-  format(ThresholdedDateTimes, "%d %b at %I %p")
  
  #Select the data of interest
  ThresholdedValues <- round(SiteSpecificData[ThresholdedIndices,Parameter],1)
  
  #Create a dataframe of the times and data
  ThresholdedData <- data.frame(Time = ThresholdedDateTimes, Values = ThresholdedValues,check.names = FALSE, stringsAsFactors = FALSE)
 
  }else {
    Status = FALSE
    ThresholdedData <- NULL
  }
  
  AlertOutput <- list(Status = Status,
                      Metadata =list(SiteName=SiteName,
                           SiteLongName=SiteAttributes$SiteLongNames[SiteIndex],
                           Parameter=Parameter,
                           ParameterLongName=ParameterAttributes$ParameterLongNames[ParameterIndex],
                           ParameterUnits=ParameterAttributes$Units[ParameterIndex],
                           ThresholdValue=Threshold,
                           ThresholdAbove=Above),
                      Data=ThresholdedData)
  return(AlertOutput)
}

#' Prepare the contents of an Alert Email
#' 
#' This function takes the output of the TestForAlert function and formats the information ready for an email to be sent
#'@param AlertData A list of the alert status, a list of alert metadata, and a data frame of the times and values when the threshold condition was met.
#'@author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#'@return Returns a list of an email subject, email message and a 2-column table for inclusion in the email
#'@keywords Email alert WrF
#'@examples
#'PrepareAlertEmailContents()
#'@export
#'
PrepareAlertEmailContents <- function(AlertData=list(Status = TRUE,
                                                     Metadata =list(SiteName="MySite",
                                                                 SiteLongName="My Site Where I live",
                                                                 Parameter="Temp",
                                                                 ParameterLongName="temperature",
                                                                 ParameterUnits="degrees C",
                                                                 ThresholdValue=2,
                                                                 ThresholdAbove=FALSE),
                                                     Data=data.frame(Time=as.POSIXct("2020-06-21 05:50"),Values=-5))){
  

  #browser()
  SiteName <-   AlertData$Metadata$SiteLongName
  Above <-      AlertData$Metadata$ThresholdAbove
  ParameterLongName <- AlertData$Metadata$ParameterLongName
  Threshold <-  AlertData$Metadata$ThresholdValue
  Units <-      AlertData$Metadata$ParameterUnits
    
  #Prepare the information for the email
    EmailSubject <- paste(SiteName, if(Above) {"high"} else {"low"}, ParameterLongName,"notification")
    
    EmailMessage <- paste("The",ParameterLongName,"at",SiteName,"is forecast to drop below",Threshold,Units, "at the following times over the next three days") 
    
    #Break the Date-Time into date and time
    EmailTable <- AlertData$Data
    Dates <- format(EmailTable[,1],"%d %b ")
    Times <- format(EmailTable[,1], "at %I %p")
    #Remove the original Date-Time column
    EmailTable <- EmailTable[,-1,drop=FALSE]
    EmailTable <- cbind(Dates,Times,EmailTable)
    #Rename the last column to match the parameter name
    names(EmailTable)[3] <- simpleCap(ParameterLongName)
    
    #Find the row numbers of the first instance of each date
    FirstDateRows <- match(unique(EmailTable$Dates),EmailTable$Dates)
    FormattedEmailTable <-kable(EmailTable,format="html") %>%
      collapse_rows(columns = 1, valign = 'top') %>%
      row_spec(FirstDateRows,extra_css = "border-top:1px solid")
      
    
    #print(EmailContents["EmailSubject"])
    #print(EmailContents["EmailMessage"])
    #print(EmailContents["EmailTable"])
    return(list(EmailSubject=EmailSubject,
                   EmailMessage=EmailMessage,
                   EmailAlertTable=FormattedEmailTable))

} 

#**************
#The Main Event
#**************

#Test for Alert Condition at Arthur's Pass
AlertCondition <- TestForAlert(SiteName = "Arthurs_Pass",Parameter="Temp",Threshold=15,Above=FALSE)

if (AlertCondition$Status){

  #Prepare Email contents
  EmailContents <- PrepareAlertEmailContents(AlertCondition)

  #send the alert email
  AlertEmail(EmailSubject=EmailContents$EmailSubject,EmailMessage=EmailContents$EmailMessage,EmailAlertTable=EmailContents$EmailAlertTable, to =c("timkerr37@hotmail.com","tim.kerr@rainfall.nz","mike.green@metsolutions.co.nz"))
}

#Test for Alert Condition at Arthur's Pass
AlertCondition <- TestForAlert(SiteName = "Castle_Hill",Parameter="Temp",Threshold=20,Above=FALSE)

if (AlertCondition$Status){
  
  #Prepare Email contents
  EmailContents <- PrepareAlertEmailContents(AlertCondition)
  
  #send the alert email
  AlertEmail(EmailSubject=EmailContents$EmailSubject,EmailMessage=EmailContents$EmailMessage,EmailAlertTable=EmailContents$EmailAlertTable)
}
  


    
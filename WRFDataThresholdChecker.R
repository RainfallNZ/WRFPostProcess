#R script to check for WRF output variables that have crossed a threshold.
#Prepared by Tim Kerr
#Initial version 20th January 2020
#Copyright 2020 Rainfall.NZ


if (!require(rdrop2)) install.packages('rdrop2'); library(rdrop2) #Package to enable DropBox access
if (!require(httpuv)) install.packages('httpuv'); library(httpuv) #Package to enable authenticating DropBox access via drop2
if (!require(blastula)) install.packages('blastula'); library(blastula) #Package to enable emailing
if (!require(keyring)) install.packages('keyring'); library(keyring) #Package to enable emailing
if (!require(knitr)) install.packages('knitr'); library(knitr) #Package to enable nice formatting of tables

#A handy function
minpositive = function(x) min(x[x > 0])

AlertEmail <- function(EmailSubject="Alert Email",EmailContent="Alert test",EmailAlertTable=cars[1:4,]){
  #A function to send an email
  # Create a simple email message using
  # Markdown-formatted text in the `body`
  # and `footer` sections with the `md()`
  # text helper function
  FormattedTable <- kable(EmailAlertTable, format="html")
  email <-
    compose_email(
      body = md(c(EmailContent,"***",
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
  
  email %>%
    smtp_send(
      #from = "tim.kerr@rainfall.nz",
      from = "noreply@metsolutions.co.nz",
      to = "timkerr37@hotmail.com",
      subject = EmailSubject,
      credentials = creds_key("MikesAutoEmailKey")
    )
}

#Set the site of interest
SiteName <- "Arthurs_Pass"

#Set the parameter of interest
Parameter <- "Temp"

#Set the threshold
Threshold <- 12

#Set whether that alarm is to be above or below the threshold
Above <- FALSE

#Figure out the parameter long name and the units to be used
ParameterAttributes <- data.frame(Parameters = c("Temp","WindSpd","WindDir","Rain","RH","SWDOWN"),
                                  ParameterLongNames = c("temperature", "wind speed", "wind direction", "rain", "relative humidity", "solar radiation"),
                                  Units = c("degrees C","metres per second", "degrees from North", "millimetres", "percent", "joules per square metre"),
                                  stringsAsFactors = FALSE)
ParameterIndex <- which(ParameterAttributes$Parameters == Parameter)

#Figure out the parameter long name and the units to be used
SiteAttributes <- data.frame(SiteNames = c("Arthurs_Pass","Porters_Pass","Lewis_Pass","Desert_Road"),
                                  SiteLongNames = c("Arthur's Pass", "Porters Pass", "Lewis Pass", "Central North Island"),
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

SiteSpecificData <- read.table(file=file.path(tempdir(),"wrf_file"),skip = StartLine, nrows = NoOfLines, sep=",", header=TRUE, stringsAsFactors = FALSE)
if (Above) ThresholdedIndices <- which(SiteSpecificData[,Parameter] > Threshold) else
  ThresholdedIndices <- which(SiteSpecificData[,Parameter] < Threshold)

if (length(ThresholdedIndices) > 0){
  #Convert the data to NZ time zone
  ThresholdedDateTimes <- as.POSIXct(paste(SiteSpecificData[ThresholdedIndices,'Date'],SiteSpecificData[ThresholdedIndices,'Time']),tz="utc")
  attr(ThresholdedDateTimes, "tzone") <- "nz"
  ThresholdedDateTimes <-  format(ThresholdedDateTimes, "%d %b at %I %p")
  
  #Select the data of interest
  ThresholdedData <- round(SiteSpecificData[ThresholdedIndices,Parameter],1)
  
  #Prepare the information for the email
  EmailSubject <- paste(SiteAttributes$SiteLongNames[SiteIndex], if(Above) {"high"} else {"low"}, ParameterAttributes$ParameterLongNames[ParameterIndex],"notification")
  
  EmailMessage <- paste("The",ParameterAttributes$ParameterLongNames[ParameterIndex],"is forecast to drop below",Threshold,ParameterAttributes$Units[ParameterIndex], "at the following times over the next three days") 
  
  EmailTable <- data.frame('Time' = ThresholdedDateTimes, Parameter = ThresholdedData,check.names = FALSE)
  names(EmailTable)[2] <- ParameterAttributes$ParameterLongNames[ParameterIndex] 
  
  #Call a yet-to-be-written email function
  print(EmailSubject)
  print(EmailMessage)
  print(EmailTable)
  AlertEmail(EmailSubject=EmailSubject,EmailContent=EmailMessage,EmailAlertTable=EmailTable)
}

    
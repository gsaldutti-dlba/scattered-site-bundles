sfimport <- function(csvstr){
  df <- read.csv(csvstr,stringsAsFactors = F,colClasses = c("Property..Parcel.ID" = 'character',
                                                            "Case.Number"='character'))
  if(df[nrow(df),1]=='Detroit Land Bank Authority'){df<-df[1:(nrow(df)-5),]}
  names(df) <- gsub("\\.","",names(df))
  return(df)
}



sfimport_report <- function(report) {
  require(salesforcer)
  sf_auth()
  df <- sf_execute_report(report)
  names(df) <- gsub(":","",names(df))
  names(df) <- gsub(" ", "", names(df))
  return(df)
}

sf_import_query <- function(query, object=NULL, api="Bulk 2.0") {
  require(salesforcer)
  sf_auth()
  df <- sf_query(query, object=object, api_type = api)
  names(df) <- gsub(":","",names(df))
  names(df) <- gsub(" ", "", names(df))
  names(df) <- gsub("__c", "", names(df))
  names(df) <- gsub(".*\\.", "", names(df))
  return(df)
}

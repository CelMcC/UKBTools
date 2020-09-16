

#' Convert to numeric and change negative numbers to NA
#'
#' @param x a vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
numFun<- function(x){
  x<- as.numeric(x)
  x<- ifelse(x< 0,NA,x)
}

#' Apply the 1.5 x IQR Rule to a vector
#'
#' @param x a numeric vector
#'
#' @return a numeric vector with outliers removed
#' @export
#'
#' @examples
applyIQRrule<- function(x){
  #http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_summarizingdata/bs704_summarizingdata7.html
  sk<-   skimr::skim(x)
  Q25<-  sk$numeric.p25
  Q75<-  sk$numeric.p75
  IQR<-  Q75 - Q25
  IQR_1.5 <- IQR * 1.5
  cat("\n IQR: ",IQR, "\tIQR x 1.5:",IQR_1.5," \n")
  lowerCutoff<- Q25 - IQR_1.5
  upperCutoff<- Q75 + IQR_1.5
  cat("\n Cutoffs applied are [ ",lowerCutoff," , ",upperCutoff," ] \n")
  y <-          ifelse(x < lowerCutoff | x > upperCutoff, NA, x)
  return(y)
}


#' Converts all fields except the first into numbers
#'
#' @param df a data.frame
#' @param colIndex columns to convert, defaults to all but first
#'
#' @return a data.frame
#' @export
#'
#' @examples
bio_numeric<- function(df,colIndex= 2:ncol(df)){
  outdf<- df
  if (ncol(df) < 3) {
    outdf<- df
    outdf[,2]<- suppressWarnings(as.numeric(outdf[,2]))
  } else {
    outdf[,colIndex]<- suppressWarnings(apply(df[,colIndex],2,as.numeric))
  }
  return(outdf)
}



#' applies numFun to all but first col in dataframe
#' removes negative special codes, and makes the rest numeric
#'
#' @param df a data.frame
#' @param colIndex columns to convert, defaults to all but first
#'
#' @return a data.frame
#' @export
#'
#' @examples
bio_numFun <- function(df,colIndex= 2:ncol(df)){
  outdf<- df
  if (ncol(df) < 3) {
    outdf[,2]<- suppressWarnings(numFun(outdf[,2]))
  } else {
    outdf[,colIndex]<- apply(df[,colIndex],2,numFun)
  }
  outdf
}


#' Create a log field with sources as input
#'
#' @param temp data.frame with binary indicators
#' @param sources character vector showing source of indicator variables
#'
#' @return
#' @export a string vector, string result for each row
#'
#' @examples
do_LOG<- function(temp, sources){
  logmat<-  newmatrix(nrow(temp),length(sources))
  for (j in 1:length(sources)){
    logmat[,j]<- ifelse(temp[,j+1]==1,sources[j],"")}
  LOG<- apply(logmat,1,collapseString)
  return(LOG)
}


#' Takes a raw UKB data table and applies instance names to columns
#' Only for use where one column per instance, can supply a textTag
#'
#' @param df raw UKB data.frame
#' @param textTag optional tag to show the content of the field
#'
#' @return
#' @export
#'
#' @examples
#' height_data<- temp %>% bio_applyInstanceNames(textTag= "height")
bio_applyInstanceNames<- function(df,textTag="Inst"){
  # returns renamed df
  inst_pat<- "\\.([0123])\\."
  nameString<- names(df)
  instances<- str_match(nameString,inst_pat)[,2]
  if(!are_unique(instances))
    stop("Cannot rename - Multiple columns per instance found")
  outnames<- paste0(textTag,instances)
  outnames[1]<- nameString[1]
  outdf<- df
  names(outdf)<- outnames
  outdf
}

#' Rename all but first column
#' Useful for changing a single name in a tidyverse sentence
#'
#' @param sett data.frame
#' @param namevec new names, must be length= ncol(sett)-1
#'
#' @return renamed data.frame
#' @export
#'
#' @examples
bio_rename<- function(sett, namevec){
  if (length(namevec)!= (ncol(sett)-1)) stop("Vector of names is the wrong length")
  if (ncol(sett)==2) {
    names(sett)[2]<- namevec[1]
  } else {
    names(sett)[2:ncol(sett)]<- namevec
  }
  return(sett)
}


#' Main Current UKB reading function
#'
#' @param udi UKB field number
#' @param baseOnly binary, restrict result to baseline only?
#' @param printHead binary, print the first part of input data?
#'
#' @return
#' @export
#'
#' @examples
bio_read<- function(udi,baseOnly= FALSE, printHead=FALSE) {
  #
  filename<- paste0(dataSourceFolder,"f.",udi,".tab")
  if (!file.exists(filename)) {
    cat("\nError: File does not exist in Data folder. Please check file is downloaded")
  } else {
    theset<- read.table(filename,sep="\t",colClasses = "character",header = T) %>% data.frame() %>%
      select(order(colnames(.))) %>% select(f.eid,everything())
    if ("X" %in% names(theset)) theset<- theset %>% select(-X)
    theset<- theset %>% arrange(f.eid)
    if (baseOnly) {
      theset<- theset %>% select(f.eid, matches("\\.0\\."))
      names(theset)[2:ncol(theset)]<- paste0("base",padded(1:((ncol(theset)-1)),2))
    }
    if (printHead) print(head(theset))
    return(theset)
  }
}

#' Convenience function for aggregating the mean for biochemistry records
#'
#' @param udi UKB field number
#' @param thename character string, name for the new aggregated field
#'
#' @return
#' @export
#'
#' @examples
biochem_read<- function(udi,thename){
  # convenience function for aggregating the mean for biochemistry records
  cat(paste('\nReading udi: ',udi," ",thename))
  temp      <-  bio_read(udi) %>% bio_numeric()
  temp$agg  <- apply(temp[,-1], 1, mean, na.rm=TRUE)
  temp<- temp %>% select(f.eid,agg)
  names(temp)[2]<- thename
  return(temp)
}



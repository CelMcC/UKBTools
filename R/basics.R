
#' Inserts a new folder into the current directory
#' If the folder already exists it does nothing
#'
#' @param folderName character string
#'
#' @return none
#' @export
#'
#' @examples
#' putFolder("Graphics")
putFolder<- function(folderName) {
  if (!dir.exists(folderName))
    dir.create(folderName)
}

#' Convert missing values in a vector into zeros
#'
#' @param thevec
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' IP$diabetes<- fixNAs(IP$diabetes)
fixNAs<- function(thevec){
  thevec<- ifelse(is.na(thevec),0,thevec)
  return(thevec)
}

#' Exclude elements in a vector
#'
#' @param x a vector
#' @param elements a single element or several elements
#'
#' @return x vector not including elements
#' @export
#'
#' @examples
#' c("a", "b", "c", "d") %>% not_including(c("a","b"))
not_including<- function(x,elements){
  y<- x[!x %in% elements]
  return(y)
}


#' Copies object x to the clipboard
#'
#' @param x a vector or data.frame
#'
#' @return none
#' @export
#'
#' @examples
#' to_clipboard(t0)
to_clipboard<- function(x = .Last.value) {
  clipr::write_clip(x)
}


#' Show all names in a data frame with partial match
#' to a TEXT tag
#'
#' @param SETT data.frame
#' @param TEXT string
#'
#' @return character vector of column name matches
#' @export
#'
#' @examples
#' shownames(IP, "patient")
shownames<- function(SETT,TEXT) {
  TEXT<- toupper(TEXT)
  thenames<- toupper(names(SETT))
  return(names(SETT)[thenames %like% TEXT])
}


#' Replace NAs in an entire data frame with zeros
#'
#' @param OB data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' OB<- OB %>% putZeros()
putZeros<- function(OB) {
  OB<- OB %>% mutate_at(vars(-group_cols()),~replace(.,is.na(.),0))
  return(OB)
}


#' Adds zero padding to the left
#'
#' @param thingToPad vector
#' @param width integer - intended final width of elements
#'
#' @return vector
#' @export
#'
#' @examples
#' nice_numbers<- padded(c(1,2,3,20), 2)
padded<- function(thingToPad,width) {
  return(str_pad(thingToPad,side="left",pad="0",width = width))
}


#' Returns the number of unique values in the vector
#'
#' @param thevar a vector
#'
#' @return an integer
#' @export
#'
#' @examples
#' howmany(IP$f.eid)
howmany<- function(thevar) {
  res<- length(unique(thevar))
  return(res)
}


#' Create an empty data frame
#'
#' @param nr integer, number of rows desired
#' @param nc integer, number of columns desired
#' @param thing value to populate the new df, defaults to NA
#'
#' @return a data frame
#' @export
#'
#' @examples
newmatrix<- function(nr,nc,thing=NA) {
  newmat<- matrix(rep(thing,nr*nc),nrow=nr,ncol=nc)
  newmat<- data.frame(newmat)
  return(newmat)
}

#' Change NaN, Inf and -Inf into NA
#'
#' @param x a data.frame
#'
#' @return a data.frame
#' @export
#'
#' @examples
clearNaNs<- function(x){
  x[is.nan(x)]<- NA
  x[is.infinite(x)]<- NA
  return(x)
}

#' Collapse a vector of values into a single string
#' separated by commas
#'
#' @param x a vector
#' @param distinctVals binary, only retain unique values?
#'
#' @return a string
#' @export
#'
#' @examples
collapseString<- function(x, distinctVals=FALSE){
  missingSet<- c(""," ","N/A","NA","NULL")
  y<- as.character(as.vector(x))
  y<- y[!is.na(y)]
  y<- y[!y %in% missingSet]
  if (distinctVals){
    y<- unique(y)
  }
  if (length(y)==0) {
    res<- ""
  } else if (length(y)==1) {
    res<- y
  } else {
    res<- str_c(y,collapse=",")
  }
}

#' Add labels to column names (all but first)
#'
#' @param sett data.frame
#' @param label character string
#' @param before placement of label c("before","after")
#'
#' @return data.frame with new labels
#' @export
#'
#' @examples
label_cols<- function(sett,label,before="before") {
  P<- ncol(sett)
  if (before!= "before") {
    names(sett)[2:P]<- paste0(names(sett)[2:P],label)
  } else {
    names(sett)[2:P]<- paste0(label,names(sett)[2:P])
  }
  return(sett)
}


fromSrc<- function(filename){
  return(paste0(dataSourceFolder,filename))
}

checkkey<- function(df){
  return(howmany(df$f.eid)==nrow(df))
}

percentable<- function(x) {
  res<- round(prop.table(table(x)),3)
  return(res)
}

beforeParenthesis<- function(vec){
  parenthLoc<- str_locate(vec,"\\(")[,1]
  trimmed<-    ifelse(is.na(parenthLoc), vec, str_sub(vec,0,parenthLoc-1))
  trimmed<-    trimws(trimmed)
  trimmed<-    str_replace_all(trimmed,"[\\s\\-/,]","_")
  trimmed<-    str_replace_all(trimmed,"__","_")
  return(trimmed)
}


niceNames<- function(vec){
  trimmed<-    trimws(vec)
  trimmed<-    str_replace_all(trimmed,"[:space:]","_")
  trimmed<-    str_replace_all(trimmed,"[:punct:]","_")
  trimmed<-    str_replace_all(trimmed,"__","_")
  trimmed<-    str_replace_all(trimmed,"__","_")
  return(trimmed)
}


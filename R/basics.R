
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

fixNAs<- function(thevec){
  thevec<- ifelse(is.na(thevec),0,thevec)
  return(thevec)
}

not_including<- function(x,elements){
  y<- x[!x %in% elements]
  return(y)
}

to_clipboard<- function(x = .Last.value) {
  clipr::write_clip(x)
}


shownames<- function(SETT,TEXT) {
  TEXT<- toupper(TEXT)
  thenames<- toupper(names(SETT))
  return(names(SETT)[thenames %like% TEXT])
}


putZeros<- function(OB) {
  OB<- OB %>% mutate_at(vars(-group_cols()),~replace(.,is.na(.),0))
  return(OB)
}

padded<- function(thingToPad,width) {
  return(str_pad(thingToPad,side="left",pad="0",width = width))
}

howmany<- function(thevar) {
  res<- length(unique(thevar))
  return(res)
}

newmatrix<- function(nr,nc,thing=NA) {
  # Create an empty data frame
  newmat<- matrix(rep(thing,nr*nc),nrow=nr,ncol=nc)
  newmat<- data.frame(newmat)
  return(newmat)
}

clearNaNs<- function(x){
  x[is.nan(x)]<- NA
  x[is.infinite(x)]<- NA
  return(x)
}

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

label_cols<- function(sett,label,before="before") {
  P<- ncol(sett)
  if (before!= "before") {
    names(sett)[2:P]<- paste0(names(sett)[2:P],label)
  } else {
    names(sett)[2:P]<- paste0(label,names(sett)[2:P])
  }
  return(sett)
}

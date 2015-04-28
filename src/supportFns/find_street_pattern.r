find_street_pattern <- function(...
  , any=TRUE
  , all=FALSE
  , colsToInclude = c("County", "Municipality", "geo.streetAddress", "location_of_crash", "Cross_Street_Name", "Road_System")
  , searchCol="geo.streetAddress"
  , ignore.case=TRUE
  ) {

## Quick and dirty way to search for string patterns in street address.
##
## This function is not used in any "production" sense, 
##   but rather, interactively when cleaning up
##
##
## ARGS: 
##   all and any are antonyms. 
##   Setting one as TRUE will set the other as false
  

  ## if either is missing, set the other to the opposite
  if (missing(any) && !missing(all))
    any <- !(all)
  else if (missing(all) && !missing(any))
    all <- !(any)

  if (isTRUE(all) && isTRUE(any))
    stop ("Pick one of 'all' or 'any' -- both cannot be TRUE")

  patterns <- unlist(list(...))

  if (isTRUE(all)) {
    ## Quick and dirty
    DT.out <- DT.Accidents
    for (p in patterns)
      DT.out <- DT.out[grepl(p, get(searchCol), ignore.case=ignore.case), unique(c(searchCol, colsToInclude)), with=FALSE]
    DT.out[, .N, keyby=colsToInclude]
  } else {
    DT.Accidents[grepl(regOr(patterns), get(searchCol), ignore.case=ignore.case), .N, keyby=colsToInclude]
  }
}

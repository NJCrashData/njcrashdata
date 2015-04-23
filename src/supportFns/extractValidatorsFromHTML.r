

## --------------------------------------------------------------
##   
##   Note to self:  Some of this could be handled with the generic 
##         extract..HTML() functions
##   
##   eg, extractValidatorValuesFromHTML()
##       extractInputAttrsFromHTML(html=html_prelogin, output="DT", showWarnings=FALSE)[grepl("^__", id)]
##       extractInputAttrsFromHTML(html=html_prelogin, output="DT", showWarnings=FALSE)[!is.na(value)]
##   Keeping these for now, for reference
##   
##       ## COMPARE: 
##       extractAttrValueFromHTML(html=html_prelogin)
##       extractInputAttrsFromHTML(html=html_prelogin, showWarnings=FALSE)
##       extractValidatorIDsFromHTML(html=html_prelogin)
##       extractValidatorValuesFromHTML(html=html_prelogin)

extractValidatorIDsFromHTML <- function(html, pat.id="__\\w+", fmt.id='.*id\\s*=\\s*"(%s)"', showWarnings=TRUE, ignore.case=TRUE) {
  pattern <- sprintf(fmt.id, pat.id)
  IDs <- extractAllMatches(pattern=pattern, string=html, remove_wildcard_from_pattern_tails=TRUE, showWarnings=FALSE, ignore.case=ignore.case)
  if (!length(IDs) && showWarnings)
    warning ("No IDs found matching pattern   '", pattern, "'  -- returning character()", call.=FALSE)
  gsub(pattern, "\\1", IDs, ignore.case=ignore.case)
}

#  extractValidatorIDsFromHTML_old <- function(html, pattern='.*id\\s*=\\s*"(__\\w+)"', showWarnings=TRUE, ignore.case=TRUE) {
#    IDs <- extractAllMatches(pattern=pattern, string=html, remove_wildcard_from_pattern_tails=TRUE, showWarnings=FALSE)
#    if (!length(IDs) && showWarnings)
#      warning ("No IDs found matching pattern   '", pattern, "'  -- returning character()", call.=FALSE)
#    gsub(pattern, "\\1", IDs, ignore.case=ignore.case)
#  }

extractValidatorValuesFromHTML <- function(html, IDs
  , pat.id="__\\w+", pat.value='[0-9a-zA-Z+/=]*'
  , fmt.id='.*id\\s*=\\s*"(%s)"', fmt.value='value="(%s).*'
  , group_for_sub.value=2, ignore.case=TRUE) {

  pattern.id    <- sprintf(fmt.id, pat.id)
  pattern.value <- sprintf(fmt.value, pat.value)

  if (missing(IDs))
    IDs <- extractValidatorIDsFromHTML(html, fmt.id=fmt.id, pat.id=pat.id)
  else 
    force(IDs)

  if (!length(IDs) && showWarnings) {
    warning ("No IDs submitted  -- returning character()", call.=FALSE)
    return(character())
  }

  pattern_full <- paste0(sprintf(fmt.id, IDs), "\\s*", pattern.value)
  names(pattern_full) <- IDs
  sapply(pattern_full, grepl, html, ignore.case=TRUE)
  ret <- sapply(pattern_full, sub, paste0("\\", group_for_sub.value), html, ignore.case=ignore.case)

  if (!length(ret) && showWarnings)  {
    warning ("No values found  -- returning character()", call.=FALSE)
    return(character())
  }

  return(ret)
}

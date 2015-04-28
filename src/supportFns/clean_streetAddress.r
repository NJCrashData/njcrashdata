

clean_street_groups <- function(string, ignore.case=FALSE, word_bound="", word_bound_left=word_bound, word_bound_right=word_bound, patterns=get_replacement_patterns(), replace=names(patterns), verbose=TRUE) {
## Set word_bound="" if you do NOT want to use a word_bound
## Set word_bound="\\b" for basic word bound
## replace can be the names of patterns, if patterns is a list of length one with names


  L.p   <- length(patterns)
  L.r   <- length(replace)
  L.wbl <- length(word_bound_left)
  L.wbr <- length(word_bound_right)

  if (!L.p) {
    warning ("pattersn has no length -- returning string unchanged")
    return(string)
  }

  ## Quick input check to confirm that all elements have lengths matching that of patterns
  confirmLength <- function(nm, L) {
    if (L %in% c(1, L.p))
      return (TRUE)
    stop(sprintf("'%s' has length %i. It should have same length as 'patterns' (%i) or length 1", nm, L, L.p))
  }
  confirmLength("replace",          L.r)
  confirmLength("word_bound_left",  L.wbl)
  confirmLength("word_bound_right", L.wbr)


  ## rpeate the word bounds if they have length 1
  if (L.wbl == 1)
    word_bound_left <- rep.int(word_bound_left, times=length(patterns))
  if (L.wbr == 1)
    word_bound_right <- rep.int(word_bound_right, times=length(patterns))

  for (i in seq(patterns)) {
      paste0(word_bound_left[[i]], patterns[[i]], word_bound_right[[i]]) %>% 
      regOr(vec = .) -> pat_i
      grep(pattern=pat_i, x=string) -> INDS
      if (length(INDS)) {
        verboseMsg(verbose, sprintf("%02i : Cleaning % 6i matches of '%s'", i, length(INDS), replace[[i]]), endl=1, minw=99)
        string[INDS] <- gsub(pattern = pat_i, replace=replace[[i]], x=string[INDS], ignore.case=ignore.case)
      } else {
        verboseMsg(verbose, sprintf("%02i : No matches found for replacement pattern '%s'", i, replace[[i]]), endl=1, minw=99)
      }
  }

  return(string)
}


clean_streetAddress <- function(streetAddress, drop_parkingLots=TRUE, upper=TRUE, verbose=TRUE) {
## These are specific issues found

## Address standards: http://pe.usps.gov/cpim/ftp/pubs/Pub28/pub28.pdf

  ## Clean quotes
  streetAddress <- removeDuplicateChars(streetAddress)


  if (isTRUE(upper)) {
    ## Most input should already be uppercase. 
    ## In order to not waste time running all elements through toupper(), search for lowercase
    has_lowercase <- grepl("[a-z]", streetAddress, ignore.case=FALSE)
    if (length(has_lowercase))
      streetAddress[(has_lowercase)] <- toupper(streetAddress[(has_lowercase)])
  } 

  orig <- copy(streetAddress)

  ## Clean other single quotes not caught above
  streetAddress <- filter_then_gsub(pattern_to_replace="'", pattern_to_filter_by=regOr(get_patterns_with_dangling_apostrophe(), escape=TRUE), replace="", string=streetAddress)



  streetAddress <- clean_street_groups(string=streetAddress, word_bound="\\b", verbose=verbose)


  ## Remove quotes around single letter or letter-numbers
  ## ie:  "I"   "J 5"  "E4"   etc
  ##      with or without quotes around it
  verboseMsg(verbose, "Removing quotes around single letter or numbers")
  streetAddress <- gsub("(\\\"|')(([A-Za-z])(\\s*[0-9A-Za-z][0-9]?)?)(\\\"|')", "\\2", streetAddress)

  ## clean this one road
  streetAddress <- gsub("\"ROAD J\"", "ROAD J", streetAddress)


  ## Remove @ signs
  verboseMsg(verbose, "Removing @ of any kind")
  streetAddress <- gsub("(\\s*\\@\\s*)+", " ", streetAddress)

  ## Remove * signs
  verboseMsg(verbose, "Removing asterisks of any kind")
  streetAddress <- gsub("(\\s*\\*\\s*)+", " ", streetAddress)

  ## Remove pound signs
  verboseMsg(verbose, "Removing pound signs of any kind")
  streetAddress <- gsub("(\\s*#\\s*)+", " ", streetAddress)

  ## Remove anything following a semicolon
  verboseMsg(verbose, "Removing anything after a semicolon")
  streetAddress <- gsub(";.*$", "", streetAddress)


  verboseMsg(verbose, "Removing apostrophes preceeding an 'S'")
  streetAddress <- gsub("'(S|s)(\\s|\\)|\\b)", "\\1\\2", streetAddress)
  # "\\1\\2" = c("'(S|s)(\\s|\\)|\\b)")

  verboseMsg(verbose, "Removing apostrophes following an 'O', 'L', 'D', etc")
  O_or_D_words <- c("ALLESANDRO", "AMICO", "ANGELO", "ANTON", "ARCY", "AURIA", "BRIAN", "BRIEN", "CON", "DONELL", "DONNELL", "DONOHUE", "HAGEN","HANLON", "KEEFE", "LEARY", "NEIL", "SHANTER", "SHAUGHNESSY", "SHIBE", "TOOLE", "WOODS")
  pat_O_or_D <- sprintf("(^| )(O|D)'%s", regOr(O_or_D_words))
  streetAddress <- gsub(pat_O_or_D, "\\1\\2\\3", streetAddress)

  INDS.quotes <- grep("\"", streetAddress)
  if (length(INDS.quotes)) {
    verboseMsg(verbose, "Replacing remaining quotes with commas (except for starting quote marks) in ", length(INDS.quotes), " rows")
    streetAddress[INDS.quotes] <- gsub("^\"", "", streetAddress[INDS.quotes])
    streetAddress[INDS.quotes] <- gsub("\"", ",", streetAddress[INDS.quotes])
  }

  INDS.s_quotes <- grep("\'", streetAddress)
  if (length(INDS.s_quotes)) {
    verboseMsg(verbose, "Removing remaining single quotes in ", length(INDS.s_quotes), " rows")
    streetAddress[INDS.s_quotes] <- gsub("^\"", "", streetAddress[INDS.s_quotes])
    streetAddress[INDS.s_quotes] <- gsub("\"", ",", streetAddress[INDS.s_quotes])
  }




## WORK STILL NEEDED
# ~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## Example of cleaning needed
# "721 RT 202-206"
# "A&S DRIVE AND WINTERS AVE"
# "A+S DR AND WINTERS AVENUE"
# OCEAN COUNTY 623 ~~> County Road 623
# 
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~


  if (isTRUE(drop_parkingLots)) {
    verboseMsg(verbose, "Removing parking lot patterns")
    streetAddress <- gsub(get_parking_lot_pattern(), "", streetAddress)
  }
  

  ## Remove double space
  verboseMsg(verbose, "Cleaning up whitespace, including trim and removing any double-spaces")
  streetAddress <- cleanWS(streetAddress)


# ======================================
# ------------------------------
#x  ## Clear first batch of basic Apostraphes
#x  S_apostraphes <- c("JOHNNY", "ANGELO", "ANTON", "ARCY", "AURIA", "BRIAN", "BRIEN", "CON", "DONELL", "DONNELL", "DONOHUE", "HAGEN","HANLON", "KEEFE", "LEARY", "NEIL", "SHANTER", "SHAUGHNESSY", "SHIBE", "WOODS")
#x  pat_O_or_D <- sprintf("(^| )(O|D)'%s", regOr(O_or_D_words))
#x  streetAddress <- gsub(pat_O_or_D, "\\1\\2\\3", streetAddress)
#x
#x  streetAddress <- gsub("STRYKER'S", "STRYKERS", streetAddress)
#x  streetAddress <- gsub("O'DONNELL", "DONNELL", streetAddress)
  
# browser(text="streetAddress cleanup")
## Find any words with non-alphanumeric characters

## THESE ARE THE SYMBOLS THAT ARE LEFT
symbolsRemaining <- c("-", "?", "/", "&")
regOr(symbolsRemaining, escape=TRUE) %>% 
  {streetAddress[!grepl(., streetAddress)]} %>% 
  grep("[^[:alnum:]|\\.|\\(|\\)|' ]", ., value=TRUE)

DT.Accidents[grepl("\\*(\\s*)$", streetAddress), .N, keyby=list(Municipality, geo.streetAddress, location_of_crash, Cross_Street_Name)][!grepl("\\*{2,}", geo.streetAddress)]

# ------------------------------
# ======================================



  ## SHOW
  if (FALSE)
    streetAddress[grepl("'", streetAddress) & !grepl("'S", streetAddress)]

  return(streetAddress)
}


 

# pat_O <- sprintf("O'%s", regOr(O_words))
# DT.Accidents[, geo.streetAddress := gsub(pat_O, "O\\1", geo.streetAddress)]

#    1:                     210 ROUTE 46 EAST-DUNKIN'
#    2:                     NJ 17 AND RT 17 RAMP 'J '
#    3:           O'TOOLE STREET AND HILLCREST AVENUE
#    4:                              1500 RTE 47 'PL'
#    5:                            227 N MAIN ST 'PL'
#    6:                   3111 DELSEA DR (CAP'N CATS)
#    7:       PINEY HOLLOW ROAD AND D'ALLESANDRO ROAD
#    8:                                    B'LINE AVE
#    9:                                    B'LINE AVE
#   10:                276 RT 202/31 (MARSHALLS' LOT)
#   11:                    325 RT 202 (MICHAELS' LOT)
#   12:      SPIRIT OF '76 BLVD AND MARKET PLACE BLVD
#   13:        MIDDLESEX COUNTY 622 AND L'AMBIANCE CT
#   14:     MIDDLESEX COUNTY 622 AND L'AMBIANCE COURT
#   15: SCHALKS' CROSSING ROAD AND SCUDDERS MILL ROAD
#   16:        MIDDLESEX COUNTY 622 AND L'AMBIANCE CT
#   17:        MIDDLESEX COUNTY 622 AND L'AMBIANCE CT
#   18:                            NJ 18 AND AVENUE'E
#   19:     MIDDLESEX COUNTY 622 AND L'AMBIANCE COURT
#   20:                     660 PLAINSBORO RD 17' AS?
#   21:                622 ROUTE 10 - LIL' DANS PIZZA
#   22:                    130 NJ 10 (MC'DONALDS LOT)
#   23:                             30 KINGS' LAND RD
#   24:                ROUTE 23 SIT N' CHAT DINER P/L
#   25:                    1207 RT 22 SHOP RITE P'LOT
#   26:                   1207 NEW BRUNSWICK AVEP'LOT
#   27:                  101 WYNDHAM BLVD SOUTH P'LOT
#   28:                  1207 RT 22 (SHOP RITE P'LOT)
#   29:                      101 WYNDHAM BLVD (P'LOT)
#   30:                  1204 NEW BRUNSWICK AVE P'LOT
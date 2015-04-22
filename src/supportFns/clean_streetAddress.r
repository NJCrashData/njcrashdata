clean_streetAddress <- function(streetAddress, drop_parkingLots=TRUE) {
## These are specific issues found

  ## Clean quotes
  streetAddress <- removeDuplicateChars(streetAddress)

  ## Remove quotes around single letter
  streetAddress <- gsub("(\\\"|')([A-Za-z])(\\\"|')", "\\2", streetAddress)

  ## Remove pound signs
  streetAddress <- gsub("(\\s*#\\s*)+", " ", streetAddress)

  ## Remove anything following a semicolon
  streetAddress <- gsub(";.*$", "", streetAddress)


  streetAddress <- gsub("(SH|HWY)\\s*#", "STATE HIGHWAY ", streetAddress)
  streetAddress <- gsub("(RT|ROUTE|RTE)\\s*#", "RT ", streetAddress)
  streetAddress <- gsub("LAKEWOOD F'DALE", "LAKEWOOD FARMINGDALE", streetAddress)
  streetAddress <- gsub("DUNKIN'\\s*DONUTS", "DUNKING DONUTS", streetAddress)
  streetAddress <- gsub("PARK'G", "PARKING", streetAddress)
  streetAddress <- gsub("WOODS'", "WOODS", streetAddress)
  streetAddress <- gsub("O'HAGEN", "OHAGEN", streetAddress)
  streetAddress <- gsub("O'CON", "OCON", streetAddress)
  streetAddress <- gsub("B'WAY", "BROADWAY", streetAddress)
  streetAddress <- gsub("*18' ", "18 ", streetAddress)
  streetAddress <- gsub("'(S|s)(\\s|\\)|\\b)", "\\1\\2", streetAddress)

  if (isTRUE(drop_parkingLots)) {
    pat_parkingLots <- "\\s*(\"|\')?PARKING(\\s|-|_|\\b)*(LOT|DECK|GARAGE)(\"|\')?"
    streetAddress <- gsub(pat_parkingLots, "", streetAddress)
  }

  ## Remove double space
  streetAddress <- cleanWS(streetAddress)

  ## SHOW
  if (FALSE)
    streetAddress[grepl("'", streetAddress) & !grepl("'S", streetAddress)]

  return(streetAddress)
}


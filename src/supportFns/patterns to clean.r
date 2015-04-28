get_parking_lot_pattern <- function() {
## living regex..  I keep adding to this as finding more offenders

    Parking_Lot_Strings <- {
        c("(\"|\')?PARKING(\\s|-|_|\\b)*(LOT|DECK|GARAGE)(\"|\')?"
          , '"PL"'
          , "'PL'"
          , "'P/L'"
          , "P'LOT"
          , "P-LOT"
          , "PARK-LOT"
          , "PARK LOT"
          , "PARK'G"
          , '"PARKING'
          , '"PRIVATE PROP(ERTY)?"'
          , "PRIVATE PARKING AREA"
          , '\\*PRIVATE PROPERTY\\*'
          , '(/|\\\\)\\s*PARKING LOT'
          , "SPORTS COMPLEX LOT"
          , "METLIFE"
          , "DRIVEWAY"
          )
    }

    pat_parkingLots <- regOr(sprintf("(%s)", Parking_Lot_Strings))

    return(pat_parkingLots)
}


get_common_store_names <- function () {
c(
  "BURGER KING"
,  "BEST BUY"
,  "WELLS FARGO"
,  "SHOPRITE"
,  "DICKS"
,  "DUNKING DONUTS"
,  "BRUNS SQ MALL"
,  "JC PENNY"
,  "MACYS"
,  "HESS GAS"
,  "VILLAGE GREEN"
,  "IHOP"
,  "WAL-MART"
)
}

get_road_structure_pattern <- function () {
  c( "GANTRY"
    , "STRUCTURE"
    , "JUG\\s*HANDLE"
    , "RAMP"
    , "SHOPPING PLAZA"
    , "ENTRANCE"
  ) %>% 
  regOr(vec=., escape=TRUE)
}


get_replacement_patterns <- function() {

  ## There is a road in Toms River named "Intermediate N Way", appears slightly varied
  ##   and causes issues on geocoding
  ## This should really be just for Municipality == "TOMS RIVER TWP"
  ## However, grepping the entire NJ data, only found 
  ## 
  ## DT.Accidents[grepl("INTERMEDIATE", geo.streetAddress) & Municipality != "TOMS RIVER TWP", .N, keyby=list(Municipality,geo.streetAddress)]
  ##           Municipality                 geo.streetAddress   N
  ##      1:  FREEHOLD BORO   INTERMEDIATE SCHOOL PARKING LOT   1
  ##      2: GLASSBORO BORO               INTERMEDIATE SCHOOL   1
  ##      3:   STAFFORD TWP        STAFFORD INTERMEDIATE-1000   1


    list(
          'US HIGHWAY' = sprintf("U\\s*S\\s*%s", regOr(c("HW", "HY", "HWY", "HIGHWAY")))
         , 'ROUTE' = c("RT\\.?", "RTE\\.?") # "ROUTE"
         , 'INTERSTATE '       = c("I\\s*\\-") #, "INTERSTATE"
         , "STATE HIGHWAY" = c("(S\\s*H)", "SHWY")
         , "HIGHWAY " = c("HWY", "HW", "HY") # HIGHWAY
        # , "RT " = c("(RT|ROUTE|RTE)\\s*#")
      ,                         'A&P' = c("(/)?\\(?A\\s*(AND|&)\\s*P( SUPERMARKET|PARKING)?( LOT)?\\)?")
      # ,                          'TA' = c("T/A")
      , 'METLIFE STADIUM PARKING LOT' = c("(METLIFE|MEADOWLANDS|IZOD( CENTER)?)( STADIUM)?( (PARKING )?LOT)?.*", "MEADOWLANDS SPORTS COMPLEX METLIFE STADIUM HOT F", "LOT L 2 METLIFE STADIUM")
      ,                     '202-206' = c("202206", "202/206", "202 AND 206( HWY$)")
        , 'TA TRUCK STOP PARKING LOT' = c("T/A PARKING LOT.*", "T/A TRUCK STOP.*")
        , 'TRAVEL CENTER PARKING LOT' = c("TRAVEL CENTER T/A LOT", "TRAVELCENTERT/ALOT")
         , 'INTERMEDIATE N WAY'       = c("INTERMEDIATE (WEST|NORTH) WAY", "INTERMEDIATE WAY( NORTH| W)?")
         # , 'US HIGHWAY' = c("USHY", "U\\s*S\\s*HWY", "U\\s*S(\\s*(HIGHWAY|HW?Y))?")

         ## ONE-OF PATTERNS
        # , "\\1\\2" = c("'(S|s)(\\s|\\)|\\b)")
        , "OSHAUGHNESSY" = c("O' SHAUGHNESSY")
        , "LAMBIANCE" = c("L'AMBIANCE")
        , "BERGENLINE AVE" = c("B'LINE AVE")
        , "KINGS LAND" = c("KINGS' LAND")
        , "WARREN COUNTY COMMUNITY COLLEGE" = c("WARREN (CTY|COUNTY)?\\s*(COMM?)?\\s*COLLEGE(\\s*LOT)?")
        , "LAKEWOOD FARMINGDALE" = c("LAKEWOOD F'DALE")
        , "DUNKING DONUTS" = c("DUNKIN'(\\s*DONUTS)?")
        , "PARKING" = c("PARK'G")
        , "WOODS" = c("WOODS'")
        , "BROADWAY" = c("B'WAY")
        , "WALMART" = c("WAL(\\-|\\s)*MART")
        , "&" = "\\s*\\+\\s*"
        , "18 " = c("*18' ")
        , "1 BORGATA WAY" = "1-BORGATA WAY"
      )
}


get_patterns_with_dangling_apostrophe <- function() {
  c(
      "RAMP 'J '"
    , "CAP'N CATS"
    , "MACY'S"
    , "MARSHALLS' LOT"
    , "MICHAELS' LOT"
    , "MC'DONALDS"
    , "AVENUE'E"
    , "660 PLAINSBORO RD 17'"
    , "WOODS' WAY"
    , "LIL' DANS PIZZA"
    , "LIL' DAN'S PIZZA"
    , "SIT N' CHAT"
   )
}

filter_then_gsub <- function(
    pattern_to_replace
  , replacement
  , string
  , ignore.case=FALSE
  , pattern_to_filter_by=pattern_to_replace
  , verbose=TRUE
) {
## instead of gsub'ing over entire string, replaces only in those elements where the filter matches

  if (length(replacement) > 1)
    stop ("replacement must have length one")
  if (length(pattern_to_filter_by) > 1)
    stop ("pattern_to_filter_by must have length one")
  if (length(pattern_to_replace) > 1)
    stop ("pattern_to_replace must have length one")

  INDS <- grep(pattern_to_filter_by, string, ignore.case=ignore.case)
  if (length(INDS)) {
    verboseMsg(verbose, sprintf("Found % 3i matches for the filter pattern '%s'", length(INDS), pattern_to_filter_by))
    string[INDS] <- gsub(pattern_to_replace, replacement, string[INDS], ignore.case=ignore.case)
  }

  return(string)
}




if (FALSE) {


    #   ## SPECIFIC ERRORS
    #   62 ND ST/ADAMS ST  ~~>  62ND ST AND ADAMS ST
    "A&P PARKING LOT( 459 SH 31)"
    "A&P PLAZA NA"
    "525 US 46 EAST / A & P PLAZA"



    ## TEST STRING
    .TestString <- {
    c(
    "T/A TRUCK STOP PLOT", "T/A PARKING LOT"
    , "T/A PARKING LOT", "T/A PARKING LOT"
    , "T/A PARKING LOT", "T/A TRUCK STOP FRONT P/L"
    , "LOT L 2 METLIFE STADIUM"
    , "MEADOWLANDS SPORTS COMPLEX METLIFE STADIUM HOT F"
    , "METLIFE PARKING LOT \"J 3\" NA"
    , "METLIFE PARKING LOT J-16"
    , "METLIFE PARKING LOT J-23 NA"
    , "METLIFE STADIUM - LOT H : EAST R"
    , "METLIFE STADIUM - LOT L-4 UK"
    , "METLIFE STADIUM LOADING DOCK NA"
    , "METLIFE STADIUM LOADING DOCK"
    , "METLIFE STADIUM LOT \"B 2\""
    , "METLIFE STADIUM LOT \"E\""
    , "METLIFE STADIUM LOT \"J 19\""
    , "METLIFE STADIUM LOT 616"
    , "METLIFE STADIUM LOT E 4"
    , "METLIFE STADIUM LOT J 2 NA"
    , "METLIFE STADIUM LOT K 1 NA"
    , "METLIFE STADIUM LOT L 5"
    , "METLIFE STADIUM LOT L 9"
    , "METLIFE STADIUM LOT"
    , "METLIFE STADIUM NA"
    , "METLIFE STADIUM PARKING LOT 8"
    , "METLIFE STADIUM PARKING LOT E 1"
    , "METLIFE STADIUM PARKING LOT E 2"
    , "METLIFE STADIUM PARKING LOT F-3"
    , "METLIFE STADIUM PARKING LOT G 15"
    , "METLIFE STADIUM PARKING LOT G 16"
    , "METLIFE STADIUM PARKING LOT G-13"
    , "METLIFE STADIUM PARKING LOT G-14"
    , "METLIFE STADIUM PARKING LOT P 7"
    , "10 PARK AVENUE-METLIFE"
    )}
    ## TEST
    .CleanedString <- .TestString
    for (nm in names(get_replacement_patterns())) {
      .CleanedString <- clean_street_groups(pat=get_replacement_patterns()[[nm]], replace=nm, string=.CleanedString)
      print(cbind(ORIGINAL=.TestString, CLEANED=.CleanedString))
    }



}

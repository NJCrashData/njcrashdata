DT.Drivers      <-  copy(ll_DT.Drivers      [[Cnty]])
DT.Occupants    <-  copy(ll_DT.Occupants    [[Cnty]])
DT.Pedestrians  <-  copy(ll_DT.Pedestrians  [[Cnty]])
DT.Vehicles     <-  copy(ll_DT.Vehicles     [[Cnty]])
DT.Accidents    <-  copy(ll_DT.Accidents    [[Cnty]])


## TEMP -- using Monmouth & Essex Country
DT.Accidents    <-  rbindlist(copy(ll_DT.Accidents[c("Monmouth", "Essex")]))
setkeyIfNot(DT.Accidents, "join_id", superset.ok=TRUE)

## --------------- PARSING JOIN CO
verboseMsg(verbose, "Parsing Join Columns For all DTs")
addParsedjoin_id_(DT.Drivers)
addParsedjoin_id_(DT.Occupants)
addParsedjoin_id_(DT.Pedestrians)
addParsedjoin_id_(DT.Vehicles)
addParsedjoin_id_(DT.Accidents)


## -------------------- DT.Drivers ----------------------------------

## Add a field for Driver details missing
DT.Drivers

## -------------------- DT.Accidents ----------------------------------
verboseMsg(verbose, "   -------    BEGINNING DT.Accidents   -------  ")
# verboseMsg(verbose, "Parsing Join Column")
# DT.Accidents[, c("Year", "County_Code", "Municipality_Code", "Department_Case_Number") := 
#                   list( as.numeric(trim(substr(join_id, 1, 4)))
#                       , as.numeric(trim(substr(join_id, 5, 6)))
#                       , as.numeric(trim(substr(join_id, 7, 8)))
#                       ,           (trim(substr(join_id, 9, 31)))
#                       )
#             ]

## Set blanks to NA
for (col in names(DT.Accidents)) {
  if (is.character(DT.Accidents[[col]]))
    DT.Accidents[, (col) := trim(get(col))]  
  DT.Accidents[trim(get(col)) == "", (col) := NA]
}

BackUpOrRestore("DT.Accidents", force=TRUE, verbose=FALSE, verboseRestore=TRUE)
# BackUpOrRestore("DT.Accidents")

## Bank the original NAs, as we want to be sure not to intoduce any new ones
tmp_NAs <- is.na(DT.Accidents)

## Clean up dates
DT.Accidents[, date_of_crash := as.Date(date_of_crash, format="%m/%d/%Y")]
DT.Accidents[, day_of_crash  := weekdays(date_of_crash)]
## Confrim no errors:
stopifnot(DT.Accidents[, !is.na(date_of_crash)])
stopifnot(DT.Accidents[, !is.na(day_of_crash) & day_of_crash %in% weekdays(today() + 1:7)])

##  ## NOTES
##  Posted_Speed -- There appear to be some typos, though a very small ammount. 
##  ie, a few random 1, 2, 3, 4 etc -- which are likely the tens digit, with the ones digit missing (is it 0 or 5)
##      or maybe they mean just 5 mph and are a typo.  Also, what of the 5mph, should any of them be 50mph ? 
##      For now, just leave them alone.  It is not to significant. 
##      At some point we could use geolocation to clean
## SEE: 
##      DT.Accidents[, .N, by = Posted_Speed][order(as.character(Posted_Speed))]


## Identify columns that are factors not numbers
cols.factorNotNumber <- c("Police_Dept_Code", "Police_Station", "Crash_Type_Code", "Route", "Route_Suffix", "route_id_SRI", "Road_System", "Road_Character", "Road_Surface_Type", "Surface_Condition", "Light_Condition", "Environmental_Condition", "Road_Divided_By", "Temporary_Traffic_Control_Zone", "Unit_Of_Measurement",  "Is_Ramp", "Ramp_To_or_From_Route_Name", "Ramp_To_or_From_Route_Direction", "Posted_Speed_Cross_Street", "Posted_Speed", "Latitude", "Longitude", "cell_phone_in_use", "Other_Property_Damage", "badge_numb", "County_Code", "Municipality_Code", "Department_Case_Number", "County", "Municipality", "Police_Department")

## Columns that are deffinitely numbers
cols.numbers <- c("MilePost", "Distance_To_Cross_Street")

## Columns that need basic string cleaning
cols.charsToClean <- c("location_of_crash", "Cross_Street_Name", "location_direction", "Directn_From_Cross_Street")

## Clean up badge number
DT.Accidents[, badge_numb := trim(gsub("\\#", "", badge_numb))]

## clean up numerics
verboseMsg(verbose, "Converting numeric columns")
for (col in cols.numbers)
  DT.Accidents[, (col) := as.numeric(trim(as.character(get(col))))]

## clean up numerics
verboseMsg(verbose, "Converting factor columns")
for (col in cols.factorNotNumber)
  DT.Accidents[, (col) := factor(basicStringCleaning(get(col)))]

verboseMsg(verbose, "Cleaning character columns")
for (col in cols.charsToClean)
  DT.Accidents[, (col) := basicStringCleaning(get(col))]


## Confirm that all time_of_crash is exactly 4 digits long
if (!all(4 == nchar(DT.Accidents[!is.na(time_of_crash), time_of_crash])))
  warning ("Not all of (non-NA) time_of_crash are exactly 4 characters long")

## Create a field for datetime_of_crash -- This will be NA whenever time_of_crash is NA.
## We dont want to use a default time, since it will default to Midnight, and that is not accurrate
verboseMsg(verbose, "Creating column datetime_of_crash")
DT.Accidents[,  datetime_of_crash := as.POSIXct(sprintf("%s %s", date_of_crash, time_of_crash), format="%Y-%m-%d %k%M", tz="America/New_York")]
## SHOW RESULTS: 
DT.Accidents[is.na(time_of_crash), list(date_of_crash, time_of_crash, datetime_of_crash)]

## Create streetAddress using location and cross street
## TODO: Incorporate ramp information, distance, etc
pat_to_not_include <- "(Parking|lot|deck|garage|Priv*Property)"
    ## Check if has street number
DT.Accidents[, geo.has_street_number := grepl("^\\s*\\d", location_of_crash)]
    ## Check if Cross_Street_Name is valid and that street has no number
DT.Accidents[, geo.include_cross_st := !is.na(Cross_Street_Name) & !grepl(pat_to_not_include, Cross_Street_Name, ignore.case=TRUE) & !geo.has_street_number]
DT.Accidents[, geo.streetAddress := paste0(location_of_crash, ifelse(geo.include_cross_st, no="", yes=paste(" and", gsub(".*/\\s*", "", Cross_Street_Name))))]


## Some basic cleaning
DT.Accidents[, geo.streetAddress := clean_streetAddress(geo.streetAddress, drop_parkingLots=TRUE)]
## SHOW
DT.Accidents[grepl("'", geo.streetAddress) & !grepl("'S", geo.streetAddress)]

## Confirm that no NAs where introduced
colsAdded <- c("datetime_of_crash", "geo.include_cross_st", "geo.has_street_number", "geo.streetAddress")
tmp_NAs.new <- is.na(DT.Accidents[, names(DT.Accidents) %ni% colsAdded, with=FALSE])
if (any(tmp_NAs != tmp_NAs.new)) {
    cols_with_NAs_introduced <- colnames(tmp_NAs)[sapply(seq.int(ncol(tmp_NAs)), function(i) any(tmp_NAs[, i] != tmp_NAs.new[, i]))]
    warning(warningCols("The following columns had NAs introduced ", cols_with_NAs_introduced))
}


###  -------------------------------------------------- ###
###  ---------------    GEOCODE   --------------------- ###
###  -------------------------------------------------- ###

if (FALSE) {
  join_ids_with_poundsign <- DT.Accidents[grepl("\\#", geo.streetAddress)][, join_id[[1]], by=list(Municipality, geo.streetAddress)]$V1
}

## --------------------------- ##
##        Using TAMU           ##
## --------------------------- ##

## Add in previously processed Lat/Lng Data
{
  try(cat("Available Credits : ", get_available_credits_tamu(), "\n"))
  if ("latitude" %in% names(DT.Accidents))
    message("Current number of non-NAs in latitude is ", formnumb(DT.Accidents[!is.na(latitude), .N], round=FALSE))
  colsToBring <- c("latitude", "longitude", "match_type", "matched_location_type")
  invisible(suppressWarnings(DT.Accidents[, (colsBringing) := NULL]))
  read_all_tamu_files_and_add_to_DT_(DT=DT.Accidents, colsToBring=colsToBring, verbose=FALSE)
  message("Updated number of non-NAs in latitude is ", formnumb(DT.Accidents[!is.na(latitude), .N], round=FALSE))
}

if (FALSE) {
  # ## Query the TAMU API
  DT.ret <- {
    # DT.Accidents[join_id %in% join_ids_with_poundsign , 
    # DT.Accidents[Municipality_Code %in% c(34:35) & Year == 2013 , 
    # 
    DT.Accidents[is.na(latitude) & County == "MONMOUTH" & Year == 2013,
    # DT.Accidents[, 
      geocode_tamu(streetAddress = geo.streetAddress
                  , city = Municipality
                  , state = "NJ"
                  , zip = NULL
                  , internal_id=join_id
                  , apikey = getOption("tamu_geo.apikey")
                  , format = "csv"
                  , check_states = FALSE
                  , folder = data.p("geocode_tamu_results", County[[1]])
                  , colsToReturn = c("internal_id", colsToBring)
                  , iter_size = 35
                )]
  }

  DT.Accidents[join_id %in% DT.ret[match_type == "bad_request", internal_id]]
  if (is.data.table(DT.ret) && identical(DT.ret[["internal_id"]], DT.Accidents[["join_id"]])) {
      if (any(colsToBring %in% names(DT.Accidents)))
        warning ("Some columns in colsToBring is/are already in DT.Accidents --- DT.ret NOT brought over")
      else 
        DT.Accidents[, colsToBring := DT.ret[, !"internal_id", with=FALSE] ]
  } else {
    ## May not be data.table
      warning ("DT.ret did not have the correct internal_id values. Try merging")
  }
}

## --------------------------- ##
##        Using Google         ##
## --------------------------- ##
if (FALSE) {
  DT.ret_ggmap <- {
    DT.Acc_code[unique(c(1, which(match_type != "Exact" & Municipality_Code == 35))) ,
    # DT.Accidents[, 
      geocode_google(streetAddress = geo.streetAddress
                  , city = Municipality
                  , state = "NJ"
                  , zip = NULL
                  , internal_id=join_id
                  , folder = data.p("geocode_ggmap_results", County[[1]])
                  , colsToReturn = c("internal_id", colsToBring)
                  , iter_size = 201
                )]
  }
  message("Compare the results from DT.ret_ggmap to that from TAMU -- then add the geo data back into DT.Accidents and DT.Acc_code")
  print(DT.ret_ggmap)
}


## ALTERNATE FOR ADDING THE DATA BACK IN
if (FALSE) 
{
  loadFromJesus("DT.geocode_with_tamu", overwrite.ifexists=FALSE, dont.fail.ifexists=TRUE)
  stopifnot("join_id" %in% names(DT.geocode_with_tamu))
  stopifnot(c("latitude", "longitude") %ni% names(DT.Accidents))
  ## ELSE: 
  ##     DT.Accidents[, (colsToBring) := NULL]
  DT.Accidents <- merge(DT.Accidents, DT.geocode_with_tamu, by="join_id", all.x=TRUE, all.y=FALSE)
}


## --------------------------- ##
##      CREATE DT.Acc_code     ##
## --------------------------- ##

if ("latitude" %in% names(DT.Accidents)) {
    DT.Acc_code <- DT.Accidents[!is.na(latitude)]
    jesusForData(DT.Acc_code)
}


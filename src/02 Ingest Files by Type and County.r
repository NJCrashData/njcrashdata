source("~/git/NJCrashData/src/00 NJCrashData Setup.r")


files <- extractFilesFromFolder(ingest.p(), ext="txt", full=TRUE)

## files are of the form 
##   countyYEARtype.txt
files.info <- data.table(filename=names(files))
fn <- gsub("\\.txt$", "", names(files))
fn <- gsub("((1|2)\\d{3})", "_SPLIT_HERE_\\1_SPLIT_HERE_", fn)
files.info <- as.data.table(do.call(rbind, strsplit(fn, "_SPLIT_HERE_")))
files.info[, names(files.info) := lapply(.SD, as.character)]
setnames(files.info, c("county", "year", "report_type"))
files.info[, year := as.num.as.char(year)]
files.info[county == "CapeMay", county := "Cape May"]
files.info[, filename := names(files)]
files.info[, file := files]

setkeyIfNot(files.info,  c("year", "county", "report_type"), organize=TRUE, verbose=FALSE)
kCols.yc <- c("year", "county")

Counties <- files.info[, unique(county)]
Report_Types <- files.info[, unique(report_type)]

## Confrim that we have each report type per group
## Except for 2009 Burlington, for which the drivers data is bad. (Zip file has 0 Bytes)
stopifnot(files.info[, lapply(Report_Types,  "%in%", report_type), by=kCols.yc][!(year == 2009 & county == "Burlington"), all(as.logical(.SD)), by=kCols.yc][, V1])
# tmp_DT.missing <- files.info[, lapply(Report_Types,  "%in%", report_type), by=kCols.yc][, all(as.logical(.SD)), by=kCols.yc][!(V1)]
# DT.toDownload[tmp_DT.missing[, 1:2, with=FALSE]][, file.exists(file.out.zip)]



ll_DT.Drivers     <- ingest_report_type("Drivers",     minYear=minYear,  nj=FALSE)
ll_DT.Occupants   <- ingest_report_type("Occupants",   minYear=minYear,  nj=FALSE)
ll_DT.Pedestrians <- ingest_report_type("Pedestrians", minYear=minYear,  nj=FALSE)
ll_DT.Vehicles    <- ingest_report_type("Vehicles",    minYear=minYear,  nj=FALSE)
ll_DT.Accidents   <- ingest_report_type("Accidents",   minYear=minYear,  nj=FALSE)

## All of the main join_id from ll_DT.Accidents
DT.join_ids <- lapply(ll_DT.Accidents, function(DT) DT[, list(join_id=unique(join_id))])


### Not all join_ids are in all reports. 
# For an ID in ll_DT.Accidents

## Check where there are accidents without sub meta data
print(
sapply(Counties, function(Cnty) {
  c(
      Vehicles = length(setdiff(DT.join_ids[[Cnty]]$join_id, ll_DT.Vehicles[[Cnty]]$join_id))
    , Drivers = length(setdiff(DT.join_ids[[Cnty]]$join_id, ll_DT.Drivers[[Cnty]]$join_id))
    , Pedestrians = length(setdiff(DT.join_ids[[Cnty]]$join_id, ll_DT.Pedestrians[[Cnty]]$join_id))
    , Occupants = length(setdiff(DT.join_ids[[Cnty]]$join_id, ll_DT.Occupants[[Cnty]]$join_id))
    )
})
)

saveImageTo()


## --------------------------------------------------------------------------- ##
## --------------------------------------------------------------------------- ##
## --------------------------------------------------------------------------- ##
## --------------------------------------------------------------------------- ##
stop ("The Rest from here on in is confirming the different reports' joins")
stop ("The Rest from here on in is confirming the different reports' joins")
stop ("The Rest from here on in is confirming the different reports' joins")
stop ("The Rest from here on in is confirming the different reports' joins")
## --------------------------------------------------------------------------- ##
## --------------------------------------------------------------------------- ##
## --------------------------------------------------------------------------- ##



                          "Move on to the cleaning"



"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"














Ji <- DT.join_ids[[Cnty]]
Xi <- ll_DT.Drivers[[Cnty]][, 1:5, with=FALSE]

Cnty <- "Essex"
j <- max(head(setdiff(Xi$join_id, Ji$join_id), -10))
rng <- (-3) : 3
ll_DT.Accidents   [[Cnty]] [sunique(c(0, unlist(as.vector(sapply(which(join_id == j), "+", rng)))))]
ll_DT.Drivers     [[Cnty]] [sunique(c(0, unlist(as.vector(sapply(which(join_id == j), "+", rng)))))]
ll_DT.Vehicles    [[Cnty]] [sunique(c(0, unlist(as.vector(sapply(which(join_id == j), "+", rng)))))]
ll_DT.Occupants   [[Cnty]] [sunique(c(0, unlist(as.vector(sapply(which(join_id == j), "+", rng)))))]
ll_DT.Pedestrians [[Cnty]] [sunique(c(0, unlist(as.vector(sapply(which(join_id == j), "+", rng)))))]


as.vector(sapply(2:3, "+", (-5):5))
ll_DT.Drivers[[Cnty]][j]
ll_DT.Vehicles[[Cnty]][j]


mapply(function(Ji, Xi) {length(setdiff(Ji$join_id, Xi$join_id))}, DT.join_ids, ll_DT.Drivers)
mapply(function(Ji, Xi) {list(J_not_X = setdiff(Ji$join_id, Xi$join_id))}, ll_DT.Drivers, DT.join_ids)

mapply(function(Ji, Xi) {list(J_not_X = length(setdiff(Ji$join_id, Xi$join_id)))}, ll_DT.Drivers, DT.join_ids)
DT.join_ids <- lapply(ll_DT.Accidents, function(DT) DT[, list(join_id=unique(join_id))])

setdiff(Ji$join_id, Xi$join_id)
table(substr(setdiff(Xi$join_id, Ji$join_id), 1, 4))
Xi[j]
ll_DT.Drivers[[Cnty]][j]
ll_DT.Occupants[[Cnty]][j]
ll_DT.Pedestrians[[Cnty]][j]
ll_DT.Vehicles[[Cnty]][j]
ll_DT.Accidents[[Cnty]][j]

extract(pat="866", Ji$join_id)


ll_DT.Drivers[j = ]

parse_join_id("20111310DPD2011-000866         ")
parse_join_id(join_id)


  ## clean up the join_id
  ll_DT.Accidents[, c("Year", "County Code", "Municipality Code", "Department Case Number") := 
                    list( as.numeric(trim(substr(join_id, 1, 4)))
                        , as.numeric(trim(substr(join_id, 5, 6)))
                        , as.numeric(trim(substr(join_id, 7, 8)))
                        ,           (trim(substr(join_id, 9, 31)))
                        )
              ]

  ## Bank the original NAs, as we want to be sure not to intoduce any new ones
  tmp_NAs <- is.na(ll_DT.Accidents)

  ## Clean up dates
  ll_DT.Accidents[, date_of_crash := as.Date(date_of_crash, format="%m/%d/%Y")]
  ll_DT.Accidents[, day_of_crash  := weekdays(date_of_crash)]
  ## Confrim no errors:
  stopifnot(ll_DT.Accidents[, !is.na(date_of_crash)])
  stopifnot(ll_DT.Accidents[, !is.na(day_of_crash) & day_of_crash %in% weekdays(today() + 1:7)])

  ## Clean up badge number
  ll_DT.Accidents[, badge_numb := trim(gsub("\\#", "", badge_numb))]

  ## clean up numerics
  ll_DT.Accidents[, MilePost := as.numeric(trim(as.character(MilePost)))]

  convertNumberCols_(ll_DT.Accidents, verbose=TRUE)

  ## Confirm that no NAs where introduced
  stopifnot(tmp_NAs == is.na(ll_DT.Accidents))


  -------------------------------------

  Type <- "Drivers"
  ll_DT.Drivers <- rbindlist(lapply(files.info[year >= minYear] [report_type == "Drivers"]$file, function(f) suppressWarnings(fread(f, colClass="character"))))
  setnames(ll_DT.Drivers, gsub(" ", "_", colNames.raw[[Type]]))

  ## Confirm zipcodes are okay
  stopifnot(0 == nrow(ll_DT.Drivers[!is.na(`Driver_Zip_Code`)][(nchar(`Driver_Zip_Code`)) != 5]))

  ll_DT.Drivers[, table(Driver_Physical_Status)]
  factorCols <- c("Driver_State", "Driver_Zip_Code", "Driver_License_State", "Driver_Sex", "Alcohol_Test_Given", "Alcohol_Test_Type", "Alcohol_Test_Results", "Charge", "Summons", "Multi_Charge_Flag", "Driver_Physical_Status")
  ll_DT.Drivers[, (factorCols) := lapply(.SD, factor), .SDcols=factorCols]

  ## Convert some numbers back to numbers
  numbCols <- c("Alcohol_Test_Results", "Vehicle_Number")
  ll_DT.Drivers[, (numbCols) := lapply(.SD, as.numeric), .SDcols=numbCols]

  summary(ll_DT.Drivers)


Occupants
# iter :   }


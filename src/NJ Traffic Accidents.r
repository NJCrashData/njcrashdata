setProject(proj="NJTrafficAccidents", create=TRUE, subl=FALSE, load=FALSE)


minYear <- 2010

reportTypes <- c("Accidents", "Drivers", "Vehicles")
selfname_( reportTypes )
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

## Confrim that we have each report type per group
## Except for 2009 Burlington, for which the drivers data is bad. (Zip file has 0 Bytes)
stopifnot(files.info[, lapply(reportTypes,  "%in%", report_type), by=kCols.yc][!(year == 2009 & county == "Burlington"), all(as.logical(.SD)), by=kCols.yc][, V1])
# tmp_DT.missing <- files.info[, lapply(reportTypes,  "%in%", report_type), by=kCols.yc][, all(as.logical(.SD)), by=kCols.yc][!(V1)]
# DT.toDownload[tmp_DT.missing[, 1:2, with=FALSE]][, file.exists(file.out.zip)]



Y <- 2013
Cnty <- "Monmouth"
# iter :   for (f in files) {

  Type <- "Accidents"
  DT.accidents <- rbindlist(suppressWarnings(lapply(files.info[year >= minYear] [report_type == Type]$file, fread)))
  setnames(DT.accidents, gsub(" ", "_", colNames.raw[[Type]]))

  ## clean up the join_id
  DT.accidents[, c("Year", "County Code", "Municipality Code", "Department Case Number") := 
                    list( as.numeric(trim(substr(join_id, 1, 4)))
                        , as.numeric(trim(substr(join_id, 5, 6)))
                        , as.numeric(trim(substr(join_id, 7, 8)))
                        ,           (trim(substr(join_id, 9, 31)))
                        )
              ]

  ## Bank the original NAs, as we want to be sure not to intoduce any new ones
  tmp_NAs <- is.na(DT.accidents)

  ## Clean up dates
  DT.accidents[, date_of_crash := as.Date(date_of_crash, format="%m/%d/%Y")]
  DT.accidents[, day_of_crash  := weekdays(date_of_crash)]
  ## Confrim no errors:
  stopifnot(DT.accidents[, !is.na(date_of_crash)])
  stopifnot(DT.accidents[, !is.na(day_of_crash) & day_of_crash %in% weekdays(today() + 1:7)])

  ## Clean up badge number
  DT.accidents[, badge_numb := trim(gsub("\\#", "", badge_numb))]

  ## clean up numerics
  DT.accidents[, MilePost := as.numeric(trim(as.character(MilePost)))]

  convertNumberCols_(DT.accidents, verbose=TRUE)

  ## Confirm that no NAs where introduced
  stopifnot(tmp_NAs == is.na(DT.accidents))


  -------------------------------------

  Type <- "Drivers"
  DT.drivers <- rbindlist(lapply(files.info[year >= minYear] [report_type == "Drivers"]$file, function(f) suppressWarnings(fread(f, colClass="character"))))
  setnames(DT.drivers, gsub(" ", "_", colNames.raw[[Type]]))

  ## Confirm zipcodes are okay
  stopifnot(0 == nrow(DT.drivers[!is.na(`Driver_Zip_Code`)][(nchar(`Driver_Zip_Code`)) != 5]))

  DT.drivers[, table(Driver_Physical_Status)]
  factorCols <- c("Driver_State", "Driver_Zip_Code", "Driver_License_State", "Driver_Sex", "Alcohol_Test_Given", "Alcohol_Test_Type", "Alcohol_Test_Results", "Charge", "Summons", "Multi_Charge_Flag", "Driver_Physical_Status")
  DT.drivers[, (factorCols) := lapply(.SD, factor), .SDcols=factorCols]

  ## Convert some numbers back to numbers
  numbCols <- c("Alcohol_Test_Results", "Vehicle_Number")
  DT.drivers[, (numbCols) := lapply(.SD, as.numeric), .SDcols=numbCols]

  summary(DT.drivers)


Occupants
# iter :   }


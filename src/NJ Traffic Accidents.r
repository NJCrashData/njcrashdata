setProject(proj="NJTrafficAccidents", create=TRUE, subl=FALSE, load=FALSE)

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
Type <- "Accidents"
# iter :   for (f in files) {

  
  DT.accidents <- rbindlist(suppressWarnings(lapply(files.info[report_type == Type]$file, fread)))

  f.accidents  <- files[files.info[J(Y, Cnty, "Accidents")]$filename]
  DT.accidents <- suppressWarnings(fread(f.accidents))

  setnames(DT.accidents, colNames.raw[["Accidents"]])

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


# iter :   }


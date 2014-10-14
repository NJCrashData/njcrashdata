setProject(proj="NJTrafficAccidents", create=TRUE, subl=FALSE, load=FALSE)

reportTypes <- c("Accidents", "Drivers", "Vehicles")
selfname_( reportTypes )
files <- extractFilesFromFolder(ingest.p(), ext="txt", full=TRUEa)

## files are of the form 
##   countyYEARtype.txt
files.info <- data.table(filename=names(files))
fn <- gsub("\\.txt$", "", names(files))
fn <- gsub("((1|2)\\d{3})", "_SPLIT_HERE_\\1_SPLIT_HERE_", fn)
files.info <- as.data.table(do.call(rbind, strsplit(fn, "_SPLIT_HERE_")))
files.info[, names(files.info) := lapply(.SD, as.character)]
setnames(files.info, c("county", "year", "report_type"))
files.info[, year := as.numeric(as.character(year))]
files.info[, filename := names(files)]

setkeyIfNot(files.info,  c("year", "county", "report_type"), organize=TRUE)
kCols.yc <- c("year", "county")

## Confrim that we have each report type per group
stopifnot(files.info[, lapply(reportTypes,  "%in%", report_type), by=kCols.yc][, all(as.logical(.SD)), by=kCols.yc][, V1])

Y <- 2013
Cnty <- "Monmouth"
# iter :   for (f in files) {

  
  
  f.accidents <- files[files.info[J(Y, Cnty, "Accidents")]$filename]
  DT.accidents <- suppressWarnings(fread(f.accidents, nrow=10))
  setnames(DT.accidents, colNames.raw[["Accidents"]])

  ## clean up the join_id
  DT.accidents[, c("Year", "County Code", "Municipality Code", "Department Case Number") := 
                    list( as.numeric(trim(substr(join_id, 1, 4)))
                        , as.numeric(trim(substr(join_id, 5, 6)))
                        , as.numeric(trim(substr(join_id, 7, 8)))
                        ,           (trim(substr(join_id, 9, 31)))
                        )
              ]

  ## Clean up dates
  DT.accidents[, date_of_crash := as.Date(date_of_crash, format="%m/%d/%Y")]
  DT.accidents[, day_of_crash  := weekdays(date_of_crash)]

  ## Clean up badnumber
  DT.accidents[, badge_numb := as.numeric(gsub("\\#", "", badge_numb))]

  ## clean up numerics
  DT.accidents[, "MilePost" := as.numeric(MilePost)]

  convertNumberCols_(DT.accidents, verbose=TRUE)


# iter :   }


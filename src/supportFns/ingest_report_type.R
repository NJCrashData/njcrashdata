ingest_report_type <- function(Type, minYear=2006, NJ=FALSE, Counties=getCounties(NJ.included=NJ))
{

  ## -------------------------------------------------------------- ##
  ## This function expects the following objects to already exist: 
  ##    Counties;   files.info;  colNames.raw;
  ## -------------------------------------------------------------- ##

  if (minYear < 2006)
    warning ("The NJTR-1 form changed from 2005 to 2006. Data may not clean correctly")

  cat (sprintf("  %20s[ %s ] \n", "", Type))
  ll_DT.counties <- emptylist(Counties)

  colNames <- colNames.raw[[Type]]
  colNames <- gsub("- ", "-", colNames)
  colNames <- gsub(" ", "_", colNames)
  colClass <- if (any(grepl("zip", colNames, ignore.case=TRUE))) "character" else NULL

  for (Cnty in Counties) {
    cat (Cnty, "...", sep="")

    ll_DT.counties[[Cnty]] <- 
      setkey(setnames(rbindlist(
                lapply(files.info[year >= minYear & county == Cnty & report_type == Type]$file, function(f) {
                      tryCatch(
                        ## fread doesn't read the whole file but fails to indicate
                        # suppressWarnings(fread(f, colClass=colClass))
                        ## Using read.table for now
                        as.data.table(read.table(f, header=FALSE, sep=",", comment.char="", quote="", colClass="character"))
                      , error = function(e) {cat("\nAbout to crash for file '", f, "'\n\nERROR THROWN:\n", e$message, "\n", sep="")}
                      )
                })
      ), colNames), join_id)
  }
  cat (" [DONE]\n")

  return(ll_DT.counties)
}
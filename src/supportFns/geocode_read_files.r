
read_all_tamu_files_and_add_to_DT_ <- function(DT, colsToBring=c("latitude", "longitude", "match_type", "matched_location_type"), verbose=TRUE) {

    if (any(colsToBring %in% names(DT)))
        stop ("Some values of colsToBring are already in DT\n\nHINT Run:    DT.Accidents[, (colsToBring) := NULL]")

    files <- dir(data.p("geocode_tamu_results"), recursive=TRUE, full=TRUE, pattern="\\.csv$")
    files <- files[!grepl("join_ids_yielding_badrequests|scratch", files)]
    setattr(files, "names", basename(files))
    verboseMsg(TRUE, "reading", length(files), "files of previously geocoded IDs", minw=88)

    ll.ret <- list()
    for (i in seq(files)) {
        file_out <- files[[i]]
        nm <- basename(file_out)
        ll.ret[[nm]] <- geocode_tamu_read_file(file_out, verbose=verbose)
    }

    ll.ret <- ll.ret[!sapply(ll.ret, function(DT) all(is.na(DT[["latitude"]]) & DT[["match_type"]] != "bad_request"))]

    ## Flatten
    DT.geocode_with_tamu <- rbindlist(ll.ret)
    ## ERROR CHECK
    if (!nrow(DT.geocode_with_tamu) || "latitude" %ni% names(DT.geocode_with_tamu))
        stop ("Internal error.  DT.geocode_with_tamu did not process correctly")

    DT.geocode_with_tamu <- DT.geocode_with_tamu[!(is.na(latitude) & match_type != "bad_request")]
    DT.geocode_with_tamu <- unique(DT.geocode_with_tamu, by=NULL)
    setnames(DT.geocode_with_tamu, "internal_id", "join_id")
    setkeyIfNot(DT.geocode_with_tamu, join_id, verbose=FALSE)


    ## Handle Duplicate values of join_id
    ## -----------------------------------
    ## Note that straight-duplicates (ie, having exact identical information) were cleared up above
    ## Remaining duplicates per id have differing data.
    ## If the geo data is NA for any of the dups, simply remove those
    ## If duplicates remain (ie, a single join_id has different lat/lng data)
    ##     then this needs manual attention (notify user)
    ## To remove duplicates, create a temporary column, 'secondary' that 
    ##    checks for N > 1 and for any NA in latitude. 
    DT.geocode_with_tamu[, N_by_id := .N, by=join_id]
    DT.geocode_with_tamu[, secondary := N_by_id > 1 & (is.na(latitude))]
    ## In the (very rare) event that there is an ID with all rows labeled as secondary,
    ##   change the first one to non-secondary
    DT.geocode_with_tamu[DT.geocode_with_tamu[, all(secondary), by=join_id][(V1)], secondary := c(TRUE, rep(FALSE, .N-1)), by=join_id]
    ## Confirm not all are marked secondary
    if (nrow(DT.geocode_with_tamu[, all(secondary), by=join_id][(V1)]))
        warning ("There was an internal error. Some join_id s of DT.geocode_with_tamu have been marked as fully secondary. This means they will be dropped altogether and not added back into the original DT")
    ## Drop any secondary rows
    DT.geocode_with_tamu <- DT.geocode_with_tamu[!(secondary)]
    suppressWarnings(DT.geocode_with_tamu[, c("secondary", "N_by_id") := NULL])

    ## Check for remaining duplicates and notify the user if any remain
    ## The above only drops rows with NAs in the latitude.
    ## If any further duplicates remain, this means that they received more than one geocode location
    ## Notify user, as this might require manual attention
    dups <- DT.geocode_with_tamu[, .N, keyby=join_id][N > 1]
    if (nrow(dups)) {
        # print out a sampling, ±2 rows
        warning ("Some join_ids have more than one value", call.=TRUE)
        print(DT.geocode_with_tamu[dups[sort(sample(nrow(dups), 10))]])
    }

    file_jesus <- jesusForData(DT.geocode_with_tamu, envir=environment(), verbose=FALSE)

    if (!any(colsToBring %in% names(DT))) {
      addColsFrom_(DT.receiving=DT, DT.giving=DT.geocode_with_tamu, colsToBring=colsToBring, joinCols="join_id")
    }

    return(invisible(DT))
}

## If there is an issue with a text file, use this to troubleshoot
if (FALSE) {
    file_out <- files[["tamu_results_20150419_230917.csv"]]
    fread(file_out, sep=",", header=FALSE)
    subl(file_out)
}

geocode_tamu_read_file <- function(
    file_out
  , format="csv"
  , verbose=TRUE
  , colsToReturn = c("internal_id", "latitude", "longitude", "match_type", "matched_location_type")
  , warn_bad = FALSE
  , ...
) {

## DEPENDS ON:   collectArgs;  removeNA;  %ni%;  data.p; verboseMsg; formnumb

    require("data.table")

    verboseMsg(verbose, "Processing '", path.unexpand(file_out), "'", sep="", time=FALSE)

    if (!is.null(colsToReturn) && !is.character(colsToReturn))
        warning ("colsToReturn should be a character vector or should be NULL")

    ## Get colnames for parsed results
    geocode_tamu_colnames <- get_geocode_tamu_colnames(census=census, censusYear=censusYear)
    geocode_tamu_colnames <- c(character="internal_id", geocode_tamu_colnames, character="url_args")

    DT.ret <- try(fread(file_out, colClasses=names(geocode_tamu_colnames), header=FALSE))

    ## If fread fails, return the file name, so that user can attempt to troubleshoot
    if (inherits(DT.ret, "try-error")) {
        warning ("Could not fread ", file_out, "   Returning file name")
        return(file_out)
    }

    ## Cleanup the DT.ret
    setnames(DT.ret, geocode_tamu_colnames)

    badRequests <- DT.ret$transaction_id == "bad_request"
    if (any(badRequests)) {
        if (warn_bad)
        warning ("Bad requests for ", sum(badRequests), " of the addresses. These will be flagged with 'bad_request' for values in the fields 'transaction_id', 'match_type' & 'matched_location_type'")
        ## Write bad IDs to file
        ## set appropriate columns to "bad_request"
        DT.ret[badRequests, c("match_type", "matched_location_type") := "bad_request"]
    }

    ## Check for any non-200 status, other than the bad_request
    DT.ret[query_status_code != 200 & transaction_id != 'bad_request' & query_status_code != '', if (.N) warning ("There are ", .N, " non-standard (200) query_status_code in addition to any 'bad_request' flags")]

    if (!is.null(colsToReturn) && is.character(colsToReturn) && !all(tolower(colsToReturn) == "all")) {
        if (any(colsToReturn %ni% names(DT.ret)))
            warning ("The following values of colsToReturn are NOT in the names of DT.ret: ", paste(setdiff(colsToReturn, names(DT.ret)), collapse=", "))
        else
            DT.ret <- DT.ret[, colsToReturn, with=FALSE]
    }

    return(DT.ret)
}

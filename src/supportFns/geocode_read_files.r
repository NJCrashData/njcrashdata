
if (FALSE) 
{
    cls(10)

    files <- dir(data.p(), recursive=TRUE, full=TRUE, pattern="\\.csv$")
    files <- files[!grepl("join_ids_yielding_badrequests|scratch", files)]
    setattr(files, "names", basename(files))

    ll.ret <- list(); cat("\n\n\n\n")
    for (i in seq(files)) {
        file_out <- files[[i]]
        nm <- basename(file_out)
        cat ("\n ---------------------- ", nm, " ---------------------------- \n")
        ll.ret[[nm]] <- geocode_tamu_read_file(file_out)
    }

    ll.ret <- ll.ret[!sapply(ll.ret, function(DT) all(is.na(DT[["latitude"]]) & DT[["match_type"]] != "bad_request"))]
    ## Flatten
    DT.geocode_with_tamu <- rbindlist(ll.ret)
    DT.geocode_with_tamu <- DT.geocode_with_tamu[!(is.na(latitude) & match_type != "bad_request")]
    DT.geocode_with_tamu <- unique(DT.geocode_with_tamu, by=NULL)
    setnames(DT.geocode_with_tamu, "internal_id", "join_id")
    setkeyIfNot(DT.geocode_with_tamu, join_id, verbose=FALSE)
    jesusForData(DT.geocode_with_tamu)

    DT.geocode_with_tamu[DT.geocode_with_tamu[, .N, by=join_id][N>1]]
    DT.Accidents <- merge(DT.Accidents, DT.geocode_with_tamu, by="join_id", all.x=TRUE, all.y=FALSE)
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

    verboseMsg(verbose, "API results written to  \"", file_out, "\"    ", minw=82, sep="")

    return(DT.ret)
}

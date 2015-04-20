
if (FALSE) 
{

    files <- dir(data.p(), recursive=TRUE, full=TRUE, pattern="\\.csv$")
    files <- files[!grepl("join_ids_yielding_badrequests", files)]
    setattr(files, "names", basename(files))

    ll.ret <- list()
    for (i in seq(files)) {
        file_out <- files[[i]]
        nm <- basename(file_out)
        cat ("\n ---------------------- ", nm, " ---------------------------- \n")
        ll.ret[[nm]] <- geocode_tamu_read_file(file_out)
        cat("\n")
    }

    ll.ret <- lapply(files, geocode_tamu_read_file)
}

if (FALSE) {

    file_out <- files[["tamu_results_20150419_213155.csv"]]
    fread(file_out, sep=",", header=FALSE)
    subl(file_out)
}

geocode_tamu_read_file <- function(
    file_out
  , format="csv"
  , verbose=TRUE
  , colsToReturn = c("latitude", "longitude", "match_type", "matched_location_type")
  , bad_request_file = data.p("geocode_tamu_results", "join_ids_yielding_badrequests", ext="csv")
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
        warning ("Bad requests for ", sum(badRequests), " of the addresses. These will be flagged with 'bad_request' for values in the fields 'transaction_id', 'match_type' & 'matched_location_type'")
        ## Write bad IDs to file
        if (!file.exists(dirname(bad_request_file)))
            dir.create(dirname(bad_request_file), recursive=TRUE, showWarnings=FALSE)
        try(DT.ret[badRequests, write(x=internal_id, ncolumns=1, file=bad_request_file, append=TRUE, sep="\n")])
        ## set appropriate columns to "bad_request"
        DT.ret[badRequests, c("match_type", "matched_location_type") := "bad_request"]
    }

    ## Check for any non-200 status, other than the bad_request
    DT.ret[query_status_code != 200 & transaction_id != 'bad_request', if (.N) warning ("There are ", .N, " non-standard (200) query_status_code in addition to any 'bad_request' flags")]

    if (!is.null(colsToReturn) && is.character(colsToReturn) && !all(tolower(colsToReturn) == "all")) {
        if (any(colsToReturn %ni% names(DT.ret)))
            warning ("The following values of colsToReturn are NOT in the names of DT.ret: ", paste(setdiff(colsToReturn, names(DT.ret)), collapse=", "))
        else
            DT.ret <- DT.ret[, colsToReturn, with=FALSE]
    }

    verboseMsg(verbose, "API results written to  \"", file_out, "\"    ", minw=82, sep="")

    return(DT.ret)
}

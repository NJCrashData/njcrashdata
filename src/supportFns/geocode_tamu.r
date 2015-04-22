options(tamu_geo.apikey="0c0ce600dda74d7dba29fd713602d05e")  # Uni
options(tamu_geo.apikey="4dabd5979f144327b0c7f50bf95aed68")  # Personal, Gmail main
options(tamu_geo.apikey="d52d68faa2154dc89c964ec4cee702bd")  # Personal, Gmail2
options(tamu_geo.apikey=c(NJCrashData="3a245f981d7c4f43917d9b2ea290013c"))  # NJCrashData@gmail.com
options(tamu_geo.apikey="f295003c76924a5ebbed43d4ef9ce662")  # tester.rs.to01@gmail.com


geocode_tamu <- function(
    streetAddress=NULL
  , city=NULL
  , state=NULL
  , zip=NULL
  , apikey=getOption("tamu_geo.apikey")
  , format="csv"
  , census="false"
  , censusYear="2000|2010"
  , notStore="false"
  , version="4.01"
  , base_url = "http://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?"
  , check_states = TRUE
  , verbose=TRUE
  , internal_id = NULL
  , colsToReturn = c("latitude", "longitude", "match_type", "matched_location_type")
  , folder = data.p("geocode_tamu_results")
  , file_name = paste0("tamu_results_", file_time_stamp, ".", tolower(format))
  , file_time_stamp = format(Sys.time(), "%Y%m%d_%H%M%S")
  , iter_size = 10000
  , break_on_blank = TRUE
  , bad_request_file = data.p("geocode_tamu_results", "join_ids_yielding_badrequests", ext="csv")
  , ...
) {

## DEPENDS ON:   collectArgs;  removeNA;  %ni%;  data.p; verboseMsg; formnumb

    require("RCurl")
    require("data.table")

    if (!length(streetAddress) & !length(city) & !length(zip) & !(length(state) > 1)) {
        message("All address fields are zero-length. Are you done geocoding? Great Job!\nReturning NULL")
        return(NULL)
    }

    if (!is.null(colsToReturn) && !is.character(colsToReturn))
        warning ("colsToReturn should be a character vector or should be NULL")

    ## Most function arguments are part of the api call, except for the following
    ## These will not be used in the final output
    non_api_args <- c("non_api_args", "base_url", "check_states", "verbose", "internal_id"
                    , "colsToReturn", "iter_size", "break_on_blank", "bad_request_file"
                    , "folder", "file_name", "file_time_stamp")

    ## CLEAN UP ARGUMENT INPUTS
    ## (1) all NAs should be changed to ''
    # (2) all logical arugments should be changed to 'true', 'false'UR
    Args <- collectArgs(except=non_api_args, incl.dots=TRUE)
    for (arg_name in names(Args)) {
        assign(arg_name, removeNA(Args[[arg_name]], replace=''))
    }
    rm(Args, arg_name)

    ## Cleanup logical args

    ## Clean up state values
    if (isTRUE(check_states)) {
        state <- toupper(state)
        if (any(state %ni% c(state.abb, "DC")))
            warning ("Some state values are invalid")
    }

    ## when CENSUS is TRUE, this requires different parsing of the return text
    if (isTRUE(census) || (is.character(census) && any(tolower(census) == "TRUE")))
        stop ("Do not know how to parse for census")


    ## Construct the URLS
    ## paste together string, first re-collecting the arguments
    Args_for_url <- collectArgs(except=c("Args", non_api_args), incl.dots=TRUE)
    Args_for_url <- Args_for_url[!sapply(Args_for_url, is.null)]
    Args_for_url <- sapply(names(Args_for_url), function(nm) paste0(nm, "=", Args_for_url[[nm]]))
    Args_for_url <- do.call(paste, c(as.list(Args_for_url), sep="&"))
    URLS <- paste0(base_url, Args_for_url)
    # URLS <- gsub(" ", "%20", URLS) ## TODO use sapply(..., URLencode)
    setattr(URLS, "names", Args_for_url)
    URLS <- sapply(URLS, URLencode)

    ## ERROR CHECK:  Confirm that there is at least one URL created
    if (!length(URLS)) {
        warning ("No URLS created - returning NULL")
        return(NULL)
    }

    ## Get colnames for parsed results
    geocode_tamu_colnames <- get_geocode_tamu_colnames(census=census, censusYear=censusYear)

    ## Confirm internal_id is valid, if not, set to NULL (ie do not use)
    if (!is.null(internal_id)) {
        L.id <- length(internal_id)
        if (L.id > 1 && L.id != length(URLS)) {
            warning (sprintf("differing lengths between internal_id (%i) and rows of DT.ret (%i)\ninternal_id will not be used"), L.id, nrow(DT.ret))
            internal_id <- NULL
        } else {
            internal_id <- paste0(removeNA(internal_id, replace=""), ",")
            geocode_tamu_colnames <- c(character="internal_id", geocode_tamu_colnames)
        }
    }

    ## Write data to file
    file_out <- file.path(folder, file_name)
    verboseMsg(verbose, "API results will be written to\n   \"", file_out, "\"    ", minw=82, sep="")


    ## Create the file and directory
    if (!file.exists(dirname(file_out)))
        dir.create(dirname(file_out), recursive=TRUE)

    ## Break up the URLS into iterations, that way partial results are not lost
    iter_starts <- seq.int(from=1, to=length(URLS), by=iter_size)
    iter_ends   <- iter_starts + iter_size - 1
    iter_ends[length(iter_ends)] <- length(URLS)

    if (length(iter_starts) != length(iter_ends) || !(length(iter_starts) >= 1) || !all(iter_ends >= iter_starts)) {
        stop ("Internal Error:  iter_starts or iter_ends are invalid. Error occurred right before iterating over URLS")
    }

    
    ## string format for verbose output
    fmt.iter <- sprintf("Querying TAMU API for addresses # %% %1$ii -%% %1$ii (of %%s total)", 1+ceiling(log10(length(URLS))))


    ## Check available api_credits
    try({
        api_credits <- get_available_credits_tamu(apikey=apikey)
        if (api_credits == 0 && isTRUE(break_on_blank)) {
            warning ("The given apikey has zero credits remaining before even starting api call.\nExiting and returning NULL", call.=FALSE)
            return(NULL)
        } else if (api_credits < length(URLS)) {
            warning ("The given apikey has only ", formnumb(api_credits), " remaining credits -- less than the ", formnumb(length(URLS)), " calls to be made", call.=FALSE)
        }
    })

    # handle <- getCurlHandle()
    multiHandle = getCurlMultiHandle()
    getURIAsynchronous
    for(i in seq(iter_starts)) {
        start <- iter_starts[[i]]
        end   <- iter_ends  [[i]]
        inds <- seq.int(from=start, to=end, by=1)
        URL_batch <- URLS[inds]
        internal_id_batch <- internal_id[inds]
        verboseMsg(verbose, sprintf(fmt.iter, start, end, formnumb(length(URLS), round=FALSE)), minw=82, frmt="%H:%M:%S %Z", time=TRUE)
        ## Hit the API, request results
        # sans URLencode :: url_results <- try(getURL( gsub(" ", "%20", URL_batch), curl=handle )) ## TODO use sapply(..., URLencode)
        # url_results <- try( getURL(url=URL_batch, curl=handle, async=FALSE) )
        url_results <- try( getURIAsynchronous(url=URL_batch, multiHandle=multiHandle) )
        if (isErr(url_results)) {
            warning("getURL() resulted in an error for the last batch. Exiting for loop. Sample URL:\n", tail(URL_batch, 1), call.=FALSE)
            break
        }


        ## CSV results issues unnecessary linebreak. 
        ## This line might cause an issue if returning JSON instead of CSV
        ## TODO, cehck for format != "csv"
        url_results <- sub(",\\r\\n$", "", url_results)

        ## Check for blanks before pasting
        blanks   <- url_results == ""
        anyBlank <- any(blanks)

        ## blanks will mess up the fread() command; thus need to pad it
        single_blank_line <- paste(rep(",", length(setdiff(geocode_tamu_colnames, "internal_id")) - 1), collapse="")
        url_results[blanks] <- single_blank_line

        ## Check for bad requests
        ## If found, insert 'bad_request' into the first field, which is the transaction_id
        badRequests <- grepl("^\\s*invalid request - ", url_results)
        if (any(badRequests)) {
            url_results[badRequests] <- paste0("bad_request", single_blank_line)
        }

        ## add in internal_id and argument-portion of URL
        url_results <- paste0(internal_id_batch, url_results, ",", names(URL_batch))
        write(x=url_results, file=file_out, ncolumns=1, append=TRUE, sep="\n")

        if (isTRUE(anyBlank)) {
            credits <- try(get_available_credits_tamu(apikey=apikey), silent=TRUE)
            if (isErr(credits) || !credits) {
                warning ("Out of api credits. Breaking out of for-loop", call.=FALSE)
                break
            }
        }
    }

    ## Ingest the files
    geocode_tamu_colnames <- c(geocode_tamu_colnames, character="url_args")
    DT.ret <- try(fread(file_out, colClasses=names(geocode_tamu_colnames), header=FALSE))

    ## If fread fails, return the file name, so that user can attempt to troubleshoot
    if (inherits(DT.ret, "try-error")) {
        warning ("Could not fread ", file_out, "   Returning file name", call.=FALSE)
        return(file_out)
    }

    ## Cleanup the DT.ret
    setnames(DT.ret, geocode_tamu_colnames)

    badRequests <- DT.ret$transaction_id == "bad_request"
    if (any(badRequests)) {
        warning ("Bad requests for ", sum(badRequests), " of the addresses. These will be flagged with 'bad_request' for values in the fields 'transaction_id', 'match_type' & 'matched_location_type'", call.=FALSE)
        ## Write bad IDs to file
        if (!file.exists(dirname(bad_request_file)))
            dir.create(dirname(bad_request_file), recursive=TRUE, showWarnings=FALSE)
        try(DT.ret[badRequests, write(x=internal_id, ncolumns=1, file=bad_request_file, append=TRUE, sep="\n")])
        ## set appropriate columns to "bad_request"
        DT.ret[badRequests, c("match_type", "matched_location_type") := "bad_request"]
    }

    ## Check for any non-200 status
    ## Ignore '' since this will happen when api_credits runs out mid-iteration
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

get_geocode_tamu_colnames <- function(census, censusYear, ...) {
## Returns a vecotr where the names of the values are the colClasses for the API return values
##   and the values of the vector are the column names
## 
## TODO:  This should vary based on CENSUS value

    geocode_tamu_colnames <- c(
       character  =  "transaction_id"
    ,  numeric    =  "api_version"
    ,  character  =  "query_status_code"
    ,  numeric    =  "latitude"
    ,  numeric    =  "longitude"
    ,  character  =  "naaccr_gis_coordinate_quality_code"
    ,  character  =  "naaccr_gis_coordinate_quality_name"
    ,  numeric    =  "match_score"
    ,  character  =  "match_type"
    ,  character  =  "feature_match_type"
    ,  integer    =  "feature_match_count"
    ,  character  =  "matching_geography_type"
    ,  numeric    =  "region_size"
    ,  character  =  "region_size_units"
    ,  character  =  "matched_location_type"
    ,  numeric    =  "time_taken"
    )

    return(geocode_tamu_colnames)
}

get_available_credits_tamu <- function(apikey=getOption("tamu_geo.apikey"), verbose=FALSE) {
    URL <- paste0("https://geoservices.tamu.edu/UserServices/Payments/Balance/AccountBalanceWebServiceHttp.aspx?version=1.0&apikey=",apikey,"&format=csv")
    verboseMsg(verbose, "Querying API using address\n     ", URL, "\n", time=FALSE)
    ret <- RCurl::getURL(URL)
    as.integer(strsplit(ret, ",")[[c(1, 2)]])
}


geocode_google <- function(
    addresses=NULL
  , city=NULL
  , state=NULL
  , zip=NULL
  , location
  , sensor=FALSE
  , base_url = "http://maps.googleapis.com/maps/api/geocode/json?"

  ## from ggmap 
  , output="all" # c("latlon", "latlona", "more", "all")
  , messaging=TRUE
  , override_limit=FALSE

  , verbose=TRUE
  , internal_id = NULL
  , colsToReturn = c("latitude", "longitude", "match_type", "matched_location_type")
  , folder = data.p("geocode_ggmap_results")
  , file_name = paste0("ggmap_results_", file_time_stamp, ".", "psv")
  , file_time_stamp = format(Sys.time(), "%Y%m%d_%H%M%S")
  , file_sep = "|"
  , iter_size = 100
  , break_on_blank = TRUE
  , bad_request_file = data.p("geocode_ggmap_results", "join_ids_yielding_badrequests", ext="csv")
  , ...
) {


    # stop ("This function is incomplete")

    output <- match.arg(output, choices=c("latlon", "latlona", "more", "all"), several.ok=FALSE)

    if (missing(location)) {
        Args <- c("streetAddress", "city", "state", "zip")
        setattr(Args, "names", gsub("streetAddress", "addresses", Args))
        Args <- lapply(Args, function(a) get(a))
        Args <- Args[!sapply(Args, is.null)]
        Args <- lapply(names(Args), function(nm) paste(nm, Args[[nm]], sep="="))
        location <- do.call(paste, c(Args, sep=","))
    }

    ## Use either location or its names as names
    nms <- if (is.null(names(location))) location else names(location)

    ## set the names for location
    if (is.null(names(location)))
        setattr(location, "names", nms)

    ## Spaces get converted to plus signs
    location <- gsub(" ", "+", location)

    ## paste together the URL
    URLS <- paste0(
        base_url,
        location,
        "&sensor=", tolower(as.character(sensor))
        )
    setattr(URLS, "names", names(location))
    URLS <- sapply(URLS, URLencode)

    ## TODO
    stopifnot(length(internal_id) != length(URLS))

    ## Write data to file
    file_out <- file.path(folder, file_name)
    if (!file.exists(file_out)) {
        dir.create(dirname(file_out), showWarnings=FALSE, recursive=TRUE)
        write(x=c("join_id", "raw_json"), file=file_out, ncolumns=2, append=TRUE, sep=file_sep)
    }
    verboseMsg(verbose, "API results will be written to\n   \"", file_out, "\"    ", minw=82, sep="")

    ## Output
    verboseMsg(verbose, "Will attempt to geocode", length(URLS), "addresses\n")

    ## Execute
    url_results <- getURL(URLS)

    ## results across the file according to 'top-down' in the matrix
    ## Thus use t(cbind(..))  --  or equivalently rbind(..)
    write(x=rbind(internal_id, url_results), file=file_out, ncolumns=2, append=TRUE, sep=file_sep)

    if (any(grepl("You have exceeded your rate", url_results))) {
        warning ("rate limit exceeded for API", call.=FALSE)
        break
    }
    setattr(url_results, "names", nms)

    ## Capture errors and blanks
    errors <- grepl("error_message", url_results)
    blanks <- grepl("ZERO_RESULTS",  url_results)

    ### ----- DID 
    parsed <- lapply(url_results, rjson::fromJSON)

    error_messages <- if (any(errors)) sapply(parsed, "[[", "error_message")
    statusses <- sapply(parsed, "[[", "status")
    results   <- sapply(parsed, "[[", "results")

    ## TODO:  Check for more than one geometry
    latlon <- lapply(results, function(x) c(latitude=x$geometry$location$lat, longitude=x$geometry$location$lng, location_type=x$geometry$location_type))

    ## Combine internal_id with an rbind of the latlon data
    DT.ret <- data.table(internal_id, do.call(rbind, latlon))

    "&& TODO:  Finish parsing.   No results available since all are showing over rate limit"

    ###  The following is from ggmap::geocode
    ## ggmap source :     gc <- fromJSON(paste(readLines(connect), collapse = ""))
    ## ggmap source :     if (messaging)
    ## ggmap source :         message(" done.")
    ## ggmap source :     close(connect)
    ## ggmap source :     if (output == "all")
    ## ggmap source :         return(gc)
    ## ggmap source :     message(paste0("Information from URL : ", url_string))
    ## ggmap source :     message("Google Maps API Terms of Service : http://developers.google.com/maps/terms")
    ## ggmap source :     if (gc$status != "OK") {
    ## ggmap source :         warning(paste("geocode failed with status ", gc$status,
    ## ggmap source :             ", location = \"", location, "\"", sep = ""), call. = FALSE)
    ## ggmap source :         return(data.frame(lon = NA, lat = NA))
    ## ggmap source :     }
    ## ggmap source :     if (length(gc$results) > 1 && messaging) {
    ## ggmap source :         message(paste("more than one location found for \"",
    ## ggmap source :             loc, "\", using address\n  \"", tolower(gc$results[[1]]$formatted_address),
    ## ggmap source :             "\"\n", sep = ""))
    ## ggmap source :     }
    ## ggmap source :     NULLtoNA <- function(x) {
    ## ggmap source :         if (is.null(x))
    ## ggmap source :             return(NA)
    ## ggmap source :         x
    ## ggmap source :     }
    ## ggmap source :     gcdf <- with(gc$results[[1]], {
    ## ggmap source :         data.frame(lon = NULLtoNA(geometry$location$lng), lat = NULLtoNA(geometry$location$lat),
    ## ggmap source :             type = tolower(NULLtoNA(types[1])), loctype = tolower(NULLtoNA(geometry$location_type)),
    ## ggmap source :             address = tolower(NULLtoNA(formatted_address)), north = NULLtoNA(geometry$viewport$northeast$lat),
    ## ggmap source :             south = NULLtoNA(geometry$viewport$southwest$lat),
    ## ggmap source :             east = NULLtoNA(geometry$viewport$northeast$lng),
    ## ggmap source :             west = NULLtoNA(geometry$viewport$southwest$lng))
    ## ggmap source :     })
    ## ggmap source :     if (output == "latlon")
    ## ggmap source :         return(gcdf[, c("lon", "lat")])
    ## ggmap source :     if (output == "latlona")
    ## ggmap source :         return(gcdf[, c("lon", "lat", "address")])
    ## ggmap source :     attrdf <- ldply(gc$results[[1]]$address_components, function(l) {
    ## ggmap source :         as.data.frame(l, stringsAsFactors = FALSE)[1, ]
    ## ggmap source :     })
    ## ggmap source :     attrdf <- attrdf[, c("types", "long_name")]
    ## ggmap source :     gcdf <- within(gcdf, {
    ## ggmap source :         point_of_interest <- tolower(NULLtoNA(attrdf$long_name[attrdf$types ==
    ## ggmap source :             "point_of_interest"]))
    ## ggmap source :         streetNo <- as.numeric(NULLtoNA(attrdf$long_name[attrdf$types ==
    ## ggmap source :             "street_number"]))
    ## ggmap source :         street <- tolower(NULLtoNA(attrdf$long_name[attrdf$types ==
    ## ggmap source :             "route"]))
    ## ggmap source :         locality <- tolower(NULLtoNA(attrdf$long_name[attrdf$types ==
    ## ggmap source :             "locality"]))
    ## ggmap source :         administrative_area_level_1 <- tolower(NULLtoNA(attrdf$long_name[attrdf$types ==
    ## ggmap source :             "administrative_area_level_1"]))
    ## ggmap source :         administrative_area_level_2 <- tolower(NULLtoNA(attrdf$long_name[attrdf$types ==
    ## ggmap source :             "administrative_area_level_2"]))
    ## ggmap source :         country <- tolower(NULLtoNA(attrdf$long_name[attrdf$types ==
    ## ggmap source :             "country"]))
    ## ggmap source :         postal_code <- tolower(NULLtoNA(attrdf$long_name[attrdf$types ==
    ## ggmap source :             "postal_code"]))
    ## ggmap source :     })
    ## ggmap source :     gcdf$query <- loc
    ## ggmap source :     return(gcdf)

    return (DT.ret)
}



# keys <- c(
#   "0c0ce600dda74d7dba29fd713602d05e"
# , "4dabd5979f144327b0c7f50bf95aed68"
# , "d52d68faa2154dc89c964ec4cee702bd"
# , "f295003c76924a5ebbed43d4ef9ce662"
# , "3a245f981d7c4f43917d9b2ea290013c"
# )
# selfname_(keys)
# sapply(keys, get_available_credits_tamu)


## This is not currently used and still needs work

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
    verboseMsg(verbose, "API results will be written to\n   \"", file_out, "\"    ", minw=82, sep="", time=TRUE, frmt="%H:%M:%S %Z")

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


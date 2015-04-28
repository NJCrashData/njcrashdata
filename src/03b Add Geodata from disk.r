
## This is not yet a proper function. 
## Call this with source

cls(3)
{
  try(cat("Available Credits : ", get_available_credits_tamu(), "\n"), silent=TRUE)
  if ("latitude" %in% names(DT.Accidents))
    message("Current number of non-NAs in latitude is ", formnumb(DT.Accidents[!is.na(latitude), .N], round=FALSE))
  colsToBring <- c("latitude", "longitude", "match_type", "matched_location_type")
  invisible(suppressWarnings(DT.Accidents[, (colsToBring) := NULL]))
  read_all_tamu_files_and_add_to_DT_(DT=DT.Accidents, colsToBring=colsToBring, verbose=FALSE)
  if ("latitude" %in% names(DT.Accidents))
    message("Updated number of non-NAs in latitude is ", formnumb(DT.Accidents[!is.na(latitude), .N], round=FALSE))
  else 
    message("No geocode data was merged into DT.Accidents. Either DT.Accidents is a fresh subset, or something went wrong")

  cat("Approx ", DT.Accidents[is.na(match_type), formnumb(.N)] , "left to geocode   and    ",DT.Accidents[(match_type == "bad_request" & !is.na(match_type)), formnumb(.N)], " with bad_request\n")
  # DT.Accidents[is.na(latitude) & is.na(match_type), .N, keyby=list(County, Year, Above_Split = as.num.as.char(Municipality_Code) > MunSplit)]
  .dash <- "------"; 
  rbind(DT.Accidents[is.na(match_type), .N, keyby=list(County, Year)], data.table(County=c(.dash,"TOTAL"), Year=c(.dash,"TOTAL"), N=c(.dash,DT.Accidents[is.na(match_type), .N])) )[N != .dash, N := formnumb(as.numeric(N), round=TRUE)][]
}

if (FALSE) {

pats.202_206 <- c("202206", "202/206", "202 AND 206( HWY$)")
streetAddress <- gsub("202(/| AND )206", "202-206", streetAddress)
string_202_206 <- find_street_pattern("202", "206", all=TRUE, colsToInclude=c("location_of_crash", "County"))$location
string_202_206 <- gsub("202/206", "202-206", string_202_206)
string_202_206[!grepl("202\\-206", string_202_206)]
string_202_206[!grepl("^20(2|6)", string_202_206)] <- gsub("^\\d+\\s*", "", string_202_206[!grepl("^20(2|6)", string_202_206)])
string_202_206 <- unique(string_202_206)


}

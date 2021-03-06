geocode

geoCols <- {
c(
"County",
"location_of_crash",
"Cross_Street_Name",
"Intersection",
"Route",
"Route_Suffix",
"Is_Ramp",
"Ramp_To_or_From_Route_Name"
)}

for (col in geoCols) {
  cat(" ----------------- ", col, " -------------------- \n")
  print (DT.Accidents[!is.na(get(col)), geoCols, with=FALSE])
}

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




http://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?

streetAddress=9355%20Burton%20Way
city=Beverly%20Hills
state=ca
zip=90210
apikey=4dabd5979f144327b0c7f50bf95aed68
format=csv
census=true
censusYear=2000|2010
notStore=false
version=4.01

http://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?streetAddress=9355%20Burton%20Way&city=Beverly%20Hills&state=ca&zip=90210&apikey=4dabd5979f144327b0c7f50bf95aed68&format=json&census=true&censusYear=2000|2010&notStore=false&version=4.01

url_geo.base <- "http://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?"
streetAddress=geo.streetAddress
city=Municipality
state="NJ"
zip=NULL
apikey=getOption("Texas_AM_geo.apikey")
format="csv"
census="false"
notStore="false"
version="4.01"








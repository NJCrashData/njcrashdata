DT.Drivers      <-  copy(ll_DT.Drivers      [[Cnty]])
DT.Occupants    <-  copy(ll_DT.Occupants    [[Cnty]])
DT.Pedestrians  <-  copy(ll_DT.Pedestrians  [[Cnty]])
DT.Vehicles     <-  copy(ll_DT.Vehicles     [[Cnty]])
DT.Accidents    <-  copy(ll_DT.Accidents    [[Cnty]])

## -------------------- DT.Drivers ----------------------------------

## Add a field for Driver details missing
DT.Drivers

## -------------------- DT.Accidents ----------------------------------
DT.Accidents
DT.Accidents[, c("Year", "County_Code", "Municipality_Code", "Department_Case_Number") := 
                  list( as.numeric(trim(substr(join_id, 1, 4)))
                      , as.numeric(trim(substr(join_id, 5, 6)))
                      , as.numeric(trim(substr(join_id, 7, 8)))
                      ,           (trim(substr(join_id, 9, 31)))
                      )
            ]

## Bank the original NAs, as we want to be sure not to intoduce any new ones
tmp_NAs <- is.na(DT.Accidents)

## Set blanks to NA
for (col in names(DT.Accidents)) {
  if (is.character(DT.Accidents[[col]]))
    DT.Accidents[, (col) := trim(get(col))]  
  DT.Accidents[trim(get(col)) == "", (col) := NA]
}

BackUpOrRestore("DT.Accidents")

## Clean up dates
DT.Accidents[, date_of_crash := as.Date(date_of_crash, format="%m/%d/%Y")]
DT.Accidents[, day_of_crash  := weekdays(date_of_crash)]
## Confrim no errors:
stopifnot(DT.Accidents[, !is.na(date_of_crash)])
stopifnot(DT.Accidents[, !is.na(day_of_crash) & day_of_crash %in% weekdays(today() + 1:7)])

##  ## NOTES
##  Posted_Speed -- There appear to be some typos, though a very small ammount. 
##  ie, a few random 1, 2, 3, 4 etc -- which are likely the tens digit, with the ones digit missing (is it 0 or 5)
##      or maybe they mean just 5 mph and are a typo.  Also, what of the 5mph, should any of them be 50mph ? 
##      For now, just leave them alone.  It is not to significant. 
##      At some point we could use geolocation to clean
## SEE: 
##      DT.Accidents[, .N, by = Posted_Speed][order(as.character(Posted_Speed))]


## Identify columns that are factors not numbers
cols.factorNotNumber <- c("Police_Dept_Code", "Police_Station", "Crash_Type_Code", "Route", "Route_Suffix", "route_id_SRI", "Road_System", "Road_Character", "Road_Surface_Type", "Surface_Condition", "Light_Condition", "Environmental_Condition", "Road_Divided_By", "Temporary_Traffic_Control_Zone", "Unit_Of_Measurement",  "Is_Ramp", "Ramp_To_or_From_Route_Name", "Ramp_To_or_From_Route_Direction", "Posted_Speed_Cross_Street", "Posted_Speed", "Latitude", "Longitude", "cell_phone_in_use", "Other_Property_Damage", "badge_numb", "Year", "County_Code", "Municipality_Code", "Department_Case_Number")

## Columns that are deffinitely numbers
cols.numbers <- c("MilePost", "Distance_To_Cross_Street")

## Clean up badge number
DT.Accidents[, badge_numb := trim(gsub("\\#", "", badge_numb))]

## clean up numerics
for (col in cols.numbers)
  DT.Accidents[, (col) := as.numeric(trim(as.character(get(col))))]

DT.Accidents[!is.na(time_of_crash), table(nchar(time_of_crash))]
DT.Accidents[!is.na(date_of_crash), date_of_crash]
DT.Accidents[1:2, as.POSIXct(sprintf("%s %04i EST", date_of_crash, time_of_crash), format="%Y-%m-%d %H%k")]
DT.Accidents[1:2, list(date_of_crash, time_of_crash, sprintf("%s %04i EST", date_of_crash, time_of_crash), as.POSIXct(sprintf("%s %04i EST", date_of_crash, time_of_crash), format="%Y-%m-%d %k%M"))]

DT.Accidents[!is.na(time_of_crash), datetime_of_crash := as.POSIXct(sprintf("%s %04i", date_of_crash, time_of_crash), format="%Y-%m-%d %k%M")]
DT.Accidents[,  datetime_of_crash := as.POSIXct(sprintf("%s %04i", date_of_crash, time_of_crash), format="%Y-%m-%d %k%M")]
DT.Accidents[is.na(time_of_crash)]


format(now(), format="%Y-%m-%d %H%k")
showDateFormats()
.myf(format)
DT.Accidents[!is.na(time_of_crash), datetime_of_crash := as.POSIXct(sprintf("%s %04i", date_of_crash, time_of_crash), format="%Y-%m-%d %H%k")]
DT.Accidents[!is.na(time_of_crash), sprintf("%s %04i", date_of_crash, time_of_crash)]
DT.Accidents[, .N, keyby=substr(sprintf("%s %04i", date_of_crash, time_of_crash), 12, 13)]

DT.Accidents[, table((time_of_crash, 1, 2))]
DT.Accidents[(substr(time_of_crash, 1, 2)) == 95]


convertNumberCols_(DT.Accidents, verbose=TRUE)


(local OSX) R|NJTrafficAccidents> nwhich(canBeNumeric(DT.Accidents))
 [1] "time_of_crash"                   "Police_Dept_Code"                "Total_Killed"
 [4] "Total_Injured"                   "Pedestrians_Killed"              "Pedestrians_Injured"
 [7] "Crash_Type_Code"                 "Total_Vehicles_Involved"         "Route"
[10] "MilePost"                        "Road_System"                     "Road_Character"
[13] "Road_Surface_Type"               "Surface_Condition"               "Light_Condition"
[16] "Environmental_Condition"         "Road_Divided_By"                 "Temporary_Traffic_Control_Zone"
[19] "Distance_To_Cross_Street"        "Is_Ramp"                         "Ramp_To_or_From_Route_Name"
[22] "Ramp_To_or_From_Route_Direction" "Posted_Speed"                    "Posted_Speed_Cross_Street"
[25] "Latitude"                        "Longitude"                       "Year"
[28] "County Code"                     "Municipality Code"
(local OSX) R|NJTrafficAccidents>



## Confirm that no NAs where introduced
stopifnot(tmp_NAs == is.na(DT.Accidents))

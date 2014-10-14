## These are the colnames for rawdata from 2001-present 
##   as given according to 
##    http://www.state.nj.us/transportation/refdata/accident/


colNames.raw <- list (
## The first four columns in each are squashed into an id
## join_id =  "Year", "County Code", "Municipality Code", "Department Case Number",
  "Accidents"  = 
    c("join_id", "County", "Municipality", "date_of_crash", "day_of_crash", "time_of_crash", "Police Dept Code", "Police Department", "Police Station", "Total Killed", "Total Injured", "Pedestrians Killed", "Pedestrians Injured", "Severity", "Intersection", "Alcohol Involved", "HazMat Involved", "Crash Type Code", "Total Vehicles Involved", "location_of_crash", "location_direction", "Route", "Route Suffix", "route_id SRI", "MilePost", "Road System", "Road Character", "Road Surface Type", "Surface Condition", "Light Condition", "Environmental Condition", "Road Divided By", "Temporary Traffic Control Zone", "Distance To Cross Street", "Unit Of Measurement", "Directn From Cross Street", "Cross Street Name", "Is Ramp", "Ramp To/From Route Name", "Ramp To/From Route Direction", "Posted Speed", "Posted Speed Cross Street", "Latitude", "Longitude", "cell_phone_in_use", "Other Property Damage", "badge_numb")
,
  "Drivers"    = 
    c("join_id", "Vehicle Number", "Driver City", "Driver State", "Driver Zip Code", "Driver License State", "Driver DOB", "Driver Sex", "Alcohol Test Given", "Alcohol Test Type", "Alcohol Test Results", "Charge", "Summons", "Multi Charge Flag", "Driver Physical Status")
,
  "Vehicles"   = 
    c("join_id", "Vehicle Number", "Insurance Company Code", "Owner State", "Make of Vehicle", "Model of Vehicle", "Color of Vehicle", "Year of Vehicle", "License Plate State", "Vehicle Weight Rating", "Towed", "Removed By", "Initial Impact Location", "Principal Damage Location", "Traffic Controls Present", "Vehicle Type", "Vehicle Use", "Special Function Vehicles", "Cargo Body Type", "Contributing Circumstances 1", "Contributing Circumstances 2", "Direction of Travel", "Pre- Crash Action", "First Sequence of Events", "Second Sequence of Events", "Third Sequence of Events", "Fourth Sequence of Events", "Oversize/Overweight Permit", "HazMat Status", "HazMat Placard", "USDOT / Other Flag", "USDOT / OTHER Number", "Carrier Name", "Hit & Run Driver Flag")
)

colNames.raw <- lapply(colNames.raw, trim)
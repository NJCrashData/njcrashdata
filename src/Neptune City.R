## Monmouth County Accidents

DT.Monmouth <- ll_DT.Accidents[["Monmouth"]]

DT.Monmouth[grepl("Nept", Municipality, ignore.case=TRUE), table(Municipality)]


DT.Monmouth[grepl("Nept", Municipality, ignore.case=TRUE), table(substr(location_of_crash, 1, 5))]

DT.Monmouth[grepl("Nept", Municipality, ignore.case=TRUE) & grepl("(Sprin|5th|FIFTH|Nept)", location_of_crash, ignore.case=TRUE)][, .N, by=list(Municipality,location_of_crash, Cross_Street_Name)][order(Municipality,N, decreasing=TRUE)]


DT.Monmouth[grepl("SPRINGDALE AVE", location_of_crash)]
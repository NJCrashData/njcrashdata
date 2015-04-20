# 04 Mapping.r

library(ggmap)

DT.Acc_code[match_type != "Exact" & Municipality_Code == 35]

  DT.ret_ggmap <- {
    DT.Acc_code[unique(c(1, which(match_type != "Exact" & Municipality_Code == 35))) ,
    # DT.Accidents[, 
      geocode_ggmap(streetAddress = geo.streetAddress
                  , city = Municipality
                  , state = "NJ"
                  , zip = NULL
                  , internal_id=join_id
                  , folder = data.p("geocode_ggmap_results", County[[1]])
                  , colsToReturn = c("internal_id", colsBringing)
                  , iter_size = 201
                )]
  }
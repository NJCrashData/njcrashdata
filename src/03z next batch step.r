# 03z next batch step.r

source(src.p("03b Add Geodata from disk.r"))

addCredits()
.us()

try(
{

  ### Query the TAMU API
  closeAllConnections()
  gc()
  DT.ret <- {
    DT.Accidents[
      i = {if (exists("match_type", inherits=FALSE)) is.na(match_type) else TRUE}
    , j = geocode_tamu(streetAddress = geo.streetAddress
                  , city = Municipality
                  , state = "NJ"
                  , zip = NULL
                  , internal_id=join_id
                  , apikey = getOption("tamu_geo.apikey")
                  , format = "csv"
                  , check_states = FALSE
                  , folder = data.p("geocode_tamu_results", County[[1]])
                  , colsToReturn = c("internal_id", colsToBring)
                  , iter_size = 150
                  , timeout = 2
                )]
  }
})

source(src.p("03b Add Geodata from disk.r"))

getCounties <- function(NJ.included=FALSE) {
  Counties <- 
    c(
    #           KEY  = VALUE
         "Atlantic"  = "Atlantic"
    ,      "Bergen"  = "Bergen"
    ,  "Burlington"  = "Burlington"
    ,      "Camden"  = "Camden"
    ,    "Cape May"  = "Cape May"
    ,  "Cumberland"  = "Cumberland"
    ,       "Essex"  = "Essex"
    ,  "Gloucester"  = "Gloucester"
    ,      "Hudson"  = "Hudson"
    ,   "Hunterdon"  = "Hunterdon"
    ,      "Mercer"  = "Mercer"
    ,   "Middlesex"  = "Middlesex"
    ,    "Monmouth"  = "Monmouth"
    ,      "Morris"  = "Morris"
    ,       "Ocean"  = "Ocean"
    ,     "Passaic"  = "Passaic"
    ,       "Salem"  = "Salem"
    ,    "Somerset"  = "Somerset"
    ,      "Sussex"  = "Sussex"
    ,       "Union"  = "Union"
    ,      "Warren"  = "Warren"
    ,   "NewJersey"  = "NewJersey"
  )

  if (!isTRUE(NJ.included))
    return(Counties[names(Counties) != "NewJersey"])
  return(Counties)
}
# parse_join_id.r

parse_join_id <- function(join_id) {
  colNames.join_id <- c("Year", "County Code", "Municipality Code", "Department Case Number")

  setNames(nm=colNames.join_id, obj = 
  list( as.numeric(trim(substr(join_id, 1, 4)))
      , as.numeric(trim(substr(join_id, 5, 6)))
      , as.numeric(trim(substr(join_id, 7, 8)))
      ,           (trim(substr(join_id, 9, 31)))
      )
  )
}

addParsedjoin_id_ <- function(DT) {
  colNames.join_id <- c("Year", "County_Code", "Municipality_Code", "Department_Case_Number")
  key.bak <- key(DT)
  DT[, join_id := cleanChars(join_id, replacement="_")]
  DT[, (colNames.join_id) := parse_join_id(join_id)]
  # DT[, (colNames.join_id) := 
  #         list( as.numeric(trim(substr(join_id, 1, 4)))
  #               , as.numeric(trim(substr(join_id, 5, 6)))
  #               , as.numeric(trim(substr(join_id, 7, 8)))
  #               ,           (trim(substr(join_id, 9, 31)))
  #             )
  # ]
  setkeyIfNot(DT, key.bak, verbose=FALSE)

  return(invisible(DT))
}
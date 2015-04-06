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
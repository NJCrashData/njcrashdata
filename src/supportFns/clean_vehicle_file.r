clean_vehicle_file <- function(f) {
  raw <- readLines(f)

  if (basename(f) %in% c("Ocean2013Vehicles.txt", "NewJersey2013Vehicles.txt"))
    raw <- gsub(  "2013150713-59742               , 3,    ,PA,MITSUBISHI                    ,TRUCK               ,RD ,2006,PA, ,T,2 ,  ,  ,04,20,02,01,05,25,  ,02,06,26,  ,  ,  ,  , ,          , ,25,995    ,                                                  ,"
                , "2013150713-59742               , 3,    ,PA,MITSUBISHI                    ,TRUCK               ,RD ,2006,PA, ,T,2 ,  ,  ,04,20,02,01,05,25,  ,02,06,26,  ,  ,  ,  , ,          , ,25995     ,                                                  ,"
                , raw)

  raw <- gsub(",4 DR,", ",4 DR."
            , gsub("A(A|C)CORD, ACCORD CROS\\?", "ACCORD other"
            , gsub("ACCOR,ACCORD CROSST\\?", "ACCORD other"
              , gsub("FORENZA \\(S, (B|E)X, (L|D) ", "FORENZA (S EX or L)"
                , gsub("NEON, 4DR      ", "NEON (4DR)     "
                , gsub(",ACC,4DR             ,", ",ACCORD (4DR)        ,"
                , gsub(",4DR, A4             ,", ",A4 (4DR)            ,"
                , gsub(",IMP, 4DR            ,", ",IMPALA (4DR)        ,"
                , gsub(",CIV, 4DR            ,", ",CIVIC (4DR)         ,"
                  , gsub("CIVIC, CIVIC DEL SOL", "CIVIC (del sol)"
                    , gsub("CIVIC, CI ", "CIVIC (CI)"
                    , gsub(",CIVIC,              ,", ",CIVIC               ,"
                    , gsub(",CIVIC,...           ,", ",CIVIC other         ,"
                    , gsub(",2DR, CAM            ,", ",CAMRY (2DR)         ,"
                      , gsub("DUR,.               ", "DUR.               "
                          , gsub("X(E|R), ?X(E|R)", "XE or XR"
                            , gsub(",330, 4DR   ", ",330 (4DR)  "
                            , gsub(",300, 4DR   ", ",300 (4DR)  "
                              , gsub(",TWN,CIV     ", ",TWN (CIV)   "
                                , gsub(",4300,       ", ",4300       "
                                , gsub(",WAG,                ", ",WAG                 "
                                , gsub(",,M3H                ", ",M3H                 "
                                , gsub("EL DORADO MFG.,               ,", "EL DORADO MFG.                ,"
                                , gsub(",WGN,                ,", ",WGN                 ,"
                                , gsub(",HINSON MFG. CO.,              ,INC.                ,", ",HINSON MFG. CO. INC.,                ,"
                                , gsub("A;TO,A", "ALTIMA"
                                , gsub(",NEON, 4DR           ,", ",NEON (4DR)           ,"
                                  , raw
                                  ))))))))))))))))))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
  writeLines(text=raw, con=f)
  return(TRUE)
}

## This only has to happen once
if (FALSE) 
{
  cat("Cleaning Vehicle files .... ")
  for (cnty in sunique(files.info$county)) {
    cat(cnty, " ... ")
    lapply(files.info[county == cnty & report_type == "Vehicles", file], clean_vehicle_file)
  }
  cat(" [DONE] ")
}



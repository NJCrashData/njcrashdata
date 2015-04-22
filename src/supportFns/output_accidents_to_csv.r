## OUTPUT DT to file to batch process. But that still has the same limit
output_accidents_to_csv <- function(
      DT = get("DT.Accidents", envir=globalenv())
    , addlcols = c("datetime_of_crash", "day_of_crash", "date_of_crash", "Alcohol_Involved")

    , append = FALSE
    , fileEncoding = ""

    ## File info
    , folder = out.p("csvs_of_DTs")
    , file_name = paste0("Accidents", file_time_stamp, ".", "csv")
    , file_time_stamp = format(Sys.time(), "%Y%m%d_%H%M%S")
    , file_sep = ","

    , split = TRUE
    , verbose=TRUE

){
    if (!isTRUE(split)) 
        stop ("Not implemented for split=FALSE    -- you can comment out the for-loops (manually setting value for CC and YY)")
    # file_out  <- file.path(folder, file_name)

    if (isTRUE(append)) {
        stop ("Not implemented for append=TRUE")
    }

    Counties <- DT[, as.character(unique(County))]
    Years <- DT[, unique(Year)]


    file_outs <- c()

    for (CC in Counties) {
      for (YY in Years) {

        file_out <- file.path(folder, CC, YY, sprintf("%s_%s_%s", CC, YY, file_name))
        file_outs <- c(file_outs, file_out)

        if (isTRUE(append) && file.exists(file_out)) {
          ## TODO:   read first line and check that headers is colnames of DT
        }

        if (!file.exists(dirname(file_out)))
            dir.create(dirname(file_out), recursive=TRUE)

        ## Create string for j expression
        names(addlcols) <- colNamesFromVector(addlcols)
        location <- 'paste(geo.streetAddress, Municipality, "NJ", sep=",")'
        jCols <- c(unique_id='join_id', location=location, streetAddress='geo.streetAddress', city='Municipality', state='"NJ"', addlcols)
        jCols <- c(unique_id='join_id', location=location, addlcols)
        j_as_string <- sprintf("list(%s)", paste(sprintf("%s=%s", names(jCols), jCols), collapse=", "))
        # cat("\n\n", j_as_string, "\n\n")

        ## Write to file
        write.table(x=
                DT[
                  i = (County == CC & Year == YY)
                , j = eval(parse(text=j_as_string))
                ]
            , file=file_out, append = append, sep = file_sep, row.names=FALSE, col.names=TRUE, fileEncoding=fileEncoding)

        ## Verbose to user
        verboseMsg(verbose, sprintf("DT written to  '%s'  total size is %s\n", file_out, formatBytes(file.info(file_out)$size)))

      }
    }

    return(invisible(file_outs))
}

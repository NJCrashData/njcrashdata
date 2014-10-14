file.in <- "~rsaporta/Downloads/ACCIDENT2007-FullDataSet.csv"


    library(data.table)

    file.in <- "path/to/your/file.csv"
    DT.accidents <- fread(file.in)

    ## Have a look at the different DRUNK_DR values
    DT.accidents[, table(DRUNK_DR)]
    ## Nine?? Really?  

    DT.accidents[DRUNK_DR == 9]


    ## Anyway, to sum up by state and drunk drivers, assuming one row of data is one accident, you can simply use: 

    DT.accidents[, .N, by=list(STATE, DRUNK_DR)]


    ## If you want to ignore cases with zero drunk drivers, filter those out
    DT.drunks <- DT.accidents[DRUNK_DR > 0, .N, by=list(STATE, DRUNK_DR)]

    ## You can reshape it too, if you'd like

    library(reshape2)
    DT.drunks <- as.data.table(dcast(DT.drunks, STATE ~ DRUNK_DR, value="N"))




------

### Adding in state names

    State Names, accorindg to 
    ftp://ftp.nhtsa.dot.gov/FARS/FARS-DOC/USERGUIDE-2007.pdf


    ## start with the built in variable 'state.name' (no "s")
    state_names <- state.name[1:50]
    ## Add in territories
    state_names <- sort(c(state_names, "District of Columbia", "Puerto Rico", "Virgin Islands"))
    ## Create index numbers that match what is shown in the file
    state_number <- setdiff(1:56, c(3, 7, 14))
    ## Create a data.table for joining
    DT.states <- data.table(state_number=state_number, state_names=state_names)

    ## Join in the info
    setkey(DT.states, "state_number")
    setkey(DT.accidents, "STATE")
    DT.accidents[DT.states, STATE_NAMES := state_names]

    ## Now you can reshape, but include the names
    DT.drunks <- DT.accidents[DRUNK_DR > 0, .N, by=list(STATE, STATE_NAMES, DRUNK_DR)]

    ## You can reshape it too, if you'd like
    DT.drunks <- as.data.table(dcast(DT.drunks, STATE + STATE_NAMES ~ DRUNK_DR, value="N"))


## Now... as for that nine-drunk driver accident. 
DT.accidents[DRUNK_DR == 9]

Googling:  "Montana May 19 2007 Traffic Fatality"
The first result leads to  http://www.city-data.com/accidents/acc-Browning-Montana.html

Which has this piece of information

>  Browning fatal car crashes and road traffic accidents (wrecks) list for 2007:
>  May 19, 2007 05:55 PM, Us-2, Sr-464, Lat: 48.555692, Lon: -113.010247, Vehicles: 1, Fatalities: 1, Drunken drivers: Inconsistent data







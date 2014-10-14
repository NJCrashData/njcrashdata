##  2013/Monmouth2013Accidents
##  
##  base="/transportation/refdata/accident/2013/"
##  
##  dir=as.character(year);
##  
##  county=document.form.county.value;
##  
##  type=document.form.type.value;
##  
##  
##  var href = base + dir + "/" + county + dir + type + ".zip";
##  
##  

setProject(proj="NJTrafficAccidents", create=FALSE, subl=FALSE, load=FALSE, ingestIn_gitData=TRUE)

lib(RCurl)
install.packages("Rcompression")

year <- c(2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 2003, 2002, 2001)

county <- c("NewJersey", "Atlantic", "Bergen", "Burlington", "Camden", "CapeMay", "Cumberland", "Essex", "Gloucester", "Hudson", "Hunterdon", "Mercer", "Middlesex", "Monmouth", "Morris", "Ocean", "Passaic", "Salem", "Somerset", "Sussex", "Union", "Warren")

type <- c("Accidents", "Drivers", "Vehicles", "Occupants", "Pedestrians")

base <- "http://www.state.nj.us/transportation/refdata/accident"


DT.toDownload <- CJ(year=year, county=county, type=type)
DT.toDownload[, url := sprintf("%s/%s/%s%s%s.zip", base, year, county, year, type)]
DT.toDownload[, file.out.zip := ingest.p(sprintf("%s%s%s.zip", county, year, type))]
DT.toDownload[, file.out.txt := ingest.p(sprintf("%s%s%s.txt", county, year, type))]


.o(ingest.p())


DT.toDownload[

DT.toDownload[!file.exists(file.out.txt)][1:5,
                  mapply(downloadBinFile, url=url, file=file.out.zip)
                  ]

                  browser()
                  unzip (zip=file.out.zip, exdir=paste0(dir.out, "/"))
                  }, by=list(dir.out = dirname(file.out.txt))  ]

DT.toDownload[!file.exists(file.out.txt)
              , list(success={
                 message("downloading :  ", url, "")
                 ret <- downloadBinFile(url, file=file.out.zip)
                 unzip (zip=file.out.zip, exdir=dirname(file.out.txt))
                 if (file.exists(file.out.txt))
                   unlink(file.out.zip)
                 ret
                 })
              , by=file.out.zip]

# DT.toDownload[file.exists(file.out.txt) & file.exists(file.out.zip), 
#                     unlink(file.out.zip) ]



urls <- DT.toDownload[, sprintf("%s/%s/%s%s%s.zip", base, year, county, year, type)]



for (u in urls) {
    
}

unzip

downloadBinFile=function(url, file){
## Courtesy of @antonio on stackoverflow
##  http://stackoverflow.com/a/15543569/1492421
    library('RCurl')
    f = CFILE(file, mode="wb")
    a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
    close(f)
    browser()
    return(a)
}

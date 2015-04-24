#Stat133 Project 2

################# Global variables  #################


# can be downloaded from https://d1b10bmlvqabco.cloudfront.net/attach/i53463e2voe2yj/gxr526imlnz7np/i8vhafgocleb/offline.final.trace.txt
# replace the variable's value with the absolute path to the offline.final.trace.txt file
OFFLINE_FILE_NAME = "offline.final.trace.txt"

# can be downloaded from https://d1b10bmlvqabco.cloudfront.net/attach/i53463e2voe2yj/gxr526imlnz7np/i8vhdwz3wems/online.final.trace.txt
# replace the variable's value with the absolute path to the online.final.trace.txt file
ONLINE_FILE_NAME = "online.final.trace.txt"

################# Part 1  #################

# txt is a character vector containing all the files' lines
txt = readLines(con = OFFLINE_FILE_NAME)

# from the spec: "drop any lines that do not have information in them (e.g., the first three lines)
# subset by logical (boolean) vector:
#    drop all lines that start with a '#' sign followed by a space because these are comments
#    example of grepl:
#        > grepl("^# ", c("abc", "# timestamp=2006-03-09 21:30:21", "# minReadings=110", "aa"), perl=TRUE)
#        [1] FALSE  TRUE  TRUE  FALSE
#    source: http://www.regular-expressions.info/rlanguage.html
txt = txt[!grepl("^# ", txt)]

# x is a string that corresponds to one line from the data file
# returns  a character matrix with 10 columns
processLine = function(x) {
  
  time = gsub(".*t=(.*?);.*", "\\1", x)
  
  scanMac = gsub(".*id=(.*?);.*", "\\1", x)
  
  pos = gsub(".*pos=(.*?);.*", "\\1", x)
  posX = gsub("^(.*?),.*", "\\1", pos)
  posY = gsub("^.*?,(.*?),.*", "\\1", pos)
  posZ = gsub("^.*?,.*?,(.*)", "\\1", pos)
  
  orientation = gsub(".*degree=(.*?);.*", "\\1", x)
  
  macsStr = gsub(".*degree=.*?;(.)", "\\1", x)
  
  macs = strsplit(macsStr, ";")[[1]]
  
  # example from http://stackoverflow.com/questions/20730537/add-new-row-to-matrix-one-by-one
  return(t(sapply(macs,
                  function(wholeMac) {
                    mac = gsub("(.*?)=.*", "\\1", wholeMac)
                    signal = gsub(".*?=(.*?),.*", "\\1", wholeMac)
                    channel = gsub(".*?=.*?,(.*?),.*", "\\1", wholeMac)
                    type = gsub(".*?=.*?,.*?,(.*)", "\\1", wholeMac)
                    c(time, scanMac,
                      posX, posY, posZ,
                      orientation,
                      mac, signal, channel, type)
                  })))
}

tmp = lapply(txt, processLine)
offline = as.data.frame(do.call("rbind", tmp))
names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
                   "orientation", "mac", "signal", "channel", "type")


################# Part 2  #################

cleanData = function(data, keepMacs = c("mm:mm:mm:ss:ss:ss", etc)) {
  # data is the output from the above processing, e.g., offline
  # keepMacs is a character vector of the 6 MAC addresses
  return(dataframe)
}

offline2 = cleanData(offline)

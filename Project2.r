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
  
  # The spec says, "Convert data that should be numeric."
  # see http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
  data$time = as.numeric(levels(data$time))[data$time]
  data$posX = as.numeric(levels(data$posX))[data$posX]
  data$posY = as.numeric(levels(data$posY))[data$posY]
  data$posZ = as.numeric(levels(data$posZ))[data$posZ]
  data$orientation = as.numeric(levels(data$orientation))[data$orientation]
  data$signal = as.numeric(levels(data$signal))[data$signal]
  
  # Drop any irrelevant variables, i.e., variables that have the same values for all records or
  # where the same information is captured in another variable.
  # 1. Drop scanMac because length(offline$scanMac[as.character.factor(offline$scanMac) != "00:02:2D:21:0F:33"]) returns 0.
  # 2. Drop "posZ" because nrow(offline[offline$posZ != 0.0, ]) returns 0.
  # 3. Drop "posX" and "posY" because they can be computed from "orientation".
  # drop columns method from http://stackoverflow.com/questions/4605206/drop-columns-r-data-frame/21719511#21719511
  data[ , c("scanMac", "posX", "posY", "posZ")] = list(NULL)
  
  # Round the values for orientation to the nearest 45 degrees, but keep the original values too. 
  data$roundedOrientation = round(data$orientation / 45.0, digits = 0) * 45
  
  # Drop all records that correspond to adhoc devices, and not the access points.
  # There will still be about a dozen MAC addresses in the data.
  # Use exploratory data analysis to ﬁgure out which are the 6 MAC addresses on the ﬂoor.
  # According to the data documentation, these 6 include 5 Linksys/Cisco and one Lancom L-54g routers.
  # You can look up the MAC addresses at http://coffer.com/mac find/ to ﬁnd the vendors.
  # This may prove helpful in narrowing down the MAC addresses to keep.
  # Prefix 0014BF is from Cisco-Linksys, LLC (example: 00:14:bf:b1:97:8a).
  # Prefix 00A057 is from Lancom.
  
  
  return(dataframe)
}








offline2 = cleanData(offline)

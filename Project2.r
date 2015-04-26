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
  
  
  # gsub can be used to capture groups.
  # See http://stackoverflow.com/questions/952275/regex-group-capture-in-r-with-multiple-capture-groups/953496#953496.

  # gsub takes as arguments:
  #   1. the regular expression
  #   2. the replacement to be returned. This contains the capture group(s) to be retained.
  #   3. the string(s) to run the regular expression over

  # An example of how gsub works:
  # > gsub("Test value: (.+)", "\\1", "Test value: 1234")
  # [1] "1234"
  # "." captures any character.
  # "*" captures zero or more of the previous character(s).
  # "+" captures one or more of the previous character(s).
  # ".*" captures zero or more of any previous character.

  # Sometimes, ".*" is insufficient to capture a group you want because it is too greedy.
  # An example of a greedy regular expression (suppose we want to get the first number 12):
  # gsub("Test value: (.*),.*", "\\1", "Test value: 12, 34, 56")
  # [1] "12, 34"

  # To make the regular expression "lazy" (or not "greedy"), add a question mark (?):
  # gsub("Test value: (.*?),.*", "\\1", "Test value: 12, 34, 56")
  # [1] "12"

  # A sample line (with line breaks and spaces added for readability):
  # "t=1139643118358;id=00:02:2D:21:0F:33;pos=0.0,0.0,0.0;degree=0.0;
  # 00:14:bf:b1:97:8a=-38,2437000000,3;00:14:bf:b1:97:90=-56,2427000000,3;
  # 00:0f:a3:39:e1:c0=-53,2462000000,3;00:14:bf:b1:97:8d=-65,2442000000,3;
  # 00:14:bf:b1:97:81=-65,2422000000,3;00:14:bf:3b:c7:c6=-66,2432000000,3;
  # 00:0f:a3:39:dd:cd=-75,2412000000,3;00:0f:a3:39:e0:4b=-78,2462000000,3;
  # 00:0f:a3:39:e2:10=-87,2437000000,3;02:64:fb:68:52:e6=-88,2447000000,1;
  # 02:00:42:55:31:00=-84,2457000000,1"
  
  # 1. ignore everything before "t=" appears in the string
  # 2. capture the text between "t=" and ";"
  #    Use the "+" instead of "*" because we must capture 1 or more characters.
  #    Use the "?" so it is a "lazy" regular expression,
  #    meaning that it reads up to the first matching ";"
  #    instead of reading to the last matching ";"
  # 3. ignore everything after the ";"
  time = gsub(".*t=(.+?);.*", "\\1", x)
  
  # 1. ignore everything before "id=" appears in the string
  # 2. capture the text between "id=" and ";"
  #    Use the "+" instead of "*" because we must capture 1 or more characters.
  #    Use the "?" so it is a "lazy" regular expression,
  #    meaning that it reads up to the first matching ";"
  #    instead of reading to the last matching ";"
  # 3. ignore everything after the ";"
  scanMac = gsub(".*id=(.+?);.*", "\\1", x)
  
  # 1. ignore everything before "pos=" appears in the string
  # 2. capture the text between "pos=" and ";"
  #    Use the "+" instead of "*" because we must capture 1 or more characters.
  #    Use the "?" so it is a "lazy" regular expression,
  #    meaning that it reads up to the first matching ";"
  #    instead of reading to the last matching ";"
  # 3. ignore everything after the ";"
  pos = gsub(".*pos=(.+?);.*", "\\1", x)
  
  # pos returns something like: "pos=0.0,1.0,2.0"
  # posX is the first number: 0.0
  # posY is the second number: 1.0
  # posZ is the third number: 2.0
  posX = gsub("^(.+?),.*", "\\1", pos)
  posY = gsub("^.+?,(.+?),.*", "\\1", pos)
  posZ = gsub("^.+?,.+?,(.+)", "\\1", pos)
  
  
  # 1. ignore everything before "degree=" appears in the string
  # 2. capture the text between "degree=" and ";"
  #    Use the "+" instead of "*" because we must capture 1 or more characters.
  #    Use the "?" so it is a "lazy" regular expression,
  #    meaning that it reads up to the first matching ";"
  #    instead of reading to the last matching ";"
  # 3. ignore everything after the ";"
  orientation = gsub(".*degree=(.+?);.*", "\\1", x)
  
  # 1. ignore everything before "degree=[DEGREE_VALUE];" appears in the string
  # 2. capture the text between "degree=[DEGREE_VALUE];" and the end of the string
  #    Use the "+" instead of "*" because we must capture 1 or more characters.
  macsStr = gsub(".*degree=.*?;(.+)", "\\1", x)
  
  # macStr returns something like ~11 MAC addresses, delimited by semicolons (";"):
  # 00:14:bf:b1:97:8a=-38,2437000000,3;00:14:bf:b1:97:90=-56,2427000000,3;
  # 00:0f:a3:39:e1:c0=-53,2462000000,3;00:14:bf:b1:97:8d=-65,2442000000,3;
  # 00:14:bf:b1:97:81=-65,2422000000,3;00:14:bf:3b:c7:c6=-66,2432000000,3;
  # 00:0f:a3:39:dd:cd=-75,2412000000,3;00:0f:a3:39:e0:4b=-78,2462000000,3;
  # 00:0f:a3:39:e2:10=-87,2437000000,3;02:64:fb:68:52:e6=-88,2447000000,1;
  # 02:00:42:55:31:00=-84,2457000000,1"
  macs = strsplit(macsStr, ";")[[1]]
  
  # example from http://stackoverflow.com/questions/20730537/add-new-row-to-matrix-one-by-one
  return(t(sapply(macs,
                  function(wholeMac) {
                    mac = gsub("(.+?)=.*", "\\1", wholeMac)
                    signal = gsub(".*?=(.+?),.*", "\\1", wholeMac)
                    channel = gsub(".*?=.*?,(.+?),.*", "\\1", wholeMac)
                    type = gsub(".*?=.*?,.*?,(.+)", "\\1", wholeMac)
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

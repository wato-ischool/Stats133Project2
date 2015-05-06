#Stat133 Project 2

################# Global variables  #################

# can be downloaded from https://d1b10bmlvqabco.cloudfront.net/attach/i53463e2voe2yj/gxr526imlnz7np/i8vhafgocleb/offline.final.trace.txt
# replace the variable's value with the absolute path to the offline.final.trace.txt file
OFFLINE_FILE_NAME = "offline.final.trace.txt"

# can be downloaded from https://d1b10bmlvqabco.cloudfront.net/attach/i53463e2voe2yj/gxr526imlnz7np/i8vhdwz3wems/online.final.trace.txt
# replace the variable's value with the absolute path to the online.final.trace.txt file
ONLINE_FILE_NAME = "online.final.trace.txt"

################# Part 1 Task 1  #################

# txt is a character vector containing all the files' lines
txt = readLines(con = OFFLINE_FILE_NAME)
txt2 = readLines(con = ONLINE_FILE_NAME)

# from the spec: "drop any lines that do not have information in them (e.g., the first three lines)
# subset by logical (boolean) vector:
#    drop all lines that start with a '#' sign followed by a space because these are comments
#    example of grepl:
#        > grepl("^# ", c("abc", "# timestamp=2006-03-09 21:30:21", "# minReadings=110", "aa"), perl=TRUE)
#        [1] FALSE  TRUE  TRUE  FALSE
#    source: http://www.regular-expressions.info/rlanguage.html
txt = txt[!grepl("^# ", txt)]
txt2 = txt2[!grepl("^# ", txt2)]

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
  
  # pos returns something like: "0.0,1.0,2.0"
  # posX is the first number: 0.0
  # posY is the second number: 1.0
  # posZ is the third number: 2.0
  posX = gsub("^(.+?),.*", "\\1", pos)
  posY = gsub("^.+?,(.+?),.*", "\\1", pos)
  posZ = gsub("^.+?,.+?,(.+)", "\\1", pos)
  
  # 1. ignore everything before "degree=" appears in the string
  # 2. capture the text after "degree=" that is part of a number
  #    [[:digit:]] is a character class that represents numbers between 0 and 9
  #    the number format is always a) one or more digits, b) a period, c) one or more digits
  orientation = gsub(".*degree=([[:digit:]]+\\.[[:digit:]]*).*", "\\1", x)
  
  # 1. ignore everything before "degree=[DEGREE_VALUE];" appears in the string
  # 2. capture the text between "degree=[DEGREE_VALUE];" and the end of the string
  macsStr = gsub(".*degree=[[:digit:]]+\\.[[:digit:]]*(.+)", "\\1", x)
  
  # get rid of any starting semicolon
  macsStr = gsub("^;", "", macsStr)
  
  # Example mac:
  # 00:14:bf:b1:97:8a=-38,2437000000,3;00:14:bf:b1:97:90=-56,2427000000,3;
  # regular expression test string to determine mac is valid
  macTestRE = paste("^", "(?:",
                    "[[:alnum:]][[:alnum:]]:[[:alnum:]][[:alnum:]]:",
                    "[[:alnum:]][[:alnum:]]:[[:alnum:]][[:alnum:]]:",
                    "[[:alnum:]][[:alnum:]]:[[:alnum:]][[:alnum:]]",
                    "=", "-?[[:digit:]]+,", "[[:digit:]]+,",
                    "[[:digit:]]+", ";",
                    ")*",
                    "[[:alnum:]][[:alnum:]]:[[:alnum:]][[:alnum:]]:",
                    "[[:alnum:]][[:alnum:]]:[[:alnum:]][[:alnum:]]:",
                    "[[:alnum:]][[:alnum:]]:[[:alnum:]][[:alnum:]]",
                    "=", "-?[[:digit:]]+,", "[[:digit:]]+,",
                    "[[:digit:]]+",
                    "$",
                    sep = "")
  
  if (!grepl(macTestRE, macsStr)) {
    macsStr = NA
  }
  
  # macStr returns something like ~11 MAC addresses, delimited by semicolons (";"):
  # 00:14:bf:b1:97:8a=-38,2437000000,3;00:14:bf:b1:97:90=-56,2427000000,3;
  # 00:0f:a3:39:e1:c0=-53,2462000000,3;00:14:bf:b1:97:8d=-65,2442000000,3;
  # 00:14:bf:b1:97:81=-65,2422000000,3;00:14:bf:3b:c7:c6=-66,2432000000,3;
  # 00:0f:a3:39:dd:cd=-75,2412000000,3;00:0f:a3:39:e0:4b=-78,2462000000,3;
  # 00:0f:a3:39:e2:10=-87,2437000000,3;02:64:fb:68:52:e6=-88,2447000000,1;
  # 02:00:42:55:31:00=-84,2457000000,1"
  
  if (!is.na(macsStr)) {
    macs = strsplit(macsStr, ";")[[1]]
  } else {
    macs = NA
  }
  
  # example from http://stackoverflow.com/questions/20730537/add-new-row-to-matrix-one-by-one
  return(t(sapply(macs,
                  function(wholeMac) {
                    # make sure this is not a malformed MAC
                    if (!is.na(wholeMac)) {
                      mac = gsub("(.+?)=.*", "\\1", wholeMac)
                      signal = gsub(".*?=(.+?),.*", "\\1", wholeMac)
                      channel = gsub(".*?=.*?,(.+?),.*", "\\1", wholeMac)
                      type = gsub(".*?=.*?,.*?,(.+)", "\\1", wholeMac)
                    } else {
                      mac = NA
                      signal = NA
                      channel = NA
                      type = NA
                    }

                    c(time, scanMac,
                      posX, posY, posZ,
                      orientation,
                      mac, signal, channel, type)
                  })))
}

################# Part 1, Task 2  #################

cleanData = function(data, keepMacs = c("00:14:bf:b1:97:8a", "00:14:bf:b1:97:90",
                                        "00:14:bf:b1:97:8d", "00:14:bf:b1:97:81",
                                        "00:14:bf:3b:c7:c6", "00:0f:a3:39:e1:c0")) {
  
  # get rid of the extraneous row names at the beginning of the data frame
  row.names(data) = NULL
  
  # remove malformed data
  data = data[!is.na(data$mac), ]
  
  # remove adhoc devices
  data = data[data$type == "3", ]
  
  # data is the output from the above processing, e.g., offline
  # keepMacs is a character vector of the MAC addresses that correspond to real vendors
  
  # Drop all records that correspond to adhoc devices, and not the access points.
  # There will still be about a dozen MAC addresses in the data.
  # Use exploratory data analysis to ﬁgure out which are the 6 MAC addresses on the ﬂoor.
  # According to the data documentation, these 6 include 5 Linksys/Cisco and one Lancom L-54g routers.
  # You can look up the MAC addresses at http://coffer.com/mac_find/ to ﬁnd the vendors.
  # This may prove helpful in narrowing down the MAC addresses to keep.
  
  # unique(offline$mac)
  # [1] <NA>              00:14:bf:b1:97:90 00:14:bf:b1:97:81 00:14:bf:3b:c7:c6 00:0f:a3:39:e1:c0 00:0f:a3:39:dd:cd 02:00:42:55:31:00
  # [8] 00:14:bf:b1:97:8a 02:64:fb:68:52:e6 00:14:bf:b1:97:8d 00:0f:a3:39:e2:10 00:0f:a3:39:e0:4b 00:04:0e:5c:23:fc 00:30:bd:f8:7f:c5
  # [15] 00:e0:63:82:8b:a9 02:37:fd:3b:54:b5 02:2e:58:22:f1:ac 02:42:1c:4e:b5:c0 02:0a:3d:06:94:88 02:5c:e0:50:49:de 02:4f:99:43:30:cd
  # [22] 02:b7:00:bb:a9:35
  
  # Find the vendors by entering the prefixes into http://coffer.com/mac_find/.
  # Keep all MACs that are from the vendors Cisco or Lancom.
  # Note that from
  # http://www.lancom-systems.de/produkte/wireless-lan/indoor-wlan-access-points/lancom-l-54g-wireless/l-54g-wireless-ueberblick/
  # we know that AVM GmbH is from Lancom 54G because it says at the bottom "[Copyright symbol] 2015 LANCOM Systems GmbH".
  # 1)  <NA> is invalid.
  # 2)  00:14:bf:b1:97:90 has the prefix 00:14:bf and is from Cisco-Linksys, LLC.
  # 3)  00:14:bf:b1:97:81 has the prefix 00:14:bf and is from Cisco-Linksys, LLC.
  # 4)  00:14:bf:3b:c7:c6 has the prefix 00:14:bf and is from Cisco-Linksys, LLC.
  # 5)  00:0f:a3:39:e1:c0 has the prefix 00:0f:a3 and is from Alpha Networks Inc.
  # 6)  00:0f:a3:39:dd:cd has the prefix 00:0f:a3 and is from Alpha Networks Inc.
  # 7)  02:00:42:55:31:00 has the prefix 02:00:42 and is from nowhere.
  # 8)  00:14:bf:b1:97:8a has the prefix 00:14:bf and is from Cisco-Linksys, LLC.
  # 9)  02:64:fb:68:52:e6 has the prefix 02:64:fb and is from nowhere.
  # 10) 00:14:bf:b1:97:8d has the prefix 00:14:bf and is from Cisco-Linksys, LLC.
  # 11) 00:0f:a3:39:e2:10 has the prefix 00:0f:a3 and is from Alpha Networks Inc.
  # 12) 00:0f:a3:39:e0:4b has the prefix 00:0f:a3 and is from Alpha Networks Inc.
  # 13) 00:04:0e:5c:23:fc has the prefix 00:04:0e and is from AVM GmbH (which is from Lancom).
  # 14) 00:30:bd:f8:7f:c5 has the prefix 00:30:bd and is from BELKIN COMPONENTS.
  # 15) 00:e0:63:82:8b:a9 has the prefix 00:e0:63 and is from cabletron - yago systems, inc.
  # 17) 02:37:fd:3b:54:b5 has the prefix 02:37:fd and is from nowhere.
  # 18) 02:2e:58:22:f1:ac has the prefix 02:2e:58 and is from nowhere.
  # 19) 02:42:1c:4e:b5:c0 has the prefix 02:42:1c and is from nowhere.
  # 20) 02:0a:3d:06:94:88 has the prefix 02:0a:3d and is from nowhere.
  # 21) 02:5c:e0:50:49:de has the prefix 02:5c:e0 and is from nowhere.
  # 22) 02:4f:99:43:30:cd has the prefix 02:4f:99 and is from nowhere.
  # 23) 02:b7:00:bb:a9:35 has the prefix 02:b7:00 and is from nowhere.
  
  # table(offline[offline$type == "3", ]$mac)
  # 00:04:0e:5c:23:fc 00:0f:a3:39:dd:cd 00:0f:a3:39:e0:4b 00:0f:a3:39:e1:c0 00:0f:a3:39:e2:10 00:14:bf:3b:c7:c6 00:14:bf:b1:97:81 
  # 418            145619             43508            145862             19162            126529            120339 
  # 00:14:bf:b1:97:8a 00:14:bf:b1:97:8d 00:14:bf:b1:97:90 00:30:bd:f8:7f:c5 00:e0:63:82:8b:a9 02:00:42:55:31:00 02:0a:3d:06:94:88 
  # 132962            121325            122315               301               103                 0                 0 
  # 02:2e:58:22:f1:ac 02:37:fd:3b:54:b5 02:42:1c:4e:b5:c0 02:4f:99:43:30:cd 02:5c:e0:50:49:de 02:64:fb:68:52:e6 02:b7:00:bb:a9:35 
  # 0                 0                 0                 0                 0                 0                 0
  
  # Eliminate the MACs with very few readings compared to the rest of the data.
  # Eliminate 00:04:0e:5c:23:fc (418 readings), 00:0f:a3:39:e0:4b (43508 readings), 00:0f:a3:39:e2:10 (19162 readings),
  # 00:30:bd:f8:7f:c5 (301 readings), 00:e0:63:82:8b:a9 (103 readings), and everything with zeroes.
  
  # Keep 00:0f:a3:39:dd:cd, 00:0f:a3:39:e1:c0, 00:14:bf:3b:c7:c6, 00:14:bf:b1:97:81,
  # 00:14:bf:b1:97:8a, 00:14:bf:b1:97:8d, and 00:14:bf:b1:97:90.
  
  # We need to have six MAC addresses, so we keep the five MACs from Cisco
  # and randomly choose one of the Alpha Networks to eliminate.
  # Let's eliminate 00:0f:a3:39:dd:cd and keep 00:0f:a3:39:e1:c0.
  
  data = data[data$mac %in% keepMacs, ]
  
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
  #    Also, > unique(offline$scanMac)
  #         [1] 00:02:2D:21:0F:33
  # 2. Drop "posZ" because nrow(offline[offline$posZ != 0.0, ]) returns 0.
  #    Also, > unique(offline$posZ)
  #         [1] 0.0
  # 3. Drop "channel" because "channel" can be determined from the MAC address
  # 4. Drop "type" which can be computed from "mac" (the mac's type is 3 for access point and 1 for device in adhoc mode = 1)
  # drop columns method from http://stackoverflow.com/questions/4605206/drop-columns-r-data-frame/21719511#21719511
  data[ , c("scanMac", "posZ", "channel", "type")] = list(NULL)
  
  # Round the values for orientation to the nearest 45 degrees, but keep the original values too. 
  data$roundedOrientation = round(data$orientation / 45.0, digits = 0) * 45
  
  # The spec says that "Signal strengths were recorded at 8 orientations in 45 degree increments (i.e., 0, 45, 90, and so on)."
  # The eight orientations are: 0, 45, 90, 135, 180, 225, 270, 335.
  # Therefore, set all orientations that are 360 to 0.
  data$roundedOrientation[data$roundedOrientation == 360] = 0
  
  # get rid of the extraneous row names at the beginning of the data frame
  row.names(data) = NULL
  
  return(data)
}

################# Part 3  #################

# Given that we are computing distances between vectors of 6 signal strengths,
# it may be helpful to organize the data in a diﬀerent structure.
# Speciﬁcally, rather than a data frame with one column of signal strengths from all access points,
# let's organize the data so that we have 6 columns of signal strengths,
# i.e., one for each of the access points.
# The new data frame will not need to keep MAC address for the access points.
# It will have variables S1, S2, S3, S4, S5, S6,
# which correspond to the signal strengths for each of the 6 access points.
# Consequently, the number of rows in this data frame will be reduced by a factor of 6.

combineSignals = function(data) {
  
  # remove everything that doesn't fit
  relevantIndices = floor(nrow(data) / 6) * 6
  data = data[1:relevantIndices, ]
  
  # make an index for every sixth row starting at 1, 2, 3, 4, 5, and 6
  start1By6 = seq(1, nrow(data), by = 6)
  start2By6 = seq(2, nrow(data), by = 6)
  start3By6 = seq(3, nrow(data), by = 6)
  start4By6 = seq(4, nrow(data), by = 6)
  start5By6 = seq(5, nrow(data), by = 6)
  start6By6 = seq(6, nrow(data), by = 6)
  
  # keep only every sixth row
  newdf = data[start1By6, ]
  
  newdf$S1 = data[start1By6, ]$signal
  newdf$S2 = data[start2By6, ]$signal
  newdf$S3 = data[start3By6, ]$signal
  newdf$S4 = data[start4By6, ]$signal
  newdf$S5 = data[start5By6, ]$signal
  newdf$S6 = data[start6By6, ]$signal

  return(newdf)
  
}

# incomingSignal is a dataframe containing S1, S2, S3, S4, S5, and S6
# trainingData is a dataframe containing S1, S2, S3, S4, S5, and S6
findNearestNeighborAndPredXY = function(incomingSignal, trainingData, k) {
  distances = sqrt((incomingSignal$S1 - trainingData$S1)^2 + (incomingSignal$S2 - trainingData$S2)^2
                   + (incomingSignal$S3 - trainingData$S3)^2 + (incomingSignal$S4 - trainingData$S4)^2
                   + (incomingSignal$S5 - trainingData$S5)^2 + (incomingSignal$S6 - trainingData$S6)^2)
  orderedDistances = order(distances)
  orderedData = trainingData[orderedDistances[1:k], c("posX", "posY")]
  x = mean(orderedData$posX)
  y = mean(orderedData$posY)
  return(data.frame(x, y))
}

# Use Euclidean distance to measure how far from the truth the prediction is.
calculateErrors = function(actual, predicted) {
  return(sqrt((actual$posX - predicted$x)^2 + (actual$posY - predicted$y)^2))
}

structureData = function(txt) {
  tmp = lapply(txt, processLine)
  offline = as.data.frame(do.call("rbind", tmp))
  names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
                     "orientation", "mac", "signal", "channel", "type")
  offline2 = cleanData(offline)
  return(offline2)
}

# for the graphs
tmp = lapply(txt, processLine)
offline2 = as.data.frame(do.call("rbind", tmp))
names(offline2) = c("time", "scanMac", "posX", "posY", "posZ",
                   "orientation", "mac", "signal", "channel", "type")

offline3 = structureData(txt)
online3 = structureData(txt2)

online = combineSignals(online2)
offline = combineSignals(offline2)

# do the nearest neighbor computations and the cross validation
# the for loop takes about 1-3 hours to run
k = 20
averageErrors = vector(length = k)
for (i in 1:k) {
  predictions = do.call(rbind, by(online, seq_len(nrow(online)), function(row) findNearestNeighborAndPredXY(row, offline, i)))
  totalErrors = calculateErrors(online, predictions)
  averageError = mean(totalErrors)
  averageErrors[i] = averageError
}

# averageErrors
# > 8.984657 8.050310 7.678957 7.481312 7.345998 7.236572 7.172336 7.127412 7.103869
# 7.086165 7.082385 7.072182 7.060496 7.057262 7.045027
# [16] 7.022970 7.021125 7.019217 7.013011 7.018817

# which.min() returns the argument with the lowest error.
# That is, it returns the k with the lowest error and acts like argmin.
# k = 19
lowestErrorK = which.min(averageErrors)
minimumError = min(averageErrors)

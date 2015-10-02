################################ This is the solution to Q-1 ################################
# Inputs : V.Size = Vector of Numbers and N = Single Number
# Output : List object of size N

Q1 = function(V.Size, N)
{
  V.Size.sort = sort(V.Size, decreasing = TRUE)  # sorting the vector of numbers
  
  myList = list()  # creating an empty list
  
  for (i in 1:N)  # the size of the list is N
  {
    listMember = paste('member', i, sep = '')  # creating a list struture with ordered list items
    
    vectLength = N + i  # the vector length for each list item is defined
    
    listMember.Vector = rep("NA", vectLength)  # initializing the list vector
    
    for (j in 1:vectLength)  # creating the vector for every list item
    {
      listMember.Vector[j] = V.Size.sort[j]  # assigning the values to the vector in a loop
    }
    
    myList[listMember] = list(listMember.Vector)  # creating the final list
  }
return(myList)  # return the output
}

Q1(V.Size = c(1, 6, 2, 4, 3, 5, 12, 33, 14), N = 4)

# The output is below:
#$member1
#[1] "33" "14" "12" "6"  "5" 
#
#$member2
#[1] "33" "14" "12" "6"  "5"  "4" 
#
#$member3
#[1] "33" "14" "12" "6"  "5"  "4"  "3" 
#
#$member4
#[1] "33" "14" "12" "6"  "5"  "4"  "3"  "2"

###################### This is the solution to Q-2 ###########################
# Input : Interger Vector
# Output : Dataframe with duplicates and the number of times they have occurred


Q2 = function(V.Ints)
{
  NumOfTimes = data.frame(table(V.Ints))
  
  names(NumOfTimes)  # one of the item is Freq which we will use.
  
  dupDataFrame = NumOfTimes[NumOfTimes$Freq > 1,]  # data frame with only which has occurred more than once
  
  return(dupDataFrame)  # return the duplicate data frame
}

Q2(V.Ints = c(7, 1, 4, 9, 11, 7, 4, 7, 13, 19, 20, 19, 1, 1, 1, 1, 25, 99, 100, 77, 99))

# The output is below:

#    V.Ints Freq
#1       1    5
#2       4    2
#3       7    3
#7      19    2
#11     99    2

###################### This is the solution to Q-3 ###########################
# Input : V.dates = vector of dates
# Output : A data frame that contains the original date, the duration from present day, 
#         and the quarter to which the date belongs. 


Q3 = function(V.dates)
{
  todayDate = Sys.Date()  # Today's date
  
  daysInterval = difftime(todayDate, V.dates, units = "days")   # time interval b/w today and date vectors
  
  numofDates = length(V.dates)  # to create a Quarter column
  
  calcQuarter = rep("NA", numofDates)  # created a calcQuarter column
  
  timeDataFrame = data.frame(V.dates, daysInterval, calcQuarter)  # created a data frame
  
  maxDaysInterval = max(daysInterval)  # to compute num of quarters needed
  
  numOfQuarters = as.integer(trunc(maxDaysInterval / 91))  # calculating the number of quarters
  
  outputDataFrame = data.frame()  # teh output dataframe which will be used in the below loop.
  
  k = 0  # starts from 0
  l = 91  # the quarter ends with 91 days
  
  for (j in 0:numOfQuarters)
  {
    searchIntervals = timeDataFrame$daysInterval[abs(daysInterval) >= k & abs(daysInterval) <= l]  # quarter wise selection
    
    tempDataFrame = subset(timeDataFrame, subset = (daysInterval %in% searchIntervals))  # a temp dataframe to store the subset of data
    
    tempDataFrame$calcQuarter = j  # assigning each quarter to the subset of data
    
    outputDataFrame = rbind(tempDataFrame, outputDataFrame)  # adding rows for each increment (i.e for each quarter)
    
    k = k + 91  # moves to the next quarter
    l = l + 91  # moves to the next quarter
  }
  
  # sorting the output data frame by date (ascending)
  
  result = outputDataFrame[order(as.Date(outputDataFrame$V.dates, format = "%Y/%m/%d")), ]
  
  return(result)
}

Q3(V.dates = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "week"))

# The Output is below:

#     V.dates       daysInterval   calcQuarter
#1   2015-01-01     217 days           2
#2   2015-01-08     210 days           2
#3   2015-01-15     203 days           2
#4   2015-01-22     196 days           2
#5   2015-01-29     189 days           2
#6   2015-02-05     182 days           2
#61  2015-02-05     182 days           1
#7   2015-02-12     175 days           1
#8   2015-02-19     168 days           1
#9   2015-02-26     161 days           1
#10  2015-03-05     154 days           1
#11  2015-03-12     147 days           1
#12  2015-03-19     140 days           1
#13  2015-03-26     133 days           1
#14  2015-04-02     126 days           1
#15  2015-04-09     119 days           1
#16  2015-04-16     112 days           1
#17  2015-04-23     105 days           1
#18  2015-04-30      98 days           1
#19  2015-05-07      91 days           1
#191 2015-05-07      91 days           0
#20  2015-05-14      84 days           0
#21  2015-05-21      77 days           0
#22  2015-05-28      70 days           0
#23  2015-06-04      63 days           0
#24  2015-06-11      56 days           0
#25  2015-06-18      49 days           0
#26  2015-06-25      42 days           0
#27  2015-07-02      35 days           0
#28  2015-07-09      28 days           0
#29  2015-07-16      21 days           0
#30  2015-07-23      14 days           0
#31  2015-07-30       7 days           0
#32  2015-08-06       0 days           0
#33  2015-08-13      -7 days           0
#34  2015-08-20     -14 days           0
#35  2015-08-27     -21 days           0
#36  2015-09-03     -28 days           0
#37  2015-09-10     -35 days           0
#38  2015-09-17     -42 days           0
#39  2015-09-24     -49 days           0
#40  2015-10-01     -56 days           0
#41  2015-10-08     -63 days           0
#42  2015-10-15     -70 days           0
#43  2015-10-22     -77 days           0
#44  2015-10-29     -84 days           0
#45  2015-11-05     -91 days           1
#451 2015-11-05     -91 days           0
#46  2015-11-12     -98 days           1
#47  2015-11-19    -105 days           1
#48  2015-11-26    -112 days           1
#49  2015-12-03    -119 days           1
#50  2015-12-10    -126 days           1
#51  2015-12-17    -133 days           1
#52  2015-12-24    -140 days           1
#53  2015-12-31    -147 days           1

################ This is the solution to Q-4 ##################
# Input : V.text = Vector of text strings
# Output : A list containing two vectors, Date and Amount


Q4 = function(V.text)
{
  extractList = list()
  
  for (i in 1:length(V.text))
  {
    text = V.text[i]
    
    splitInWords = strsplit(text, " ")[[1]]  # split the text in words
    
    splitInDates = grep("/", splitInWords, value = TRUE)  # extract the dates
    
    splitInAmount = grep("^[$0-9,]+$", splitInWords, value = TRUE) # begining with these characters.
    
    extractList[[i]] = list(splitInDates, splitInAmount)
  }
  
  return(extractList)
}

Q4(V.text = c("Listed on 1/05/2009 for 180000 and sold for $150,250 on 3/1/2009",
           "listed on 01/08/15 for 200 and sold for $400 on 2/8/2015"))

# The output is as below-

#[[1]]
#[[1]][[1]]
#[1] "1/05/2009" "3/1/2009" 

#[[1]][[2]]
#[1] "180000"   "$150,250"


#[[2]]
#[[2]][[1]]
#[1] "01/08/15" "2/8/2015"

#[[2]][[2]]
#[1] "200"  "$400"

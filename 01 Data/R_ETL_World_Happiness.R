require(readr)

# Set the Working Directory to the 00 Doc folder
# Download the cannata/diamonds file into a folder ../../CSVs and rename the file PreETL_Diamonds.csv
#../../ means two folders up from working directory
file_path = "CSVs/PreETL_WorldHappiness.csv"
happiness <- readr::read_csv(file_path)
names(happiness)

df <- happiness
names(df)
str(df) # Uncomment this line and  run just the lines to here to get column types to use for getting the list of measures. first section tells you the columb type chr (dimension), num (meajavascript:;sure), int, etc.

#putting all measures in a list. Measures are listed in the section explained above
measures <- c()

#sets everything else as dimensions, by separating what is the difference btwn col names and measure list
dimensions <- setdiff(names(df), measures)
dimensions

# Get rid of special characters in each column.
# Google ASCII Table to understand the following:
#for all names in col set it equal to n
# removes all special char THIS IS CRITICAL!! [^ -~] means everything not (^) btwn space and tilda, 
# called regular expression. gsub means substitute with the replacement, an empty string. Doing that in # every column so for loop is going through each column, n, with lapply gives it the column where every # row in the the column (df[n]) will substitute (gsub function) all special characters with an empty    # string, essentially deleting it. Helps with JSON errors

#defining another function that changes na to 0 in measures
na2zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions, and change NA to 0.
if( length(measures) > 1) {
  for(m in measures) {
    print(m)
    df[m] <- data.frame(lapply(df[m], na2zero)) #IMPORTANT. makes sure that NA is equal to 0
    df[m] <- data.frame(lapply(df[m], as.numeric)) # This is needed to turn measures back to numeric because gsub turns them into strings.
  }
}

# making a function being stored in variable (na2emptystring) that takes in one argument saying if that # argument is na (not applicable) then change it to an empty string, otherwise return x
na2emptyString <- function (x) {
  x[is.na(x)] <- ""
  return(x)
}

#if there are dimentions, do all the stuff below to each one
if( length(dimensions) > 0) {
  for(d in dimensions) {
    # Change NA to the empty string.
    df[d] <- data.frame(lapply(df[d], na2emptyString)) #lapply the function we just made on every row in     the d column of the data frame. lapply, you give it a list and calls the function (na2empty) on each     element of the list. two arguments here, list then function. 
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[^ -~]",replacement= "")) 
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= "")) #run find & replace to        find any character that is a double quote (\" means escaping the double quote) or a single quote and     replace it with an empty string (remove it). You do not want to have strings with double/single         quotes bc those are used as delimiters in JSON and will confuse him :-(
    
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and ")) #if finds & replaces it      with space 'and' space. Oracle will fuck up with these in there. 
    
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";")) #Oracle gets confused by         colons so it replaces it with semicolons. 
  }
}

str(df) #just checkin
#Now we have cleaned out the data.

write.csv(df, gsub("PreETL_", "", file_path), row.names=FALSE, na = "") #write the csv. removing PreETL to rename file. 

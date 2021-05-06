#########################################################################
# Remove duplicate rows (posts) from CrowdTangle data
# Code author Irene Morse (UMich)
# Tested on data from Dr. Justine Davis (UMich)
# Last modified 6 May 2021
#########################################################################
# When CrowdTangle data is downloaded at different points in time and merged,
# duplicate rows/posts are somtimes introduced.
# There are posts which have the same unique URL but appear in the data twice.
# In the data they generally differ on certain dimensions other than URL.
# For example, a post may receive more likes or shares over time.
# This code allows you to examine and delete such duplicates,
# based on total likes and one user-defined criterion.

# You can also set an acceptable threshold for differences in likes
# using the likes_diff parameter (default is 1).
# The user-defined criterion is defined through the crit_var parameter
# and it must a variable that can be ordered (e.g. Shares, Date).
#########################################################################

# Step 1: Collect all duplicate posts for examination
CollectDuplicates <- function(dataframe) {
  dataframe$duplicateURL <- duplicated(dataframe$URL)
  cat('There are ', sum(dataframe$duplicateURL), ' duplicate rows/posts.')
  if(sum(dataframe$duplicateURL)==0){
    return(list())
  }
  print('Process updates: ')
  Duplicates <- list()
  elem <- 1
  for(i in 1:nrow(dataframe)){
    if(dataframe$duplicateURL[i]){
      Duplicates[[elem]] <- which(dataframe$URL==dataframe$URL[i])
      elem <- elem + 1
    }
    if(i %% 10000 == 0){
      cat("row ", i, " complete...")
    }
  }
  
  return(Duplicates)
}

# Step 2: Examine differences between duplicates
ExamineDuplicates <- function(Duplicates, dataframe){
  print('Process updates: ')
  Changed <- data.frame(matrix(0, ncol=ncol(fulldata), nrow=length(Duplicates)))
  colnames(Changed) <- colnames(dataframe)
  for(i in 1:length(Duplicates)){
    elems <- Duplicates[[i]]
    if(length(elems)>=2){
      for(col in 1:ncol(dataframe)){
        if(!(identical(dataframe[elems[1],col], dataframe[elems[2],col]))){
          Changed[i,col] <- Changed[i,col]+1
        }
      }
    }
    if(length(elems)>=3){
      for(col in 1:ncol(dataframe)){
        if(!(identical(dataframe[elems[2],col], dataframe[elems[3],col]))){
          Changed[i,col] <- Changed[i,col]+1
        }
      }
    }
    if(i %% 10000 == 0){
      cat("element ", i, " complete...")
    }
  }
  return(Changed)
}

# Step 3: Identify duplicates to keep/delete based on text, likes and 1 additional criterion
SortDuplicates <- function(Duplicates, dataframe, text_var, crit_var, likes_diff = 1){
  # create all_likes if it doesn't exist already
  if(!("all_likes" %in% colnames(dataframe))){
    dataframe$all_likes <- fulldata$Likes + fulldata$Love + fulldata$Wow + 
      fulldata$Haha + fulldata$Sad + fulldata$Angry + fulldata$Care
    print("all_likes variable created.")
  }
  print("Process updates: ")
  to_keep <- vector()
  to_delete <- vector()
  unsure <- vector()
  for(i in 1:length(Duplicates)){
    elems <- Duplicates[[i]]
    text_var_nchar <- nchar(dataframe[elems,text_var])
    if((0 %in% text_var_nchar) & (sum(text_var_nchar) != 0)){
      # if one text var is empty but others are not empty
      # delete the row with the empty text var
      idx_del <- which(text_var_nchar == 0)
      to_delete <- c(to_delete, elems[idx_del])
      # for remaining elems check all_likes and criterion
      elems_left <- elems[-idx_del]
      order1 <- elems_left[order(dataframe[elems_left, "all_likes"])]
      order2 <- elems_left[order(dataframe[elems_left, crit_var])]
      if(unique(order1==order2)==TRUE){
        # if most_likes and criterion line up, keep based on either variable
        to_keep <- c(to_keep, order1[length(order1)])  ## order1 or order2 is fine
        to_delete <- c(to_delete, order1[-length(order1)])  ## order1 or order2 is fine
      }else if(max(dataframe[elems_left, "all_likes"]) - min(dataframe[elems_left, "all_likes"]) <= likes_diff){
        # if difference in likes is minimal, keep based on criterion
        to_keep <- c(to_keep, order2[length(order2)])  ## must be order2
        to_delete <- c(to_delete, order2[-length(order2)])  ## must be order2
      }else{
        unsure <- c(unsure, elems_left)
      }
    }else if((!(0 %in% text_var_nchar)) | (sum(text_var_nchar) == 0)){
      # if one text var is empty but others are not empty
      # check all_likes and criterion
      order1 <- elems[order(dataframe[elems, "all_likes"])]
      order2 <- elems[order(dataframe[elems, crit_var])]
      if(unique(order1==order2)==TRUE){
        # if most_likes and criterion line up, keep based on either variable
        to_keep <- c(to_keep, order1[length(order1)])  ## order1 or order2 is fine
        to_delete <- c(to_delete, order1[-length(order1)])  ## order1 or order2 is fine
      }else if(max(dataframe[elems, "all_likes"]) - min(dataframe[elems, "all_likes"]) <= likes_diff){
        # if difference in likes is minimal, keep based on criterion
        to_keep <- c(to_keep, order2[length(order2)])  ## must be order2
        to_delete <- c(to_delete, order2[-length(order2)])  ## must be order2
      }else{
        unsure <- c(unsure, elems)
      }
    }
    
    if(i %% 10000 == 0){
      cat("element ", i, " complete...")
    }
  }
  
  # check that everything is accounted for
  if(sum(unlist(lapply(Duplicates, length))) == (length(to_delete)+length(to_keep)+length(unsure))){
    print("Check 1 TRUE")
  }else{
    print("Check 1 FALSE. There is an issue.")
  }
  if(length(Duplicates) == length(to_keep)){
    print("Check 2 TRUE")
  }else{
    print("Check 2 FALSE. There is an issue.")
  }
  
  SortedElements <- list('to_delete'=to_delete, 'to_keep'=to_keep, 'unsure'=unsure)
  print("Sorting complete.")
  cat("Unsure about ", length(unsure), " element(s).")
  return(SortedElements)
}

# Step 4: Delete relevant duplicates
DeleteDuplicates <- function(dataframe, SortedElements){
  to_delete <- SortedElements$to_delete
  # remove duplicates
  dataframe_noduplicates <- dataframe[-to_delete,]
  return(dataframe_noduplicates)
}

#########################################################################
# Testing / Examples of Use
#########################################################################

# load data as dataframe
setwd('E://Project with Justine Davis')
fulldata <- read.csv('CrowdTangle_01012015-12012020-emojis_V3.csv')

# Step 1
Duplicates <- CollectDuplicates(fulldata)
# check length of duplicate groups
unique(unlist(lapply(Duplicates, length)))  ## all are pairs or triples (2 or 3)
sum(unlist(lapply(Duplicates, length)))
#save(Duplicates, file = 'Duplicates')  ## to avoid having to rerun this slow thing
#load('Duplicates')

# Step 2
Changed <- ExamineDuplicates(Duplicates, fulldata)
sort(colSums(Changed))

# Look into changes
fulldata$Video.Length[Duplicates[[which(Changed$Video.Length>=1)[4]]]]
fulldata$URL[Duplicates[[which(Changed$Likes.at.Posting>=1)[1]]]]
fulldata$Group.Name[Duplicates[[which(Changed$Group.Name>=1)[1]]]]

# examine issues with message vars
for(change in which(Changed$message3>=1)){
  cat("relevant indices ", Duplicates[[change]], "\n")
  cat("message 2:\n")
  print(fulldata$message2[Duplicates[[change]]])
  cat("message 3:\n")
  print(fulldata$message3[Duplicates[[change]]])
  cat("\n\n\n")
}

# examine issues with Group.Name and relevant coding
out <- list()
i <- 1
for(change in which(Changed$Group.Name>=1)){
  out[[i]] <- c(fulldata$Group.Name[Duplicates[[change]]], 
                fulldata$soro[Duplicates[[change]]], 
                fulldata$gbagbo[Duplicates[[change]]], 
                fulldata$bedie[Duplicates[[change]]],
                fulldata$ado[Duplicates[[change]]],
                fulldata$affi[Duplicates[[change]]],
                fulldata$politic[Duplicates[[change]]])
  i = i + 1
}
length(out)
length(unique(out))
print(unique(out))

# Step 3
fulldata$Created <- strptime(fulldata$Created, format = "%Y-%m-%d %T")
fulldata$Created <- as.POSIXct(fulldata$Created)
fulldata$download_date <- as.Date(fulldata$download_date, format = "%m-%d-%Y")
# check if all_likes and download date line up (more likes, later date)
# if so, delete earlier one
SortedElements <- SortDuplicates(Duplicates, fulldata, 'message3', 'download_date', likes_diff = 2)

# Step 4
fulldata_noduplicates <- DeleteDuplicates(fulldata, SortedElements)
should_be_empty_now <- CollectDuplicates(fulldata_noduplicates)
print(should_be_empty_now)

# save
write.csv(fulldata_noduplicates, file = 'CrowdTangle_01012015-12012020-emojis_V3_nodups.csv', row.names = FALSE)

#########################################################################
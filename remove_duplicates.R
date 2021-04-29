#########################################################################
# Remove duplicate rows (posts) from CrowdTangle data
# Code author Irene Morse (UMich)
# Tested on data from Dr. Justine Davis (UMich)
# Last modified 29 April 2021
#########################################################################
# When CrowdTangle data is downloaded at different points in time and merged,
# duplicate rows/posts are somtimes introduced.
# There are posts which have the same unique URL but appear in the data twice.
# In the data they generally differ on certain dimensions other than URL.
# For example, a post may receive more likes or shares over time.
# This code allows you to examine and delete such duplicates,
# using user-defined criteria.
#########################################################################

# Step 1: Collect all duplicate posts for examination
CollectDuplicates <- function(dataframe) {
  dataframe$duplicateURL <- duplicated(dataframe$URL)
  cat('There are ', sum(dataframe$duplicateURL), ' duplicate rows/posts.')
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

# Step 3: Identify duplicates to keep/delete based on text and 2 additional criteria

# next stage:
# 1. check if all_likes and download date line up (more likes, later date)
# 2. if so, delete earlier one
# 3. if not, collect these indices for further examination

SortDuplicates <- function(Duplicates, dataframe, text_var, crit1_var, crit2_var) {
  print("Process updates: ")
  to_keep <- vector()
  to_delete <- vector()
  unsure <- vector()
  for(i in 1:length(Duplicates)){
    elems <- Duplicates[[i]]
    text_var_nchar <- nchar(dataframe[elems,text_var])
    if((0 %in% text_var_nchar) & (sum(text_var_nchar) != 0)){
      idx_del <- which(text_var_nchar == 0)
      to_delete <- c(to_delete, elems[idx_del])
      elems_left <- elems[-idx_del]
      most_likes <- which(dataframe[elems_left, crit1_var] == dataframe[elems_left, crit1_var])
      latest_dd <- which(dataframe[elems_left, crit2_var] == dataframe[elems_left, crit2_var])
      if(unique(most_likes==latest_dd)){
        to_keep <- c(to_keep, elems_left[1])
        to_delete <- c(to_delete, elems_left[-1])
      }else{
        unsure <- c(unsure, elems_left)
      }
    }else if(!(0 %in% text_var_nchar) | (sum(text_var_nchar) == 0)){
      most_likes <- which(dataframe[elems, crit1_var] == dataframe[elems, crit1_var])
      latest_dd <- which(dataframe[elems, crit2_var] == dataframe[elems, crit2_var])
      if(unique(most_likes==latest_dd)){
        to_keep <- c(to_keep, elems[1])
        to_delete <- c(to_delete, elems[-1])
      }else{
        unsure <- c(unsure, elems)
      }
    }
    
    if(i %% 10000 == 0){
      cat("element ", i, " complete...")
    }
  }
  
  # check that everything is accounted for
  if(sum(unlist(lapply(Duplicates, length))) == (length(to_delete)+length(to_keep)+sum(unlist(lapply(lapply(unsure, '[', 1), length))))){
    "Check 1 TRUE"
  }else{
    "Check 1 FALSE. There is an issue."
  }
  if(length(Duplicates) == length(SortedElements$to_keep)){
    "Check 2 TRUE"
  }else{
    "Check 2 FALSE. There is an issue."
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
fulldata <- read.csv('CrowdTangle_01012015-12012020-emojis_V3.csv', row.names = FALSE)

# Step 1
Duplicates <- CollectDuplicates(fulldata)
# check length of duplicate groups
unique(unlist(lapply(Duplicates, length)))  ## all are pairs or triples (2 or 3)

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
SortedElements <- SortDuplicates(Duplicates, fulldata, 'message2', 
                                 'all_likes', 'download_date')

# Step 4
fulldata_noduplicates <- DeleteDuplicates(fulldata, SortedElements)
should_be_empty_now <- CollectDuplicates(fulldata_noduplicates)

# save
write.csv(fulldata_noduplicates, file = 'CrowdTangle_01012015-12012020-emojis_V3_nodups.csv', row.names = FALSE)

#########################################################################
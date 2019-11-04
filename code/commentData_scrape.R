################################################################################################################################################
## This script takes in a vector of YT vid IDS and extracts ALL comment text, vido ID, commenter name, timestamp, number of likes.            ##
## Uses four main functions to produce the desired output: get_all_comments, flatten_dfs, subset_removed_vids, & final_step.                  ##
## Functions last updated 18 September 2019.                                                                                                            ##
################################################################################################################################################

library(dplyr)
library(curl)
library(magrittr)
library(tidyr)
library(jsonlite)
library(purrr)

setwd("~/Desktop")
# female_vtts <- read.csv("female_vtts_IDs_combined.csv", stringsAsFactors = F)
setwd("~/Dropbox (MIT)/AltRight/scraping_youtube/female_YT_SCRAPE2")
female_vtts <- read.csv("female_vtts_IDs_combined_Blair_RedIce_Removed.csv", stringsAsFactors = F)
female_vtts <- subset(female_vtts, female_vtts$ID != "Now" & female_vtts$ID != "Fear") # remove the "Now" & "Fear" mistakes which should've been removed earlier
vidID <- sample(female_vtts$ID,size = 150, replace = F) # slightly more, in case some are removed (ie Faith Goldy )
class(vidID) # good, character 
vidID <- unique(vidID) # good, shouldn't be any duplicates 


########################################################################
##  I.  Extract ALL comments for each video, using API                ##
########################################################################

get_all_comments <- function(vidID){
  # browser()
  firstPart_http <- "https://www.googleapis.com/youtube/v3/commentThreads?key=AIzaSyCg9dPw32nUG6ATnElRa5hXsYXD0RnzSAM&textFormat=plainText&part=snippet&videoId="
  video_ID <- vidID
  maxRes <- "&maxResults=100"
  faith <- curl_fetch_memory(paste0(firstPart_http, video_ID, maxRes)) # named bc tested on faith goldy
  faith$status_code # good, status code == 200 
  faith <- jsonlite::prettify(rawToChar(faith$content)) # convert from binary to readable characters 
  str(faith) # json now 
  faith <- fromJSON(faith) # now list object with vars of interest
  token <- faith$nextPageToken # extract and replace this key each time for all comments
  comments <- faith$items$snippet$topLevelComment 
  rownames(comments) <- NULL 
  
  i <- 1 
  store <- list() 
  
  while(! is.null(token)){
    
    baseurl <- "https://www.googleapis.com/youtube/v3/commentThreads?key=XXX&textFormat=plainText&part=snippet&videoId=" # Replace key=XXX with actual API key
    video_ID <- vidID 
    nptokenarg <- "&pageToken="
    faith2 <- curl_fetch_memory(paste0(baseurl, video_ID, nptokenarg, token, maxRes))
    faith2 <- jsonlite::prettify(rawToChar(faith2$content))
    faith2 <- fromJSON(faith2)
    token <- faith2$nextPageToken
    df <- faith2$items$snippet$topLevelComment # temp df 
    print(df$snippet$videoId) # check progress 
    print(head(df$snippet$textDisplay)) # ensure new names each time 
    rownames(df) <- NULL
    store[[i]] <- df
    i <- i + 1
  }
  
  store[[length(store)+1]] <- comments 
  return(list(store))
}

output <- lapply(vidID, get_all_comments) 
glimpse(output) # fourth level is the df 


########################################################################
##  II.  Flatten nested lists w/ nested dfs of varying lengths        ##
########################################################################


flatten_dfs <- function(x){
  # browser()
  sublist <- x[[1]] # extract list of dfs 
  print(length(sublist)) # find out the length of list here (*4 post-flattened)
  sublist <- sublist %>% purrr::flatten()
  if (length(sublist) == 4){
    result <- sublist$snippet
    result2 <- result %>% dplyr::select(authorDisplayName, videoId, textOriginal, likeCount, publishedAt, canRate, viewerRating)
  } else if (length(sublist) > 4) { # multiple dataframes 
    i <- 4 
    j <- 1 
    dfs <- c()
    while(i <= length(sublist)){
      result <- sublist[[i]]
      result2 <- result %>% dplyr::select(videoId, authorDisplayName, textOriginal, likeCount, publishedAt, canRate, viewerRating)
      i <- i + 4 # every fourth element is the next snippet
      j <- i + 1 # for indexing 
      dfs <- c(dfs, result2)
    }
    return(dfs)
  } else if (length(sublist) == 0){ 
    result2 <- data.frame("videoId" = 1:20, "authorDisplayName" = "Video Removed")
  }
  together <- bind_rows(result2) # bind all rows from before 
  return(together)
}

store2 <- lapply(output, FUN = flatten_dfs) 
glimpse(store2)


########################################################################
##  III.  Omit rows for videos which have been taken down             ##
########################################################################

subset_removed_vids <- function(x){
  sublist <- x[[1]]
  newer <- subset(sublist, length(sublist) == 0)
}

removed <- lapply(store2, FUN = subset_removed_vids)
glimpse(removed)

dummy <- NA %>% as.vector()
for(i in 1:length(removed)){
  dummy[i] <- ifelse(class(removed[[i]]) == "character", 1, 0)
}

store2_clean <- store2[-c(which(grepl(0, dummy)))] # comment data with removed vids omitted, use this to proceed, okay, 108 elements, so 42 were removed from that sample! 


########################################################################
##  IV.  Unlist elements and append dfs for each ID                   ##
########################################################################

final_step <- function(x){ 
  # browser() 
  if (class(x) == "data.frame"){
    stuff <- x
  }
else if (class(x) == "list"){
  # browser()
  vid_id <- x[seq(from = 1, to = length(x), by = 7)] %>% unlist() %>% as.data.frame() # 7 variables repeating in this order for each of the appended dfs 
  comm_name <- x[seq(from = 2, to = length(x), by = 7)] %>% unlist() %>% as.data.frame()
  txt_og <- x[seq(from = 3, to = length(x), by = 7)] %>% unlist() %>% as.data.frame()
  likes <- x[seq(from = 4, to = length(x), by = 7)] %>% unlist() %>% as.data.frame()
  pub_time <- x[seq(from = 5, to = length(x), by = 7)] %>% unlist() %>% as.data.frame()
  can_rate <- x[seq(from = 6, to = length(x), by = 7)] %>% unlist() %>% as.data.frame()
  view_rating <- x[seq(from = 7, to = length(x), by = 7)] %>% unlist() %>% as.data.frame()
  
  # store and append these all 
  vid_id_All <- c() 
  vid_id_All <- rbind(vid_id_All, vid_id)
  comm_name_All <- c()
  comm_name_All <- rbind(comm_name_All, comm_name)
  txt_og_All <- c()
  txt_og_All <- rbind(txt_og_All, txt_og)
  likes_All <- c()
  likes_All <- c(likes_All, likes)
  pub_time_All <- c()
  pub_time_All <- c(pub_time_All, pub_time)
  can_rate_All <- c()
  can_rate_All <- c(can_rate_All, can_rate)
  view_rating_All <- c()
  view_rating_All <- c(view_rating_All, view_rating)
  
  # combine this all into df 
  stuff <- cbind(comm_name_All, vid_id_All, txt_og_All, likes_All, pub_time_All, can_rate_All, view_rating_All) # hm, now it just puts whatever is first.. 
  colnames(stuff) <- c("authorDisplayName", "videoId", "textOriginal", "likeCount", "publishedAt", "canRate", "viewerRating")
  }
  together <- bind_rows(stuff) # join all "stuff" dfs 
  return(together)
}

final_df <- lapply(store2_clean, FUN = final_step) # list of all dfs combined together 

clean_comment_data <- do.call("rbind", final_df) # now, unlist list of dfs 

### FINALLY works. 

####################################################
##### Additional - refining the sample etc:    ##### 
####################################################



#####***
setwd("~/Desktop/all_comments_data")
female_cleanCommData <- read.csv("femaleComments_sample100.csv", stringsAsFactors = F)


####### sample 100 videos #######

length(unique(female_cleanCommData$videoId)) # 100

# write.csv(x, "femaleComments_sample100.csv", row.names = F)



nested_samples <- female_cleanCommData %>% 
  group_by(videoId) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(n = 70) # do extra because when making distinct will bring it back down 

glimpse(nested_samples)

sampled_data <- nested_samples %>% 
  mutate(samp = map2(data, n, sample_n, replace = T)) %>% 
  dplyr::select(-data) %>% 
  unnest(samp) 

sampled_data <- sampled_data %>% distinct(textOriginal, .keep_all = T) # okay, 5975 unique rows sampled 
# (could have duplicate names but not duplicate comments) - get this down to 5k to merge back into 

samp_text <- sample(unique(sampled_data$textOriginal), 5000)
final_female <- sampled_data[sampled_data$textOriginal %in% samp_text, ] ### okay, great, this is the og to merge back into 
final_female$vid_female <- 1 # keep this to indicate when altogether that it's a male vid

write.csv(final_female, "female_fullSample_toMerge.csv", row.names = F)
# so then join this with 5k females (unique rows but not necessarily unique author names)
#### Now, get the unique names and throw that in a google doc 

length(unique(final_female$textOriginal)) # okay good, 5000 
length(unique(final_female$authorDisplayName)) # 4508 unique commenter names 

final_female_unique <- final_female %>% distinct(authorDisplayName, .keep_all = T) # unique author commenter names 

setwd("~/Desktop/all_comments_data/toMerge")
write.csv(final_female_unique, "female_uniqueNames_toMerge.csv", row.names = F)


# okay, now merge both together: 
## complete 10k of unique rows to eventually merge: 

female_full <- read.csv("female_fullSample_toMerge.csv", stringsAsFactors = F)
male_full <- read.csv("male_fullSample_toMerge.csv", stringsAsFactors = F)

bothGenders_full10kk <- rbind(female_full, male_full) 
bothGenders_full10kk <- bothGenders_full10kk[, -2]
bothGenders_full10kk <- bothGenders_full10kk %>% arrange(authorDisplayName)
write.csv(bothGenders_full10kk, "bothGenders_full10k_toMerge.csv", row.names = F)

## merged unique names m and f 

female_unique <- read.csv("female_uniqueNames_toMerge.csv", stringsAsFactors = F)
male_unique <- read.csv("male_uniqueNames_toMerge.csv", stringsAsFactors = F)

bothGenders_unique <- rbind(female_unique, male_unique)
bothGenders_unique <- bothGenders_unique[, -2]
bothGenders_unique <- bothGenders_unique %>% arrange(authorDisplayName)

write.csv(bothGenders_unique, "bothGenders_unique_toMerge.csv", row.names = F)

######***



####### sample 100 comments for each video #######

nested_samples <- x %>% 
  group_by(videoId) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(n = 170) # do extra because when making distinct will bring it back down 

glimpse(nested_samples)

sampled_data <- nested_samples %>% 
  mutate(samp = map2(data, n, sample_n, replace = T)) %>% 
  dplyr::select(-data) %>% 
  unnest(samp) 


sampled_data <- sampled_data %>% distinct(authorDisplayName, .keep_all = T) # unique author commenter names 


sampled_data_10k <- sampled_data[sample(nrow(sampled_data), 10000),]

glimpse(sampled_data) # looks good 
length(unique(sampled_data_10k$authorDisplayName))# okay, 8573 unique names but different comments # or 10K as well 

write.csv(sampled_data, "femaleComments_sample10k.csv", row.names = F)


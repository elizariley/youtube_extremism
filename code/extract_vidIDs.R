# This script extracts vid ids from vtts so can link to meta- and comment data 

# Females
setwd("~/Dropbox (MIT)/AltRight/scraping_youtube/female_YT_SCRAPE2") 
library(readxl)
library(dplyr)
library(stringi) # for stri_sub function used to keep only last 11 characters (yt ID) 

# this is the directory where each folder is the name of a (female) alt-Righter 
subdir_1 <- list.files("~/Dropbox (MIT)/AltRight/scraping_youtube/female_YT_SCRAPE2/FEMALES") %>% as.data.frame()
head(subdir_1)
colnames(subdir_1)[1] <- "subdir1_person_name"
class(subdir_1)

# remove the excess files i.e. code in this second scrape dropbox folder.
# also remove blaire white and RedIceTV 
subdir_1# ok, so these are rows 13:16 
class(subdir_1$subdir1_person_name)
subdir_1$subdir1_person_name <- subdir_1$subdir1_person_name %>% as.character()
subdir_1 <- subdir_1[-c(1, 12:16), ] %>% as.data.frame()
colnames(subdir_1)[1] <- "subdir1_person_name"

# now for each of these people-name-folders, I'm listing the files in the respective subdirectories & store all in one place in my enviro

person <- c()

for(i in 1:nrow(subdir_1)){
  person[i] <- subdir_1$subdir1_person_name[i] %>% as.character()
  second_subdirectory <- list.files(paste0("~/Dropbox (MIT)/AltRight/scraping_youtube/female_YT_SCRAPE2/FEMALES/", person)) %>% as.data.frame()
}

second_subdirectory # all vtts combined in one place

## all ## 
head(second_subdirectory$.) # try to think of a way to exctract the ID as a way to merge etc. maybe just add as a new column rather than completely rewrite etc. 
second_subdirectory$. <- second_subdirectory$. %>% as.character()

# okay, this should get rid of the stuff after the id, so ".en.vtt"
second_subdirectory$ID <- gsub("\\..*$", "", second_subdirectory$.)  %>% as.character() # deletes stuff after the vidID 
second_subdirectory$ID
class(second_subdirectory)

## keep only last 11 chars because all yt vidIDs are 11 chars (tg)
# install.packages("stringi")
second_subdirectory$ID <- stri_sub(second_subdirectory$ID, from = -11, to = -1) 
second_subdirectory$ID # okay, sweet, did this for all 
colnames(second_subdirectory)[1] <- "full_vtt_fileName"
colnames(second_subdirectory) # where ID is youtube ID 

# Remove additional non-IDs

library(stringr)
female_vtts_IDs_combined <- second_subdirectory %>% filter(!str_detect(second_subdirectory$ID, ' '))
spacey <- second_subdirectory %>% filter(str_detect(second_subdirectory$ID, ' ')) # okay, omitting these 38 vids which don't have the vid ID in the title for some reason 
# delete [838,] which is just the word "now" in place of ID

write.csv(female_vtts_IDs_combined, "female_vtts_IDs_combined_Blair_RedIce_Removed.csv", row.names = F)

# can then loop metadata python code over this list of vidIDs
# Now link up the jsons to vid names and IDs 
library(dplyr)
json_files <- list.files() %>% as.character()
class(json_files)

tail(json_files) # delete 3 files at the end 
json_files <- json_files[-c(2193:2195)]
head(json_files)
class(json_files) # chlass character 

# Continue combination

test <- transform(json_files, first4 = substr(json_files, 1, 4), number.json = substr(json_files, 5, 8)) %>% as.data.frame()
class(test$number.json)
test$number.json <- as.character(test$number.json)
class(test$number.json)
test$number.json <- readr::parse_number(test$number.json)
test$number.json <- test %>% arrange(number.json) %>% as.data.frame()
test$number.json <- test$number.json[c(2190:2195)]

#######
vtts_csv <- read.csv("female_vtts_IDs_combined.csv", stringsAsFactors = F)
combined <- cbind(vtts_csv, test)
head(combined)

combined2 <- combined2[, -c(3, 5)]
combined2 <- combined2[, -4] # doesn't remove for some reason 

colnames(combined2)[3] <- "json_number"

write.csv(combined2, "jsonNumbers_vttIDs.csv", row.names = F)



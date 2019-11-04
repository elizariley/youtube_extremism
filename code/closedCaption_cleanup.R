# This script reads in a folder of vtts (which contain scraped closed caption text) and converts them from .vtt format with time stamps to clean .txt files 

setwd("~/Dropbox (MIT)/AltRight/scraping_youtube/male_YT_sample5") # where excel with vtt file names is stored 
baked_vtts <- read_excel("baked_alaska_vtt_fileNames.xlsx") # depends on spokesperson
baked_vtts$file_name <- gsub('\"', "", baked_vtts$file_name, fixed = TRUE) # remove additional "\" when read in 

flw <- vector("list", length(baked_vtts)) 

for(i in 1:14) { 
  setwd("/Users/elizariley") 
  flw[[i]] <- readLines(baked_vtts$file_name[i])
  # remove tags 
  flw[[i]] <- gsub("<.*?>"," ", flw[[i]])
  # trim whitespace 
  flw[[i]] <- trimws(flw[[i]])
  flw[[i]] <- gsub("\\s+", " ", flw[[i]])
  empty_lines = grepl('^\\s*$', flw[[i]])
  flw[[i]] = flw[[i]][! empty_lines]
  # remove align start position 
  line_object <- flw[[i]]
  flw[[i]] <- line_object[-(1:(grep("align:start position", 
                                    line_object)[1]-1))]
  flw[[i]] <- line_object[-grep("align:start position", 
                                line_object)]
  # remove duplicates 
  flw[[i]] <- flw[[i]][-which(duplicated(flw[[i]]))]
  form <- sprintf('baked_alaska_%s.txt', i) 
  setwd("~/Dropbox (MIT)/AltRight/scraping_youtube") 
  write.table(flw[[i]], 
              file = form, 
              sep = " ", 
              row.names = F)
  

library(bib2df)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tm)
library(stringr)

# parse file and separate author names into their parts
df <- bib2df(file = "data/Biblio-Bibtex.bib", separate_names = T)
df$recordID <- rownames(df)
df$recordID <- as.numeric(df$recordID)

#general content analysis

#journals
df_journalNames <- df %>%
  distinct(JOURNAL) %>%
  arrange(desc(JOURNAL))

write.csv(df_journalNames, file = "data/journalNames.csv", row.names = F)

#this file is used to edit journal names to be mostly spelled the same
#journalNamesedited.csv contains hand edited journal names

df_editedJournals <- read.csv("data/journalNamesedited.csv", header = T, quote = "\"")
df_editedJournals$edit <- as.character(df_editedJournals$edit)
number_of_journals <- nrow(distinct(df_editedJournals, edit))

#get records with journals filled in

df_articles <- filter(df, df$CATEGORY == "ARTICLE " & !is.na(df$JOURNAL))

df_articles <- left_join(df_articles, df_editedJournals, by = "JOURNAL")

#number of article per journal
df_numPerJournal <- df_articles %>%
  group_by(edit) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:50)

write.csv(df_numPerJournal, file = "data/articlesPerJournal.csv", row.names = F)


#find duplicates based on title text

df_duplicates <- df

#simplify titles to make them match better

for (row in 1:nrow(df_duplicates)){
  title <- as.character(df_duplicates[row, "TITLE"])
  title <- tolower(title)
  title <- removePunctuation(title)
  title <- stripWhitespace(title)
  df_duplicates[row, "TITLE"] <- title
}

#remove empty titles

df_duplicates <- filter(df_duplicates, !is.na(TITLE))

df_title <- select(df_duplicates, TITLE, recordID)


#build data frame of IDs of duplicates, this takes several days to run

df_dupIDs <- data.frame(matrix(ncol = 2, nrow = 0))
col_names <- c("orig_id", "dup_id")
colnames(df_dupIDs) <- col_names

for (row in 1:nrow(df_title)) {
  title <- as.character(df_title[row, "TITLE"])
  recordID <- as.character(df_title[row, "recordID"])
  
  for (durow in 1:nrow(df_duplicates)){
    duptitle <- as.character(df_duplicates[durow, "TITLE"])
    duprecordID <- as.character(df_duplicates[durow, "recordID"])
    
    if (title == duptitle){
      
      a <- data.frame("org_id" = recordID,
                      "dup_id" = duprecordID)
      
      df_dupIDs <- rbind(df_dupIDs, a)
      
    }
  }
}
write.csv(df_dupIDs, file = "data/rawdupids.csv", row.names = F)

df_dupIDs <- read.csv("data/rawdupids.csv")

df_dup_sortedIDs$orig_id <- as.numeric(df_dupIDs$org_id)
df_dup_sortedIDs$dup_id <- as.numeric(df_dupIDs$dup_id)

df_dup_sortedIDs <- select(df_dup_sortedIDs, orig_id, dup_id)

df_dup_sortedIDs <- filter(df_dup_sortedIDs, orig_id < dup_id)

df_dup_sortedIDs <- distinct(df_dup_sortedIDs)

b <- df_dup_sortedIDs$orig_id
c <- df_dup_sortedIDs$dup_id

d <- c(b, c)

df_dup_all <- data.frame(d)
colnames(df_dup_all) <- c("recordID")
df_dup_all <- distinct(df_dup_all)

df_duplicate_bibs <- inner_join(df, df_dup_all, by = "recordID")

df_duplicate_bibs <- arrange(df_duplicate_bibs, desc(TITLE))

df_duplicate_bibs <- select(df_duplicate_bibs, recordID, TITLE, JOURNAL, YEAR, KEYWORDS)
write.csv(df_duplicate_bibs, file = "data/duplicateBibs.csv", row.names = F)

#author analysis

df_authors <- select(df, recordID, AUTHOR)

#get author last name and first initial for each publication
df_authorlastname <- data.frame(matrix(ncol = 2, nrow = 0))
col_names <- c("recordID", "name")
colnames(df_dupIDs) <- col_names

for (row in 1:nrow(df_authors)) {
  recordID <- as.character(df_authors[row, "recordID"])
  authors <- df_authors[row, "AUTHOR"][[1]][[1]]
  
  
  if(nrow(authors) > 0){
    
    
    for (authorrow in 1:nrow(authors)){
      authname <- paste(authors[authorrow, 4], str_sub(authors[authorrow, 2],1,1), sep = "")
      e <- data.frame("recordID" = recordID,
                      "name" = authname)
      
      df_authorlastname <- rbind(df_authorlastname, e)
      
    }
  }
  
}

write.csv(df_authorlastname, file = "data/authorslastnamefirstinit.csv", row.names = F)

df_authorlastname <- read.csv("data/authorslastnamefirstinit.csv", header = T)
df_authorlastname$name <- removePunctuation(df_authorlastname$name)

df_authorsites <- read.csv("data/LTERpersonnelsites.csv", header = T)

df_authorsites <- distinct(df_authorsites)

write.csv(df_authorsites, file = "data/LTERpersonnlesitesdistinct.csv", row.names = F)

# get pubyear for recordID
df_authoryear <- select(df, recordID, YEAR)

#calculate pubs per year after 1980
df_pubsperyear <- df_authoryear %>% filter(YEAR > 1980) %>% group_by(YEAR) %>% tally()

#add year column and site to authors
df_authoryear$recordID <- as.numeric(df_authoryear$recordID)
df_authorall <- left_join(df_authorlastname, df_authorsites, by = "name")
df_authorall$recordID <- as.numeric(df_authorall$recordID)
df_authorall <- full_join(df_authorall, df_authoryear, by = "recordID")

write.csv(df_authorall, file = "data/authorsiteyear.csv", row.names = F)

#calculate average number of authors per paper per year
df_numauthors <- df_authorall %>% group_by(recordID, YEAR) %>% tally()
df_avgnumauthors <- df_numauthors %>% filter(YEAR > 1980) %>% group_by(YEAR) %>% summarize(avg_num_authors = mean(n))

#take out all authors that didn't get a site ID assigned 
df_authorall_withsite <- filter(df_authorall, !is.na(site))

#calculate number of cross site pubs per year
df_sitesperpaper <- df_authorall_withsite %>% group_by(recordID, YEAR) %>% summarise(numsites = n_distinct(site))
df_crossperyear <- df_sitesperpaper %>% filter(YEAR > 1980) %>% filter(numsites > 1) %>% group_by(YEAR) %>% tally()

#calculate average number sites involved in cross site papers
df_avgsitesperpaper <- df_sitesperpaper %>% filter(YEAR > 1980) %>% filter(numsites > 1) %>% group_by(YEAR) %>% summarise(avgsites = mean(numsites))

#calcualte max number of sites per paper
df_maxsitesperpaper <- df_sitesperpaper %>% filter(YEAR > 1980) %>% filter(numsites > 1) %>% group_by(YEAR) %>% summarise(max_num_sites = max(numsites))

#put all stats together in one table
df_sitestats <- full_join(df_pubsperyear, df_crossperyear, by = "YEAR")
df_sitestats <- full_join(df_sitestats, df_avgsitesperpaper, by = "YEAR")
colnames(df_sitestats) <- c("YEAR", "num_papers", "num_crosspapers", "avg_num_sites")
df_sitestats <- full_join(df_sitestats, df_maxsitesperpaper, by = "YEAR")
df_sitestats <- full_join(df_sitestats, df_avgnumauthors, by = "YEAR")

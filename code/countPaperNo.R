

library(pacman)
p_load(tidyverse,RefManageR,bib2df,tm)


#read bib file

ReadBib("G:\\LTER-network-bibliography-master\\LTER2016.bib") -> lter_16
ReadBib("G:\\LTER-network-bibliography-master\\LTER2017.bib") -> lter_17
ReadBib("G:\\LTER-network-bibliography-master\\LTER2018.bib") -> lter_18

bib2df(file = "G:\\LTER-network-bibliography-master\\LTER-Biblio-Bibtex.bib", 
       separate_names = T) -> lter_before

#convert bib to tibble(data.frame)

lter_16 + lter_17 + lter_18 -> lter_after

as.data.frame(lter_after) %>% as_tibble() -> l_after
as.data.frame(lter_before) %>% as_tibble() -> l_before

#save these variables

save(l_after,l_before,file = "G:/LTER-network-bibliography-master/lter_git.RData")

#load("G:/LTER-network-bibliography-master/lter_git.RData")

#filter columns with NA proportion greater than 50%

l_before %>% 
  select(which(colMeans(is.na(.)) < 0.5)) %>%
  filter(CATEGORY == "ARTICLE ") %>%
  select(JOURNAL,TITLE,YEAR,KEYWORDS)-> l_b

l_after %>%
  filter(bibtype == "Article") %>%
  select(title,journal,year,keywords)-> l_a

#bind all journal articles together

names(l_b) = str_to_lower(names(l_b))  #unify column names
rbind(l_a,l_b) -> lter_all

#simplify the title to find duplicates more easily

lter_all %>%
  mutate(title = removePunctuation(title)) %>%
  mutate(title = str_squish(title)) %>%
  mutate(title = str_to_lower(title)) -> lter_wash_title

#summarize article number each year

lter_wash_title %>%
  count(title,year) %>%
  count(year) %>%
  filter(year>1980) %>%
  rename(n = nn) -> lter_article_no

lter_article_no %>%
  ggplot(aes(x = as.numeric(year),y = n)) +
   geom_point() +
   geom_line() +
   geom_smooth() +
   xlab("year") +
   ylab("No. of LTER Journal Articles\n")

#peaks at 2006 and drops since then,makes no sense
#also,2014 and 2015 could not be used
#done in 2018.10.29,2018 could go up.


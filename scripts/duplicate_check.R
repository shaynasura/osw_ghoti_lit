## Gulf Ecosystem Initiative (GEI) - Offshore Wind Working Group
## Ghoti Article

## Checking for duplicates in the article lists from Web of Science

### Ray did the original literature search in Web of Science on 
### This first search returned 877 hits.

### Shayna did an updated literature search in Web of Science on Monday, February 26, 2024.
### This second search returned 925 hits.



library(tidyverse)
library(synthesisr)

original_search <- read_csv("literature_lists/ray_ghoti_list_original.csv")

updated_search <- read_csv("literature_lists/shayna_ghoti_list_26Feb2024.csv")

ghoti_sheet <- read_csv("literature_lists/ghoti_lit_review.csv")



## check that our original search file matches the articles in our ghoti lit search google sheet.
ghoti_v_original <- anti_join(ghoti_sheet, original_search, by = 'Article Title')
original_v_ghoti <- anti_join(original_search, ghoti_sheet, by = 'Article Title')


## How many new hits did we get between these 2 searches? -> 48 new hits
925 - 877


## first checking for duplicates in our original search
orig_dups_1 <- find_duplicates(original_search$`Article Title`,
                             method = "exact",                  #note there are different methods for detecting duplicates, too.
                             to_lower = TRUE,
                             rm_punctuation = TRUE)

orig_unique <- extract_unique_references(original_search,
                                         matches = orig_dups_1,
                                         type = "select")

orig_dups_1_list <- review_duplicates(original_search$'Article Title', matches = orig_dups_1)
print(orig_dups_1_list) # these are the duplicates in our original search based on exact matches of titles - 9 duplicates.

original_search[18,12]

#### check for duplicates in our original search based on less strict matching...[TO DO]


## second: checking for duplicates in our updated search
updated_dups_1 <- find_duplicates(updated_search$`Article Title`,
                               method = "exact",                  #note there are different methods for detecting duplicates, too.
                               to_lower = TRUE,
                               rm_punctuation = TRUE)

updated_unique <- extract_unique_references(updated_search,
                                         matches = updated_dups_1,
                                         type = "select")
## 916 updated unique records

updated_dups_1_list <- review_duplicates(updated_search$'Article Title', matches = updated_dups_1)
print(updated_dups_1_list) # these are the duplicates in our updated search based on exact matches of titles - 9 duplicates.





## Ray's way of comparing our original search list to our update search list using anti-join in dplyr

new_records <- anti_join(updated_search,original_search, by = 'Article Title')      # this return 99 records, which is too many.

#check for records in our original search that are NOT in our updated search...
oddball_records <- anti_join(original_search, updated_search, by = 'Article Title')    # this returns 50 records in our original search, but not in our updated search
oddball_records_2 <- anti_join(clean_original_search, clean_updated_search, by = "Article_Title")    # also returns 50 records.

# the 50 records returned from our original search mostly seem to be Proceedings Papers...
oddball_records$`Document Type`

### Okay, so Ray and I realized that our original search included 'Meetings' records instead of 'Dissertation/Theses' records.
### Using our search terms and only keeping those records for 'Articles', 'Review Articles', 'Other', and 'Meetings' produced 881 records on February 26, 2024.
###           This 881 records is MUCH closer to our 877 records from our original search.
### Using our search terms and only keeping those records for 'Articles', 'Review Articles', 'Other', and 'Dissertation/Theses' produced 925 records on Feb. 26, 2024.

### So to combine and update our literature search lists, we need to remove those articles in our original search that DO NOT show up in our updated search. Then we need to check our list for duplicates, and then append our already screened information to our final, updated list.







## Need to do some cleaning of the bibliographic files to better test for matches...
### change article titles to all lower case
### remove punctuation and special characters from article titles
clean_original_search <- original_search %>% 
  mutate(Article_Title = tolower(gsub("[[:punct:]]", " ", original_search$'Article Title'))) %>% 
  relocate(Article_Title, .before = 13)

clean_updated_search <- updated_search %>% 
  mutate(Article_Title = tolower(gsub("[[:punct:]]", " ", updated_search$'Article Title'))) %>% 
  relocate(Article_Title, .before = 13)

clean_new_records <- anti_join(clean_updated_search, clean_original_search, by = "Article_Title")    # huh - this still returns 99 records, which is too many.



## checking for duplicates in the above...
clean_dups_1 <- find_duplicates(clean_new_records$Article_Title,
                               method = "exact",                  #note there are different methods for detecting duplicates, too.
                               to_lower = TRUE,
                               rm_punctuation = TRUE)

clean_dups_unique <- extract_unique_references(clean_new_records,
                                         matches = clean_dups_1,
                                         type = "select")

clean_dups_1_list <- review_duplicates(clean_new_records$Article_Title, matches = clean_dups_1)
print(clean_dups_1_list)     # still only detecting 1 duplicate in this set of 99 hits





## these duplicate functions require a single data frame to detect duplicates within. Need to combine 2 dataframes.

both_searches <- rbind(original_search, updated_search)

both_dups_1 <- find_duplicates(both_searches$`Article Title`,
                        method = "string_osa",
                        to_lower = TRUE,
                        rm_punctuation = TRUE)

both_unique <- extract_unique_references(both_searches, matches = both_dups_1, type = "select")
both_dups_1_list <- review_duplicates(both_searches$`Article Title`, matches = both_dups_1)
print(head(both_dups_1_list))


## The number next to the title in the list of matches - does it correspond to the row number in the 'both_searches' dataframe? Checking the first 2 matches listed, which have a #1 and #831 next to their title, respectively.

both_searches[831,]
both_searches[1,12]
both_searches[831,12]

# apparently not....gah. How can we identify all the duplicates (both the original and the copies / duplicates) and remove them??? How can we remove anything that is detected as having a duplicate??

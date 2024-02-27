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
print(orig_dups_1_list) # these are the duplicates in our original search based on exact matches of titles


#### check for duplicates in our original search based on less strict matching...[TO DO]




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

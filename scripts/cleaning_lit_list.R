# Gulf Ecosystem Initiative (GEI) - Offshore Wind Working Group
# Ghoti Article



### Ray did the original literature search in Web of Science on February 1, 2024 around 5:20 pm.
### This first search returned 877 hits.

### Shayna did an updated literature search in Web of Science on Monday, February 26, 2024.
### This second search returned 925 hits.



### This script will do the following:
###       1. remove items in our original list that do NOT appear in our updated list (meeting papers versus dissertations).
###       2. combine our original and updated lists.
###       3. clean our combined article lists of duplicates.
###       4. attach our screening info from our ghoti google sheet to our final, updated list of records.
###       5. export ghoti_updated_info dataframe to CSV file to upload to Google Sheets


library(tidyverse)
library(synthesisr)

original_search <- read_csv("literature_lists/ray_ghoti_list_original.csv")
updated_search <- read_csv("literature_lists/shayna_ghoti_list_26Feb2024.csv")
ghoti_sheet <- read_csv("literature_lists/ghoti_lit_review_28Feb2024.csv")



## 1. Find records in original_search that are also present in our updated_search - this removes the 'Meetings' records included in original_search

matching_records <- original_search[original_search$`Article Title` %in% updated_search$`Article Title`, ]

## 2. Combine our original_search and updated_search dataframes of records

combined_records <- rbind(updated_search, matching_records)
# 827+925


## 3. Remove duplicates from our combined_records list to get a final list without duplicates

combined_duplicates <- find_duplicates(combined_records$`Article Title`,
                                       method = "exact",
                                       to_lower = TRUE,
                                       rm_punctuation = TRUE)

combined_unique <- extract_unique_references(combined_records,
                                             matches = combined_duplicates,
                                             type = "select")
## 916 unique records.
## original search had 877 - 9 duplicates - 50 meetings = 818 records.
## updated search had 925 - 9 duplicates = 916 unique records.

# option to review all the duplicate records, if desired.
combined_dups_list <- review_duplicates(combined_records$`Article Title`, matches = combined_duplicates)
print(combined_dups_list)


## 4. Attach our screening info from our ghoti google sheet to our final, updated, combined list of records.

# trim our ghoti file to only include article title and our screening info
ghoti_trimmed <- ghoti_sheet %>% 
  select(c('Article Title', 13:25))

# need to also remove duplicates from our ghoti file because we have duplicates in there.
ghoti_duplicates <- find_duplicates(ghoti_trimmed$`Article Title`,
                                    method = "exact",
                                    to_lower = TRUE,
                                    rm_punctuation = TRUE)
ghoti_unique <- extract_unique_references(ghoti_trimmed,
                                             matches = ghoti_duplicates,
                                             type = "select")
ghoti_dups_list <- review_duplicates(ghoti_trimmed$`Article Title`, matches = ghoti_duplicates)
print(ghoti_dups_list)


# combine our screened info to final combined list of records - 916 final records
ghoti_updated_info <- left_join(combined_unique, ghoti_unique, by = 'Article Title')

tail(ghoti_updated_info)

# trimming our final ghoti_updated_info to only include those columns we currently have on our google sheet

ghoti_updated_trimmed <- ghoti_updated_info %>% 
  select(c("Publication Type",
           "Authors",
           "Article Title",
           "Source Title",
           "Volume",
           "Issue",
           "Start Page",
           "End Page",
           "DOI",
           "Publication Date",
           "Publication Year",
           "Abstract",
           81:93))

# # double checking for any duplicates. None found anymore.
# ghoti_final_duplicates <- find_duplicates(ghoti_updated_info$`Article Title`,
#                                           method = "exact",
#                                           to_lower = TRUE,
#                                           rm_punctuation = TRUE)
# 
# ghoti_dups_list <- review_duplicates(ghoti_updated_info$`Article Title`, matches = ghoti_final_duplicates)
# print(ghoti_dups_list)


### 5. Export ghoti_updated_info dataframe to CSV file to upload to Google Sheets

ghoti_final_records <- write_csv(ghoti_updated_info, "ghoti_final_records.csv", na = "")
ghoti_final_trimmed <- write_csv(ghoti_updated_trimmed, "ghoti_final_trimmed.csv", na = "")


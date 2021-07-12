library(stringdist)
library(dplyr)
setwd("~/GitHub/cchsflow")
test_different <- read.csv("tests/testdata/test_different.csv", header = FALSE, fileEncoding="UTF-8-BOM")
test_similar <- read.csv("tests/testdata/test_similar.csv", header = FALSE, fileEncoding="UTF-8-BOM")

# Loop through all string combinations in a dataframe containing 1 column of strings to be compared
loop_distance <- function(test_data)
{
  i <- 1
  k <- 1
  while (i <= nrow(test_data)) {
    str1 <- test_data[i,1]
    nchar1 <- nchar(str1)
    j <- i+1
    while (j <= nrow(test_data)) {
      str2 <- test_data[j,1]
      nchar2 <- nchar(str2)
      if (nchar1 > 0 && nchar2 > 0) {
        dist <- stringdist(str1, str2, method = "dl")
        max_chars <- max(nchar1, nchar2)
        similarity <- 1 - (dist/max_chars)
        if (k == 1) {
          summary_table <- data.frame(dist, str1, str2, similarity)
        } else {
          summary_table[k,1] <- dist
          summary_table[k,2] <- str1
          summary_table[k,3] <- str2
          summary_table[k,4] <- similarity
        }
        k <- k+1
      }
      j <- j+1
    }
    i <- i+1   
  }
  return (summary_table)
}

# Test for different strings and save
dif_strings_table <- loop_distance(test_different)
write.csv(dif_strings_table,"tests/testdata/question_diff_string_dist_summary.csv")

# Test for similar strings; need to only test across each row
l <- 1
while (l <= nrow(test_similar)) {
  sim_df <- t(test_similar[l,])
  row_strings_table <- loop_distance(sim_df)
  if (l == 1) {
    sim_strings_table <- row_strings_table
  } else {
    sim_strings_table <- bind_rows(sim_strings_table, row_strings_table)
  }
  l <- l+1
}
write.csv(sim_strings_table,"tests/testdata/question_sim_string_dist_summary.csv")

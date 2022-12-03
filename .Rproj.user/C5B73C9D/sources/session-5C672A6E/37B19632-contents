library(dplyr)

## PART 1: Finding the elf with most calories

calories <- readLines("data/01-day.txt")

cl <- data.frame(calories = as.numeric(calories),
                    rowname = 0)

p <- 0
for(i in seq(nrow(cl))) {
  p <- ifelse(is.na(cl$calories[i]), 
                          p + 1, 
                          p)
  cl$rowname[i] <- p
}


cl %>% 
  filter(!is.na(calories)) %>% 
  group_by(rowname) %>% 
  summarise(sum = sum(calories)) %>% 
  slice_max(order_by = sum, n = 1)


## PART 2: How much calories in total do the top 3 have? 
cl %>% 
  filter(!is.na(calories)) %>% 
  group_by(rowname) %>% 
  summarise(sum = sum(calories)) %>% 
  slice_max(order_by = sum, n = 3) %>% 
  summarise(sum_all = sum(sum))


# Load libraries, data ----------------------------------------------------

library(stringr)
library(dplyr)

input <- read.delim("03-input.txt", header = FALSE)


# Part 1 ------------------------------------------------------------------



# split string to two equal parts
input <- input %>% 
  mutate(part1 = str_sub(V1, start = 1, end = nchar(V1)/2),
         part2 = str_sub(V1, start = nchar(V1)/2 + 1, end = -1))

# make function that will find common elements in vectors
matchwords <- function(x, y) {
  v1 <- unlist(str_split(x, ""))
  v2 <- unlist(str_split(y, ""))
  unique(v1[v1 %in% v2])
}


# add a column with the common element
for (i in 1:nrow(input)) {
  input$overlap[i] <- matchwords(input$part1[i], input$part2[i])
}

# make df of priorities
priority <- data.frame(x = c(letters, LETTERS), 
                       y = seq(1:52))

# get total score
input %>%
  inner_join(priority, by = c("overlap" = "x")) %>% 
  summarise(sum = sum(y)) 



# PART 2 ------------------------------------------------------------------

input <- input %>% 
  # make groups of three
  mutate(group = rep(1:nrow(input), each = 3, length.out = nrow(input)))


output <- vector()
for (i in unique(input$group)) {
  df <- filter(input, group == i)
  v1 <- unlist(str_split(df$V1[1], ""))
  v2 <- unlist(str_split(df$V1[2], ""))
  v3 <- unlist(str_split(df$V1[3], ""))
  output[i] <- Reduce(intersect, list(v1, v2, v3))
}



data.frame(output = output) %>% 
  inner_join(priority, by = c("output" = "x")) %>% 
  summarise(total = sum(y))

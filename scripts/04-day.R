
# Load libraries & data ---------------------------------------------------

library(dplyr)
library(stringr)
library(purrr)

input <- read.delim("./data/04-input.txt", header = FALSE)


# Part 1: Find identical --------------------------------------------------

split <-  str_split(input$V1, ",", simplify = TRUE) 


extract_range <- function(x) {
  seq(str_extract(x, "\\d+"), str_extract(x, "\\d+$"))
}

vector1 <- lapply(split[,1], extract_range)
vector2 <- lapply(split[,2], extract_range)


find_identical <- function(x,y) {
  identical(intersect(x, y), x) || identical(intersect(x,y), y)
}

map2_dbl(vector1, vector2, 
     find_identical) %>% 
  sum()



# Part 2: Overlap ---------------------------------------------------------

map2_dbl(vector1, vector2, 
         ~length(intersect(.x, .y)) > 0) %>% 
  sum()

# Appreciative of your help yesterday, one Elf gives you an encrypted strategy guide
# (your puzzle input) that they say will be sure to help you win. 
# "The first column is what your opponent is going to play:
#   A for Rock, 
#   B for Paper, and 
#   C for Scissors. 
#   
# The second column, you reason, must be what you should play in response: 
#   X for Rock, 
#   Y for Paper, and 
#   Z for Scissors. 
#   
# Your total score is the sum of your scores for each round. 
# The score for a single round is the score for the shape you selected 
# (1 for Rock, 
# 2 for Paper, and 
# 3 for Scissors) 
# 
# plus the score for the outcome of the round 
# (0 if you lost, 
# 3 if the round was a draw, and 
# 6 if you won).


# PART 1 ------------------------------------------------------------------

library(readr)
library(forcars)
library(dplyr)

input <- read_delim("data/02-input.txt", 
                    col_names = FALSE, 
                    col_types = "ff")


input <- input %>% 
  mutate(X1 = fct_recode(X1, 
                         "Rock" = "A",
                         "Paper" = "B",
                         "Scissors" = "C"),
         X2 = fct_recode(X2,
                         "Rock" = "X",
                         "Paper" = "Y",
                         "Scissors" = "Z")
         )

input <- input %>% 
  mutate(win = case_when(
    # draws
    X1 == X2 ~ 3, 
    # wins
    X1 == "Rock" & X2 == "Paper" ~ 6, 
    X1 == "Paper" & X2 == "Scissors" ~ 6,
    X1 == "Scissors" & X2 == "Rock" ~ 6,
    # losses
    TRUE ~ 0
  ),
  style = case_when(
    X2 == "Rock" ~ 1,
    X2 == "Paper" ~ 2,
    X2 == "Scissors" ~ 3
  ),
  total = win + style) 

sum(input$total)



# PART 2 ------------------------------------------------------------------


# The Elf finishes helping with the tent and sneaks back over to you.
# "Anyway, the second column says how the round needs to end: 
# X means you need to lose, 
# Y means you need to end the round in a draw,
# and Z means you need to win. Good luck!"
# 
# The total score is still calculated in the same way, 
# but now you need to figure out what shape to choose so the round ends as indicated. 
# The example above now goes like this:
#   
# In the first round, your opponent will choose Rock 
# - (A), and you need the round to end in a draw (Y), so you also choose Rock. 
#   This gives you a score of 1 + 3 = 4.
#   
# - In the second round, your opponent will choose Paper (B),
#   and you choose Rock so you lose (X) with a score of 1 + 0 = 1.
#   
# - In the third round, you will defeat your opponent's Scissors with Rock 
#   for a score of 1 + 6 = 7.
#   
# Now that you're correctly decrypting the ultra top secret strategy guide,
# you would get a total score of 12.


input %>% 
  select(-c(style, total, win)) %>% 
  mutate(end_needed = case_when(
                        # recode the recoded from part 1 (not the most elegant...)
                         X2 == "Rock" ~ 0,
                         X2 == "Paper" ~ 3,
                         X2 == "Scissors" ~ 6),
         style_needed = case_when(
           # draws
           end_needed == 3 ~ as.character(X1), # add as char as otherwise case_when fails
                                               # would have to specify each product as fct otherwise
           # losses
           X1 == "Rock" & end_needed == 0 ~ "Scissors", 
           X1 == "Paper" & end_needed == 0 ~ "Rock",
           X1 == "Scissors" & end_needed == 0 ~ "Paper",
           # wins
           X1 == "Rock" & end_needed == 6 ~ "Paper",
           X1 == "Paper" & end_needed == 6 ~ "Scissors",
           X1 == "Scissors" & end_needed == 6 ~ "Rock"
         ),
         style = case_when(
           # give points per style
           style_needed == "Rock" ~ 1,
           style_needed == "Paper" ~ 2,
           style_needed == "Scissors" ~ 3
         ),
         total = style + end_needed) %>%  
  summarise(sum = sum(total))
  
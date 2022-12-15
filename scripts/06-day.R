input <- readLines("./data/05-input.txt")


input

letters <- unlist(str_split(input, ""))


# Part 1 ------------------------------------------------------------------



for (i in seq_along(letters)) {
  if(length(unique(c(letters[i], letters[i+1], letters[i+2], letters[i + 3]))) == 4) {
    print(i + 3)
    break
  } else {
    next
  }
}


# Part 2 ------------------------------------------------------------------



for (i in seq_along(letters)) {
  if(length(unique((letters[i:(i+13)]))) == 14) {
    print(i + 13)
    break
  } else {
    next
  }
}


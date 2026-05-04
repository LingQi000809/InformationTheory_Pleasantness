library(ppm)
library(humdrumR)
library(dplyr)
library(egg)

# setwd("information_theory")


extract_seq_from_krn <- function(krn_file_path, column = 'V1') {
  # Read chorale files and extract Harmony spine
  chorales <- readHumdrum(krn_file_path) 
  df <- as.data.frame(chorales)
  raw_tokens <- df[[column]]
  
  tokens <- raw_tokens[sapply(raw_tokens, is_data_token)]
  tokens <- sapply(tokens, clean_up_token, USE.NAMES = FALSE)
  
  tokens
}

## Clean up data
is_data_token <- function(tok) {
  if(is.na(tok)) return(FALSE)
  tok <- as.character(tok)
  if(nchar(tok)==0) return(FALSE)
  first <- substring(tok,1,1)
  if(first %in% c("!", "*", "=") ) return(FALSE)
  if(tok == ".") return(FALSE)
  TRUE
}
clean_up_token <- function(tok) {
  tok <- as.character(tok)
  # Remove trailing ";"
  tok <- sub(";$", "", tok)
  tok
}

compare_columns <- function(
  column1, 
  column2,
  train_file_pattern,
  test_file_pattern,
  all_file_pattern
) {
  # build alphabet and ppm
  alphabet1 = sort(unique(
    extract_seq_from_krn(all_file_pattern, column1)
  ))
  alphabet2 = sort(unique(
    extract_seq_from_krn(all_file_pattern, column2)
  ))
  ppm1 <- new_ppm_simple(alphabet_levels = alphabet1)
  ppm2 <- new_ppm_simple(alphabet_levels = alphabet2)
  
  # train
  model_seq(
    ppm1, 
    factor(
      extract_seq_from_krn(train_file_pattern, column1),
      levels = alphabet1
    )
  )
  model_seq(
    ppm2, 
    factor(
      extract_seq_from_krn(train_file_pattern, column2),
      levels = alphabet2
    )
  )
  
  # test
  res1 = model_seq(
    ppm1,
    factor(
      extract_seq_from_krn(test_file_pattern, column1),
      levels = alphabet1
    )
  )
  res2 = model_seq(
    ppm2,
    factor(
      extract_seq_from_krn(test_file_pattern, column2),
      levels = alphabet2
    )
  )
  
  plot(
    res1$information_content,
    xlab = "Position",
    ylab = "Information content (bits)",
    type = "l", col = "blue",
    ylim = c(0, 20)
  )
  points(
    res2$information_content,
    type = "l", col = "red"
  )
  legend(
    "top",
    legend = c(column1, column2),
    fill = c("blue", "red")
  )
  
  
  
  plot(
    res1$entropy,
    xlab = "Position",
    ylab = "Entropy (bits)",
    type = "l", col = "blue",
    ylim = c(2, 12)
  )
  points(
    res2$entropy,
    type = "l", col = "red"
  )
  legend(
    "top",
    legend = c(column1, column2),
    fill = c("blue", "red")
  )
}

# V1 = roman numeral
# V2 = root
# V3 = bass
# V4 = tenor
# V5 = alto
# V6 = soprano
compare_columns(
  'V1', 'V6',
  './bhchorales/chor0.*.krn',
  './bhchorales/chor200.krn',
  './bhchorales/*.krn'
)

# sort(unique(extract_seq_from_krn('./bhchorales/*.krn', 'V1')))
## generate sequence
# model_seq(ppm_harm, seq = 5, generate = TRUE)



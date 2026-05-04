library(ppm)
library(dplyr)
library(purrr)
library(combinat)
library(egg)
library(humdrumR)


setwd("information_theory")

extract_feature_from_krn <- function(
  krn_file_path, 
  feature_column = 'V1', 
  simplify_pitch = FALSE
) {
  # Read chorale files and extract Harmony spine
  chorales <- readHumdrum(krn_file_path) 
  if (simplify_pitch) {
    chorales |> pitch(simple = TRUE) -> chorales
  }
  df <- as.data.frame(chorales)
  raw_tokens <- df[[feature_column]]
  
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


train_ppm <- function(
  train_file_folder, train_file_pattern, column,
  order_bound, simplify_pitch=FALSE
) {
  train_file_path = paste0(train_file_folder, '/', train_file_pattern)
  feature_values = extract_feature_from_krn(train_file_path, column, simplify_pitch=simplify_pitch)
  alphabet = sort(unique(feature_values))
  ppm <- new_ppm_simple(
    alphabet_levels = alphabet,
    order_bound = order_bound
  )
  
  files <- list.files(
    path=train_file_folder,
    pattern=train_file_pattern, 
    full.names=TRUE
  )
  for (file in files) {
    file_feature_values = extract_feature_from_krn(file, column, simplify_pitch=simplify_pitch)
    model_seq(
      ppm, 
      factor(file_feature_values, levels = alphabet)
    )
  }
  
  ppm
}

group_by_tail <- function(seq_df_list, n_tail = 1) {
  # Create a key for each tibble based on the last n_tail rows
  keys <- map_chr(seq_df_list, function(seq_df) {
    tail_df <- tail(seq_df, n_tail) %>% 
      select(information_content, entropy)
    # Collapse into one string key
    paste0(
      apply(round(tail_df, 1), 1, paste, collapse = "_"),
      collapse = "|"
    )
  })
  
  # Split list of tibbles by their keys
  idx_groups <- split(seq_along(seq_df_list), keys)
  idx_groups <- idx_groups[lengths(idx_groups) > 1]
  
  # Deduplicate by symbol sequence within each group
  idx_groups <- map(idx_groups, function(indices) {
    symbols <- map_chr(indices, function(i) {
      paste(tail(seq_df_list[[i]]$symbol, n_tail), collapse = "|")
    })
    # keep only first occurrence of each unique symbol-tail
    indices[!duplicated(symbols)]
  })
  
  idx_groups[lengths(idx_groups) > 1]
}


############################
# Seq Generation Variables #
############################

train_order_bound = 4
seq_length = 8
num_seq_to_generate = 10000

# train PPM with bhchorales
ppm <- train_ppm(
  './bhchorales', '*.krn', 'V6', train_order_bound, simplify_pitch=TRUE
)
# generate sequences
all_seq_df <- replicate(
  n = num_seq_to_generate,
  # use own generation algorithm
  expr = model_seq(ppm, seq = seq_length, train = FALSE, generate = TRUE),
  simplify = FALSE
)

# # disregard all seq that have equally unlikely distribution
# valid_idx <- which(map_lgl(all_seq_df, function(df) {
#   last_row <- tail(df, 1)
#   (last_row$information_content < 5) | (last_row$entropy < 5)
# }))
# all_seq_df <- all_seq_df[valid_idx]

##########################
# Seq Analysis Variables #
##########################
n_tail_to_compare = 2

same_tail_idx_grouped <- group_by_tail(all_seq_df, n_tail=n_tail_to_compare)
max_len <- max(lengths(same_tail_idx_grouped))
keys <- names(same_tail_idx_grouped)[lengths(same_tail_idx_grouped) == max_len]

examine_seqs_with_key <- function(key_idx) {
  for (idx in same_tail_idx_grouped[[keys[key_idx]]]) {
    print(all_seq_df[[idx]])
    cat("\n---\n")
  }
}
key_idx_to_examine <- 3
examine_seqs_with_key(key_idx_to_examine)
sprintf(
  "Found %.0f sequences with different tokens and the same %.0f-ending IC/entropy values. Max length = %.0f; Example IC_entropy = %s. Order bound = %.0f.", 
  length(same_tail_idx_grouped),
  n_tail_to_compare,
  max_len, 
  keys[[key_idx_to_examine]],
  train_order_bound
) 



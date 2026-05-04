# Imports & Setup -----------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(data.table)

setwd("/Users/ling/Desktop/GT/InformationTheory_Pleasantness/information_theory")

original_files <- list.files("swapchord_results", pattern = "_original.csv", full.names = TRUE)
swap_files <- list.files("swapchord_dedupe", pattern = "_comparisons.csv", full.names = TRUE)

# Analyze Distribution --------------------------------------------------

analyze_corpus_distribution <- function() {
  all_chords <- data.frame()
  for (file in original_files) {
    song_data <- read.csv(file)
    song_data$song <- sub(".*/(.*)_original\\.csv", "\\1", file)
    all_chords <- bind_rows(all_chords, song_data)
  }

  summary_df <- data.frame(
    Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
    Entropy = as.numeric(summary(all_chords$Entropy)[c(1,2,3,4,5,6)]),
    Information_Content = as.numeric(summary(all_chords$Information_content)[c(1,2,3,4,5,6)])
  )
  cat("Summary for original corpus:\n")
  print(summary_df)

  # Plot combined scatter plot of Entropy vs. IC
  plot <- ggplot(all_chords, aes(x = Entropy, y = Information_content)) +
    geom_point(alpha = 0.5, size = 1) +
    labs(
      title = "Original Corpus: Entropy vs. IC",
      x = "Entropy (bits)",
      y = "Information Content (bits)"
    ) +
    theme_minimal() +
    xlim(0, 6) + ylim(0, 10)
  print(plot)

  summary_df
}

analyze_swapped_chords_distribution <- function() {
  all_swapped <- data.frame()

  for (file in swap_files) {
    comp_data <- read.csv(file, stringsAsFactors = FALSE)
    comp_data$song <- sub(".*/(.*)_comparisons\\.csv", "\\1", file)
    # Filter rows where Row == "dist" (swapped chords)
    swapped_rows <- comp_data %>% filter(!is.na(Distance))
    all_swapped <- bind_rows(all_swapped, swapped_rows)
  }

  summary_df <- data.frame(
    Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
    Entropy = as.numeric(summary(all_swapped$Swap_Entropy)[c(1,2,3,4,5,6)]),
    Information_Content = as.numeric(summary(all_swapped$Swap_IC)[c(1,2,3,4,5,6)])
  )

  cat("Summary for swapped chords:\n")
  print(summary_df)

  plot <- ggplot(all_swapped, aes(x = Swap_Entropy, y = Swap_IC)) +
    geom_point(alpha = 0.5, size = 1, color = "blue") +
    labs(
      title = "Swapped Chords: Entropy vs. IC",
      x = "Entropy (bits)",
      y = "Information Content (bits)"
    ) +
    theme_minimal() +
    xlim(0, 6) + ylim(0, 10)
  print(plot)

  # --- Histogram: Entropy Distribution ---
  plot_entropy_hist <- ggplot(all_swapped, aes(x = Orig_Entropy)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(
      title = "Distribution of Entropy for Swappable Pairs",
      x = "Entropy (bits)",
      y = "Number of swappable pairs"
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    theme_minimal()
  print(plot_entropy_hist)

  # --- Histogram: Information Content Distribution ---
  plot_ic_hist <- ggplot(all_swapped, aes(x = Orig_IC)) +
    geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(
      title = "Distribution of Information Content for Swappable Pairs",
      x = "Information Content (bits)",
      y = "Number of swappable pairs"
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    theme_minimal()
  print(plot_ic_hist)

  summary_df
}


analyze_orig_chords_distribution <- function() {
  all_swapped <- data.frame()

  for (file in swap_files) {
    comp_data <- read.csv(file, stringsAsFactors = FALSE)
    comp_data$song <- sub(".*/(.*)_comparisons\\.csv", "\\1", file)
    # Filter rows where Row == "dist" (swapped chords)
    swapped_rows <- comp_data %>% filter(!is.na(Distance))
    all_swapped <- bind_rows(all_swapped, swapped_rows)
  }

  summary_df <- data.frame(
    Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
    Entropy = as.numeric(summary(all_swapped$Orig_Entropy)[c(1,2,3,4,5,6)]),
    Information_Content = as.numeric(summary(all_swapped$Orig_IC)[c(1,2,3,4,5,6)])
  )

  cat("Summary for swapped chords:\n")
  print(summary_df)

  plot <- ggplot(all_swapped, aes(x = Orig_Entropy, y = Orig_IC)) +
    geom_point(alpha = 0.5, size = 1, color = "red") +
    labs(
      title = "Original Chords at swap position: Entropy vs. IC",
      x = "Entropy (bits)",
      y = "Information Content (bits)"
    ) +
    theme_minimal()
  print(plot)

  summary_df
}


# Run the analysis
corpus_summary = analyze_corpus_distribution()
swap_summary = analyze_swapped_chords_distribution()
orig_summary = analyze_orig_chords_distribution()



# Extra Analysis ----------------------------------------------------------
analyze_low_ic <- function(head) {
  all_chords <- data.frame()
  for (i in seq_along(original_files)) {
    file <- original_files[i]
    song_data <- read.csv(file)
    song_data$song <- sub(".*/(.*)_original\\.csv", "\\1", file)
    song_data$file_index <- i  # <-- add file index here
    all_chords <- bind_rows(all_chords, song_data)
  }

  all_chords$Information_content <- as.numeric(all_chords$Information_content)
  lowest_ic <- all_chords[order(all_chords$Information_content), ]

  lowest_ic <- lowest_ic[!duplicated(lowest_ic[, c("Symbol", "Information_content", "Entropy")]), ]

  lowest_ic_head <- head(lowest_ic, head)

  # add a step to dedupe

  write.csv(lowest_ic_head, "lowest_ic.csv", row.names = FALSE)
}
analyze_low_ic(5000)


# Divide Labels --------------------------------------------------

get_chord_function <- function(roman_numeral_symbol) {
  # Normalize input
  sym <- gsub("[#o+-]", "", roman_numeral_symbol)  # remove accidentals and qualifiers
  sym <- toupper(sym)

  # Define groups
  tonic_roots <- c("I", "III", "VI")
  subdom_roots <- c("II", "IV")
  dom_roots <- c("V", "VII")

  if (sym %in% tonic_roots) {
    return("T")
  } else if (sym %in% subdom_roots) {
    return("S")
  } else if (sym %in% dom_roots) {
    return("D")
  } else {
    return("")
  }
}

# division into categories
label_quadrants <- function(entropy_threshold, ic_threshold) {
  all_labeled <- data.frame()

  for (file in swap_files) {
    comp_data <- read.csv(file, stringsAsFactors = FALSE)
    comp_data$song <- NA_character_
    comp_data$Label <- NA_character_
    comp_data$Orig_Function <- NA_character_
    comp_data$Swap_Function <- NA_character_

    swapped_idx <- which(!is.na(comp_data$Distance))
    comp_data$song[swapped_idx]  <- sub(".*/(.*)_comparisons\\.csv", "\\1", file)
    # Assign quadrant labels to swapped chords only
    # High Entropy, High IC -> 1
    # Low Entropy, high IC -> 2
    # Low Entropy, Low IC -> 3
    # High Entropy, Low IC -> 4
    comp_data$Label[swapped_idx] <- case_when(
      comp_data$Orig_Entropy[swapped_idx] >  entropy_threshold & comp_data$Orig_IC[swapped_idx] >  ic_threshold ~ "1",
      comp_data$Orig_Entropy[swapped_idx] <= entropy_threshold & comp_data$Orig_IC[swapped_idx] >  ic_threshold ~ "2",
      comp_data$Orig_Entropy[swapped_idx] <= entropy_threshold & comp_data$Orig_IC[swapped_idx] <= ic_threshold ~ "3",
      comp_data$Orig_Entropy[swapped_idx] >  entropy_threshold & comp_data$Orig_IC[swapped_idx] <= ic_threshold ~ "4",
      TRUE ~ NA_character_
    )
    comp_data$Orig_Function[swapped_idx] <- sapply(
      comp_data$Orig_Symbol[swapped_idx],
      get_chord_function
    )
    comp_data$Swap_Function[swapped_idx] <- sapply(
      comp_data$Swap_Symbol[swapped_idx],
      get_chord_function
    )

    all_labeled <- bind_rows(
      all_labeled,
      comp_data
    )
  }

  # add a column for sequence index
  all_labeled$Comparison <- NULL
  is_empty <- apply(all_labeled, 1, function(row) all(is.na(row) | row == ""))
  all_labeled$Seq <- cumsum(is_empty) + 1
  all_labeled$Seq[is_empty] <- NA
  all_labeled <- all_labeled %>% select(Seq, everything())

  # Write to CSV
  fwrite(all_labeled, "stimuli/combined_swaps_labeled.csv", na = "")
}

label_quadrants(
  corpus_summary$Entropy[corpus_summary$Statistic=="Mean"],
  corpus_summary$Information_Content[corpus_summary$Statistic=="Mean"]
)

split_by_label <- function(combined_file = "stimuli/combined_swaps_labeled.csv") {
  df <- read.csv(combined_file, stringsAsFactors = FALSE)
  all_seqs <- split(df, df$Seq)
  seq_order <- order(sapply(all_seqs, function(seq) seq$Distance[which(!is.na(seq$Distance))[1]]))
  all_seqs <- all_seqs[seq_order]

  # Prepare output lists for each label
  subsets <- list("1" = list(), "2" = list(), "3" = list(), "4" = list())

  for (seq in all_seqs) {
    # Find the label present in this sequence (ignore NA)
    lbl <- na.omit(unique(seq$Label))[1]
    if (lbl %in% c("1", "2", "3", "4")) {
      subset <- subsets[[lbl]]
      subset[[length(subset) + 1]] <- seq
      # Add a single empty row after each sequence
      empty_row <- as.data.frame(lapply(seq[1, ], function(x) NA), stringsAsFactors = FALSE)
      subset[[length(subset) + 1]] <- empty_row
      subsets[[lbl]] <- subset
    }
  }

  # Write each label's file
  for (lbl in c("1", "2", "3", "4")) {
    if (length(subsets[[lbl]]) > 0) {
      df_sub <- do.call(rbind, subsets[[lbl]])
      # Recompute sequence index for this subset
      is_empty <- apply(df_sub, 1, function(row) all(is.na(row) | row == ""))
      df_sub$Seq <- cumsum(is_empty) + 1
      df_sub$Seq[is_empty] <- NA
      fwrite(df_sub, paste0("stimuli/swap_pairs_", lbl, ".csv"), na = "")
      cat(sprintf("There are %d sequence pairs with label %s\n", max(df_sub$Seq, na.rm = TRUE), lbl))
    } else {
      cat(sprintf("There are 0 sequence pairs with label %s\n", lbl))
    }
  }
}
split_by_label()

# Examine Harmonic Functions --------------------------------------------------
plot_swap_distribution <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  df_filtered <- df[which(!is.na(df$Orig_Function) & !is.na(df$Swap_Function)) &
                      df$Orig_Function != "" & df$Swap_Function != "", ]
  df_filtered$Swap_Direction <- paste0(df_filtered$Orig_Function, " -> ", df_filtered$Swap_Function)

  swap_plot <- ggplot(df_filtered, aes(x = Swap_Direction, fill = Swap_Direction)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.3) +  # show counts
    labs(
      title = paste0("Histogram of Swap Directions for ", file_path),
      x = "Swap Direction (Orig -> Swap)",
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  print(swap_plot)
}
plot_swap_distribution("stimuli/combined_swaps_labeled.csv")
for (lbl in c("1", "2", "3", "4")) {
  plot_swap_distribution(paste0("stimuli/swap_pairs_", lbl, ".csv"))
}

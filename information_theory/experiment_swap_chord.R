# Imports & Setup -----------------------------------------------------------------
library(humdrumR)
library(ppm)
library(dplyr)
library(purrr)
library(combinat)
library(egg)
# library(foreach)
# library(doParallel)
setwd("information_theory")

# Preprocessing Functions -----------------------------------------------------------------
cleanupHarm <- function(harm) {
  harm <- gsub("^[0-9%.]+", "", harm)
  harm <- gsub("[bcdefg;]+$", "", harm)
  reduceHarmony(harm)
}

extractFeature <- function(filepath, exclusiveInterpretation) {
  hum <- readHumdrum(filepath)
  hum[[, exclusiveInterpretation]] |> cleanupHarm()
}

# Training Functions -----------------------------------------------------------------
train_ppm <- function(
    all_file_paths,
    test_file_to_exclude,
    exclusiveInterpretation,
    alphabet,
    order_bound=10L
) {
  ppm <- new_ppm_simple(
    alphabet_levels = alphabet,
    order_bound = order_bound
  )
  for (file in all_file_paths) {
    if (file == test_file_to_exclude) {
      next
    }
    file_feature_values = extractFeature(file, exclusiveInterpretation)
    model_seq(
      ppm,
      factor(file_feature_values, levels = alphabet)
    )
  }
  ppm
}


# Test Functions ----------------------------------------------------------
swapChordAtIdx <- function(ppm, seqIdx, featureValues, alphabet) {
  lapply(alphabet, function(chord) {
    newFeatureValues <- featureValues
    newFeatureValues[seqIdx] <- chord
    icTable <- model_seq(
      ppm,
      factor(newFeatureValues, levels = alphabet),
      train = FALSE)
    return(icTable)
  })
}

rankClosest <- function(
  chord_swapped_tables,
  true_chord,
  swapped_idx,
  alphabet
) {
  true_idx <- match(true_chord, alphabet)
  true_table <- chord_swapped_tables[[true_idx]]
  true_ic <- true_table$information_content[[swapped_idx]]
  true_ent <- true_table$entropy[[swapped_idx]]
  true_vals <- c(
    info = true_ic,
    ent  = true_ent
  )

  dists <- sapply(chord_swapped_tables, function(t) {
    vals <- c(
      info = t$information_content[[swapped_idx]],
      ent  = t$entropy[[swapped_idx]]
    )
    sqrt(sum((vals - true_vals)^2))  # Euclidean distance
  })

  # Rank by ascending distance
  ranked_alphabet_idx <- order(dists)
  # remove the true chord
  ranked_alphabet_idx <- ranked_alphabet_idx[ranked_alphabet_idx != true_idx]

  ic_vals <- sapply(ranked_alphabet_idx, function(i) {
    chord_swapped_tables[[i]]$information_content[[swapped_idx]]
  })
  ent_vals <- sapply(ranked_alphabet_idx, function(i) {
    chord_swapped_tables[[i]]$entropy[[swapped_idx]]
  })

  data.frame(
    swapped_idx = swapped_idx,
    alphabet_idx = ranked_alphabet_idx,
    true_chord = true_chord,
    true_ic = true_ic,
    true_ent = true_ent,
    swapped_chord = alphabet[ranked_alphabet_idx],
    swapped_ic = ic_vals,
    swapped_ent = ent_vals,
    dist = dists[ranked_alphabet_idx]
  )
}

# Load data -----------------------------------------------------------------
billboard <- readHumdrum('billboard/*')
exclusiveInterpretation <- '**harm'
alphabet <- extractFeature('billboard/*', exclusiveInterpretation) |> unique()


# Variables -----------------------------------------------------------------
all_file_paths <- list.files(
    path='billboard',
    full.names=TRUE
  )

# Swap Chord Experiment -----------------------------------------------------------------
run_experiment <- function(min_idx, max_idx) {
  # each file takes turn to be the test file
  #foreach(
  #  test_file_idx = min_idx:max_idx,
  #  .export = c("train_ppm", "swapChordAtIdx", "rankClosest",
  #              "cleanupHarm", "extractFeature", "save_print_result",
  #              "exclusiveInterpretation", "alphabet", "all_file_paths"),
  #  .packages = c("humdrumR", "ppm", "dplyr", "purrr")
  #) %dopar% {
  for (test_file_idx in min_idx:max_idx) {
    test_file_path <- all_file_paths[test_file_idx]
    # train data
    ppm <- train_ppm(
      all_file_paths,
      test_file_path,
      exclusiveInterpretation,
      alphabet
    )

    # evaluate test data
    chord_seq <- extractFeature(test_file_path, exclusiveInterpretation)
    true_ic_table <- model_seq(
      ppm,
      factor(chord_seq, levels = alphabet),
      train = FALSE
    )

    # swap all chords after the beginning of the song
    min_swap_idx = 8
    max_swap_idx = length(chord_seq) - 1
    # swap chords
    # all-chord-candidtate IT tables for all swap_idx
    # list of list of df; size = max_swap_idx - min_swap_idx
    # -> all-chord-candidate IT tables for the current swap_idx
    # -> list of df; size = alphabet size
    # ----> alphabet_idx chord-candidate IT table for the current swap_idx
    # ----> df; length = chord sequence length
    swapped_ic_tables <- list()
    # ranked_tables
    # chord-candidtate ranked tables for all swap_idx
    # list of df; size = max_swap_idx - min_swap_idx
    # -> chord-candidate ranked table for the current swap_idx
    # -> df; length = alphabet size
    ranked_tables <- list()

    # swap chords at all swap_idx; rank by distance to true chord
    swap_idx_range <- (min_swap_idx:max_swap_idx)
    for (i in seq_along(swap_idx_range)) {
      swap_idx <- swap_idx_range[i]
      swappedTables <- swapChordAtIdx(
        ppm,
        swap_idx,
        chord_seq,
        alphabet
      )
      swapped_ic_tables[[i]] <- swappedTables

      rankedTable <- rankClosest(
        swappedTables,
        chord_seq[[swap_idx]],
        swap_idx,
        alphabet
      )
      ranked_tables[[i]] <- rankedTable
    }
    # combine ranked table for all swapped idx
    ranked_table_combined <- bind_rows(ranked_tables)
    # filter candidates with low distance
    close_matches <- ranked_table_combined %>%
      filter(
        dist < 99999,
        true_chord != "r",
        swapped_chord != "r"
      ) %>%
      arrange(dist)

    save_print_result(
      close_matches,
      true_ic_table,
      swapped_ic_tables,
      min_swap_idx,
      window = 5,
      outdir = "swapchord_results",
      file_label = tools::file_path_sans_ext(basename(test_file_path))
    )
  }
}

save_print_result <- function(
    close_matches,
    true_ic_table,
    swapped_ic_tables,
    min_swap_idx,
    window = 5,
    outdir = NULL,
    file_label = "testfile"
) {
  if (!is.null(outdir) && !dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }

  all_comparisons <- list()

  for (row_i in seq_len(nrow(close_matches))) {
    match_row <- close_matches[row_i, ]
    swap_idx <- match_row$swapped_idx
    alphabet_idx <- match_row$alphabet_idx

    # original and swapped tables
    orig_tbl <- true_ic_table
    swap_tbl <- swapped_ic_tables[[swap_idx - min_swap_idx + 1]][[alphabet_idx]]

    lo <- max(1, swap_idx - window)
    hi <- min(nrow(orig_tbl), swap_idx + window)

    comp_tbl <- data.frame(
      Comparison   = row_i,
      Row          = lo:hi,
      Orig_Symbol  = orig_tbl$symbol[lo:hi],
      Orig_Order   = orig_tbl$model_order[lo:hi],
      Orig_IC      = orig_tbl$information_content[lo:hi],
      Orig_Entropy = orig_tbl$entropy[lo:hi],
      Swap_Symbol  = swap_tbl$symbol[lo:hi],
      Swap_Order   = swap_tbl$model_order[lo:hi],
      Swap_IC      = swap_tbl$information_content[lo:hi],
      Swap_Entropy = swap_tbl$entropy[lo:hi]
    )

    comp_tbl$Distance <- NA
    comp_tbl$Distance[comp_tbl$Row == swap_idx] <- match_row$dist

    all_comparisons[[row_i]] <- comp_tbl

    # Print to console
    cat("\n==== Match", row_i, "for", file_label,
        "swap_idx:", swap_idx,
        "true:", match_row$true_chord,
        "swapped:", match_row$swapped_chord,
        "dist:", signif(match_row$dist, 4), "====\n")
    print(comp_tbl)
  }

  # Combine into one data.frame with empty rows between
  combined_comparisons <- do.call(rbind, lapply(seq_along(all_comparisons), function(i) {
    df <- all_comparisons[[i]]
    empty_row <- df[1, ] * NA  # make an empty row of NAs
    rbind(df, empty_row)
  }))

  # Save everything once
  if (!is.null(outdir)) {
    comp_file <- file.path(outdir, paste0(file_label, "_comparisons.csv"))
    write.csv(combined_comparisons, comp_file, row.names = FALSE, na = "")

    ranked_file <- file.path(outdir, paste0(file_label, "_ranked.csv"))
    write.csv(close_matches, ranked_file, row.names = FALSE)

    # clean true_ic_table so it can be written
    orig_tbl_clean <- data.frame(
      Row = 1:nrow(true_ic_table),
      Symbol = true_ic_table$symbol,
      Order = true_ic_table$model_order,
      Information_content = true_ic_table$information_content,
      Entropy = true_ic_table$entropy
    )
    orig_file <- file.path(outdir, paste0(file_label, "_original.csv"))
    write.csv(orig_tbl_clean, orig_file, row.names = FALSE)

    # swapped_file <- file.path(outdir, paste0(file_label, "_swapped.rds"))
    # saveRDS(swapped_ic_tables, swapped_file)
  }

  invisible(combined_comparisons)
}

#n_cores <- detectCores()
#cluster <- makeCluster(n_cores - 1)
#registerDoParallel(cluster)

run_experiment(516, 516)

#stopCluster(cl = cluster)


# Examine all swapped IC tables -----------------------------------------------
swapped_ic_tables <- readRDS("swapchord_results/ABBA_Chiquitita_1979.varms_swapped.rds")
print(swapped_ic_tables[[22]][[2]], n=25)


# Deduplication -----------------------------------------------------------
library(digest)
library(data.table)
dedupe_comparison_file <- function(filename) {
  df <- tryCatch(
    read.csv(filename, na.strings = c("", "NA")),
    error = function(e) {
      message(sprintf("Error reading %s: %s", filename, e$message))
      return(NULL)
    }
  )
  if (is.null(df)) {
    # Reading failed; skip further processing
    return(invisible(NULL))
  }
  df <- df[rowSums(!is.na(df)) > 0, ]

  seqs <- split(df, df$Comparison)
  keep_cols <- c("Orig_Symbol", "Swap_Symbol")
  seq_hashes <- vapply(seqs, function(s) {
    content <- as.data.frame(s[, keep_cols, drop = FALSE])
    row.names(content) <- NULL
    digest(content, algo = "xxhash64")
  }, character(1))
  unique_seqs <- seqs[!duplicated(seq_hashes)]
  for (i in seq_along(unique_seqs)) unique_seqs[[i]]$Comparison <- i

  empty_row <- as.data.frame(lapply(df[1, ], function(x) NA), stringsAsFactors = FALSE)
  out_list <- lapply(seq_along(unique_seqs), function(i) {rbind(unique_seqs[[i]], empty_row)})
  out_df <- do.call(rbind, out_list)

  # Write to file
  file_simple_name <- sub(".*/(.*)", "\\1", filename)
  fwrite(out_df, paste0("swapchord_dedupe/", file_simple_name) , na = "")

  print(sprintf("Deduped %s from %d to %d sequences", file_simple_name, length(seqs), length(unique_seqs)))
}

comparison_files <- list.files("swapchord_results", pattern = "_comparisons.csv", full.names = TRUE)
for (filename in comparison_files) {
  dedupe_comparison_file(filename)
}

# dedupe_comparison_file("swapchord_results/ArethaFranklin_ChainOfFools_1967_comparisons.csv")

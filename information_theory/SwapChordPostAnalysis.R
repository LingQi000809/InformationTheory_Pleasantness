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
  # remove duration
  harm <- gsub("^[0-9%.]+", "", harm)
  # remove inversion
  harm <- gsub("[bcdefg;]+$", "", harm)
  # remove extension
  harm <- reduceHarmony(harm)
  harm
}

extractFeature <- function(filepath, exclusiveInterpretation) {
  hum <- readHumdrum(filepath)
  hum[[, exclusiveInterpretation]]
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


# Load data -----------------------------------------------------------------
billboard <- readHumdrum('billboard/*')
exclusiveInterpretation <- '**harm'
alphabet <- extractFeature('billboard/*', exclusiveInterpretation) |>
  unlist() |>
  as.character() |>
  unique()


# Variables -----------------------------------------------------------------
all_file_paths <- list.files(
  path='billboard',
  full.names=TRUE
)
test_file_idx <- 1

test_file_path <- all_file_paths[test_file_idx]
# train data
ppm <- train_ppm(
  all_file_paths,
  test_file_path,
  exclusiveInterpretation,
  alphabet
)
chord_seq <- extractFeature(test_file_path, exclusiveInterpretation)
true_ic_table <- model_seq(
  ppm,
  factor(chord_seq, levels = alphabet),
  train = FALSE
)

true_ic_df <- as.data.frame(true_ic_table)
# Keep only columns that are NOT lists
final_table <- true_ic_df[, sapply(true_ic_df, is.atomic)]
file_name <- paste0(test_file_idx, "_noDuration_noInversion_noExtension.csv")
write.csv(final_table, file_name, row.names = FALSE, na = "")


for(f in c("information_theory/1_noDuration.csv", "information_theory/1_noDuration_noInversion.csv", "information_theory/1_noDuration_noInversion_noExtension.csv")){

  df <- read.csv(f, stringsAsFactors = FALSE)

  # Normalize information_content and entropy
  df <- df %>%
    mutate(
      IC_norm = scale(information_content)[,1],
      Ent_norm = scale(entropy)[,1]
    )

  write.csv(df, f, row.names = FALSE, na = "")

  plot <- ggplot(df, aes(x = entropy, y = information_content)) +
    geom_point(alpha = 0.5, size = 1, color = "blue") +
    labs(
      title = f,
      x = "Entropy (bits)",
      y = "Information Content (bits)"
    ) +
    theme_minimal()
  print(plot)
}

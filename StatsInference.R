library(dplyr)
library(tidyr)
library(readr)
library(glmmTMB)
library(performance)
library(ggeffects)

setwd("/Users/ling/Desktop/GT/InformationTheory_Pleasantness")

# Pre-processing ----------------------------------------------------------


# Load data
ratings_raw <- read.csv("data_analysis/chord_ratings.csv")
excerpt_cov <- read.csv("data_analysis/overall_ratings.csv")


# Remove non-chord rows
ratings <- ratings_raw %>% filter(chord_index >= 0)
ratings$Symbol |> unique()

# Merge excerpt-level covariates (overall pleasantness per participant & trial)
ratings <- ratings %>%
  left_join(excerpt_cov, by = c("response", "trial_id")) %>%
  mutate(
    interaction = IC * Entropy,
    trial_clean = sub("^re", "", trial_id),
    quadrant = sub("_.*", "", trial_clean),
    seq_id = sub("_[OS]$", "", trial_clean),
    version = sub("^.*_", "", trial_clean),
    chord_factor = factor(chord_index)
  )

# Standardize GLOBALLY (across all rows)
ratings_std_all_participants <- ratings %>%
  mutate(
    pleasantness = as.numeric(scale(chord_pleasantness)),
    overall_pleasantness = as.numeric(scale(overall_pleasantness)),
    surprise = as.numeric(scale(IC)),
    uncertainty = as.numeric(scale(Entropy)),
    interaction = as.numeric(scale(IC) * scale(Entropy)),
    SensoryDissonance = as.numeric(scale(SensoryDissonance)),
    SpectralComplexity = as.numeric(scale(SpectralComplexity)),
    SpectralCentroid = as.numeric(scale(SpectralCentroid))
  )
write.csv(ratings_std_all_participants, "data_analysis/ratings_std_all_participants.csv", row.names = FALSE)

# Standardize per participant (response and predictors)
data_std <- ratings %>%
  group_by(response) %>%
  mutate(
    pleasantness = as.numeric(scale(chord_pleasantness)),
    overall_pleasantness = as.numeric(scale(overall_pleasantness)),
    surprise = as.numeric(scale(IC)),
    uncertainty = as.numeric(scale(Entropy)),
    interaction = as.numeric(scale(IC) * scale(Entropy)),
    SensoryDissonance = as.numeric(scale(SensoryDissonance)),
    SpectralComplexity = as.numeric(scale(SpectralComplexity)),
    SpectralCentroid = as.numeric(scale(SpectralCentroid))
  ) %>%
  ungroup()
data_std <- data_std %>%
  mutate(
    trial_clean = sub("^re", "", trial_id),
    quadrant = sub("_.*", "", trial_clean),
    seq_id = sub("_[OS]$", "", trial_clean),
    version = sub("^.*_", "", trial_clean),
    chord_factor = factor(chord_index)
  )

write.csv(data_std, "data_analysis/data_std.csv", row.names = FALSE)

# Average across participants per chord
data_std_avg_subject <- data_std %>%
  group_by(trial_id, chord_index) %>%
  summarise(
    pleasantness = mean(pleasantness, na.rm = TRUE),
    overall_pleasantness = mean(overall_pleasantness, na.rm = TRUE),
    Symbol = first(Symbol),
    Order = first(Order),
    surprise = first(surprise),
    uncertainty = first(uncertainty),
    interaction = first(interaction),
    HarmonicFunction = first(HarmonicFunction),
    quadrant = first(quadrant),
    seq_id = first(seq_id),
    version = first(version),
    SensoryDissonance = first(SensoryDissonance),
    SpectralComplexity = first(SpectralComplexity),
    SpectralCentroid = first(SpectralCentroid),
    chord_factor = first(chord_factor),
    .groups = "drop"
  ) %>%
  group_by(trial_id) %>%
  ungroup()

# Visualization and Examination of Stimuli ----------

##  Plot IC/Ent distribution of chords  -----------
library(ggplot2)
library(dplyr)
# TODO: include repeated measures
ratings_filtered <- ratings_raw %>%
  filter(chord_index >= 0, !grepl("^re", trial_id))

# One row per chord
chords_only <- ratings_filtered %>%
  group_by(trial_id, chord_index) %>%
  summarise(
    IC = mean(IC, na.rm = TRUE),
    Entropy = mean(Entropy, na.rm = TRUE),
    SensoryDissonance = mean(SensoryDissonance, na.rm = TRUE),
    SpectralCentroid = mean(SpectralCentroid, na.rm = TRUE),
    SpectralComplexity = mean(SpectralComplexity, na.rm = TRUE),
    .groups = "drop"
  )

# Scatter plot
plot <- ggplot(chords_only, aes(x = Entropy, y = IC)) +
  geom_point(alpha = 0.7, color = "steelblue", size = 2) +
  labs(
    title = "Stimuli: Entropy vs IC",
    x = "Entropy (bits)",
    y = "Information Content (IC, bits)"
  ) +
  theme_minimal() +
  xlim(0, 6) + ylim(0, 10)
print(plot)

## IC/Ent correlation ---------------
# pairwise Pearson correlation matrix
library(Hmisc)
library(ggplot2)
library(dplyr)

stimuli_features_corr_matrix <- function(stimuli_df) {
  predictors <- stimuli_df %>%
    select(Entropy, IC, SensoryDissonance, SpectralCentroid, SpectralComplexity)
  cor_results <- rcorr(as.matrix(predictors), type = "pearson")
  r_mat <- cor_results$r      # correlation coefficients
  p_mat <- cor_results$P      # p-values

  # Initialize matrix
  r_star_mat <- matrix("", nrow = nrow(r_mat), ncol = ncol(r_mat))
  rownames(r_star_mat) <- rownames(r_mat)
  colnames(r_star_mat) <- colnames(r_mat)

  # Loop over cells
  for(i in 1:nrow(r_mat)) {
    for(j in 1:ncol(r_mat)) {
      if(i == j){
        r_star_mat[i,j] <- "-"
      } else {
        r_val <- round(r_mat[i,j], 2)
        p_val <- p_mat[i,j]

        # Assign stars
        star <- ""
        if(p_val < 0.001) star <- "***"
        else if(p_val < 0.01) star <- "**"
        else if(p_val < 0.05) star <- "*"

        # Only append star if significant
        r_star_mat[i,j] <- paste0(r_val, star)
      }
    }
  }
  library(knitr)
  knitr::kable(r_star_mat, caption = "Pairwise Pearson Correlation Matrix with Significance Stars")
}

### correlation among all stimuli ----
stimuli_features_corr_matrix(chords_only)

### after deduping pair... -----
# Create a pair_id by removing the "_O" or "_S" at the end
ratings_filtered <- ratings_filtered %>%
  mutate(pair_id = sub("_[OS]$", "", trial_id),
         version = sub("^.*_", "", trial_id))  # "O" or "S"
unique_chords <- ratings_filtered %>%
  group_by(pair_id, chord_index) %>%
  summarise(
    IC = mean(IC, na.rm = TRUE),
    Entropy = mean(Entropy, na.rm = TRUE),
    SensoryDissonance = mean(SensoryDissonance, na.rm = TRUE),
    SpectralCentroid = mean(SpectralCentroid, na.rm = TRUE),
    SpectralComplexity = mean(SpectralComplexity, na.rm = TRUE),
    .groups = "drop"
  )
stimuli_features_corr_matrix(unique_chords)


# Analysis 0. Replicating Cheung's LMM Full Model (Initial maximal random-effects) --------------------
original_data_avg_subject <- data_std_avg_subject[grepl("_O$", data_std_avg_subject$trial_id) & !grepl("^re", data_std_avg_subject$trial_id), ]

maximal_model <- glmmTMB(
  pleasantness ~
    uncertainty +
    surprise +
    interaction +
    SensoryDissonance +
    SpectralComplexity +
    SpectralCentroid +
    overall_pleasantness +
    ar1(chord_factor + 0 | trial_id) +
    (1
       + uncertainty
       + surprise
       + interaction
       + SensoryDissonance
       + SpectralComplexity
       + SpectralCentroid
       + overall_pleasantness
     | trial_id),
  # data = original_data_avg_subject,
  data = data_std_avg_subject,
  REML = FALSE
)
summary(maximal_model)

## Stepwise Simplification --------------------------------------
# Remove slopes that
# (1) has very small random-slope variance, which models how much the effect of a predictor varies across groups
# (2) has extreme correlations close to +-1, which suggests over-parameterized slopes that are linearly related to each other.

simp_model <- glmmTMB(
  pleasantness ~
    uncertainty +
    surprise +
    interaction +
    SensoryDissonance +
    SpectralComplexity +
    SpectralCentroid +
    overall_pleasantness +
    ar1(chord_factor + 0 | trial_id) +
    (1
       #+ uncertainty # removed
       #+ surprise
       #+ interaction # removed
       #+ SensoryDissonance # removed
       #+ SpectralComplexity # removed
       #+ SpectralCentroid # removed
       #+ overall_pleasantness # removed
     | trial_id),
  data = data_std_avg_subject,
  # data = data_std_avg_subject,
  REML = FALSE
)

summary(simp_model)


## Null Model --------------------------------------------------

null_model <- glmmTMB(
  pleasantness ~
    SensoryDissonance +
    SpectralComplexity +
    SpectralCentroid +
    overall_pleasantness +
    # ar1(chord_factor + 0 | trial_id) +
    (1
      # + uncertainty
      # + surprise
      + interaction
      # + SensoryDissonance
      # +  SpectralComplexity
      # +  SpectralCentroid
      # +  overall_pleasantness
     | trial_id),
  data = data_std_avg_subject,
  REML = FALSE
)
summary(null_model)



## Likelihood Ratio Test -----------------------------------
anova(null_model, simp_model)

## Individual Fixed Effects

reduced_model <- update(simp_ar_model, . ~ . - interaction)
anova(reduced_model, simp_ar_model)

# Wald confidence intervals
confint(simp_ar_model, method = "Wald")

# Diagnostics -----------------------------------
# Ideally, residuals are randomly scattered around 0 → no heteroscedasticity.
plot(residuals(simp_ar_model))
# QQ plot compares residuals to a normal distribution. Ideally, points lie roughly on the line → residuals ~ normal.
qqnorm(residuals(simp_ar_model))
qqline(residuals(simp_ar_model))


# Analysis 1. All Participants Pleasantness and IC/Ent -----------------------------------------
## LMM Full Model All Parcitipants
lmm_model <- glmmTMB(
  pleasantness ~
    uncertainty +
    surprise +
    interaction +
    SensoryDissonance + SpectralComplexity + SpectralCentroid + overall_pleasantness +
    (1 | response) + # Random intercept per participant
    ar1(chord_factor + 0 | trial_id) + # auto-corrlation between subsequent chords
    (1
     # + uncertainty # 3
     #+ surprise
     # + interaction # 6
     # + SensoryDissonance # 1
     # + SpectralComplexity # 4
     # + SpectralCentroid # 2
     # + overall_pleasantness # 5
     | trial_id),
  data = ratings_std_all_participants,
  REML = FALSE
  # control = glmmTMBControl(
  #  optCtrl = list(iter.max = 1000, eval.max = 1000),
  #  optimizer = nlminb
  #)
)
summary(lmm_model)


## Analysis 1: Null model comparison ---------------------------------------
null_model_1 <- glmmTMB(
  pleasantness ~
    SensoryDissonance + SpectralComplexity + SpectralCentroid + overall_pleasantness +
    (1 | response) +
    ar1(chord_factor + 0 | trial_id) +
    (1 | trial_id),
  data = ratings_std_all_participants,
  REML = FALSE
)

# Full vs null likelihood ratio test
anova(null_model_1, lmm_model)

# Test each predictor of interest against a reduced model
lmm_no_uncertainty  <- update(lmm_model, . ~ . - uncertainty)
lmm_no_surprise     <- update(lmm_model, . ~ . - surprise)
lmm_no_interaction  <- update(lmm_model, . ~ . - interaction)

anova(lmm_no_uncertainty, lmm_model)
anova(lmm_no_surprise,    lmm_model)
anova(lmm_no_interaction, lmm_model)

# Wald 95% confidence intervals
confint(lmm_model, method = "Wald")

# Analysis 2. Only Target Chord Version Comparison ----------------------------------------------------------
data_std$swapped_target <- ifelse(ratings$chord_index == 5, 1, 0)
data_swapped <- subset(data_std, swapped_target == 1)
data_swapped <- subset(data_swapped, !grepl("^re", trial_id))
data_swapped$version <- factor(
  ifelse(grepl("O$", data_swapped$trial_id), "O", "S"),
  levels = c("O", "S")
)
data_swapped$pair_id <- sub("_[OS]$", "", data_swapped$trial_id)

## per qudrant
quadrants <- unique(data_swapped$quadrant)

swap_models <- list()

for(q in quadrants){

  df_q <- subset(data_swapped, quadrant == q)

  model_q <- glmmTMB(
    pleasantness ~ version
    + surprise
    + uncertainty
    + interaction
    + HarmonicFunction
    + SensoryDissonance
    + SpectralComplexity
    + SpectralCentroid
    + overall_pleasantness
    + (1 | response)
    + (1 | pair_id),
    data = df_q,
    REML = FALSE
  )

  cat("\n\n============================\n")
  cat("Quadrant:", q, "\n")
  cat("============================\n")

  print(summary(model_q))

  swap_models[[q]] <- model_q
}

## LMM on version
# TODO: ignore direction of version's correlation?
swap_model <- glmmTMB(
  pleasantness ~ version
    + surprise
    + uncertainty
    + interaction
    + HarmonicFunction
    + SensoryDissonance
    + SpectralComplexity
    + SpectralCentroid
    + overall_pleasantness
    + (1 | response)  # random intercept per participant
    + (1 | pair_id),  # random intercept per swappable pair
  data = data_swapped,
  REML = FALSE
)
summary(swap_model)

# for each quadrant, is version significant or not?
# for high-entropy quadrant, version is significant predictor?

# Null model (without version)
null_swap <- update(swap_model, . ~ . - version)

# Compare them
anova(null_swap, swap_model)


# Analysis 3. Compare Target Chord Absolute Diff with Other Chords--------
data_pairs <- ratings_std_all_participants %>%
  group_by(response, seq_id, chord_index) %>%
  summarise(
    pleasantness_change = abs(chord_pleasantness[version == "S"] - chord_pleasantness[version == "O"]),
    .groups = "drop"
  ) %>%
  mutate(is_target = chord_index == 5,
         chord_factor = factor(chord_index))

swap_model2 <- glmmTMB(
  pleasantness_change ~ is_target +  # main test
    (1 | response) +  # random intercept per participant
    (1 | seq_id) +      # random intercept per pair
    ar1(chord_factor + 0 | seq_id),
  data = data_pairs,
  REML = FALSE
)

summary(swap_model2)

## Analysis 3: Null model comparison ---------------------------------------
null_model_3 <- glmmTMB(
  pleasantness_change ~
    (1 | response) +
    (1 | seq_id) +
    ar1(chord_factor + 0 | seq_id),
  data = data_pairs,
  REML = FALSE
)

# Full vs null likelihood ratio test
anova(null_model_3, swap_model2)

# Test is_target against reduced model
swap_model2_no_target <- update(swap_model2, . ~ . - is_target)
anova(swap_model2_no_target, swap_model2)

# Wald 95% confidence intervals
confint(swap_model2, method = "Wald")


ggplot(data_pairs, aes(x = is_target, y = pleasantness_change)) +

  # Participant lines (behind the violin)
  geom_line(aes(group = response), color = "gray", alpha = 0.3) +

  # Violin + boxplot for distribution
  geom_violin(aes(fill = is_target), alpha = 0.5, width = 0.8) +
  geom_boxplot(aes(fill = is_target), width = 0.2, outlier.shape = NA) +

  # Mean ± SE points
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "black") +

  # Colors and labels
  scale_x_discrete(labels = c("Other Chords", "Target Chord")) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(
    x = "",
    y = "Absolute Pleasantness Difference",
    title = "Pleasantness Divergence: Target swapped chord vs Other Chords"
  ) +
  theme_minimal()

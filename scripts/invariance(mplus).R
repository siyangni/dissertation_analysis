library(MplusAutomation)

# Directory containing the longitudinal invariance Mplus files
invariance_dir <- "mplus/invariance_test"

# Run the three longitudinal invariance models in the correct DIFFTEST order
runModels(file.path(invariance_dir, "configural_invariance_baseline.inp"))
runModels(file.path(invariance_dir, "threshold_invariance.inp"))
runModels(file.path(invariance_dir, "strong_invariance.inp"))

# Read all model outputs in the invariance directory
all_models <- readModels(invariance_dir)

# Restrict to just the three longitudinal invariance models by output filename
model_names <- names(all_models)
keep_index <- grepl(
  "configural_invariance_baseline|threshold_invariance|strong_invariance",
  model_names
)

invariance_models <- all_models[keep_index]

# Extract summaries and attach a model_filename column to each
temp_summaries_list <- Map(
  f = function(model_object, model_name) {
    df <- model_object$summaries
    df$model_filename <- model_name
    df
  },
  model_object = invariance_models,
  model_name = names(invariance_models)
)

# Restrict to columns that are common across all models so rbind works
common_columns <- Reduce(intersect, lapply(temp_summaries_list, names))

invariance_summaries <- do.call(
  rbind,
  lapply(temp_summaries_list, function(df) df[, common_columns, drop = FALSE])
)

# Select key fit indices to summarise the longitudinal invariance test
invariance_table <- invariance_summaries[, c(
  "model_filename",
  "Title",
  "ChiSqM_Value", "ChiSqM_DF", "ChiSqM_PValue",
  "CFI", "TLI",
  "RMSEA_Estimate", "RMSEA_90CI_LB", "RMSEA_90CI_UB"
)]

# Reorder rows: configural, threshold, strong
desired_order <- c(
  "configural_invariance_baseline.out",
  "threshold_invariance.out",
  "strong_invariance.out"
)

invariance_table <- invariance_table[match(desired_order, rownames(invariance_table)), ]

print(invariance_table)

library(MplusAutomation)

# Directory containing the mixture LGCM Mplus files
mixture_lgcm_dir <- "mplus/growth_mixture_models"

## ----------------------------------------------------------------------
## Run 1-, 2-, 3-, and 4-class mixture LGCM models
## ----------------------------------------------------------------------

runModels(file.path(mixture_lgcm_dir, "sc_gmm_lgcm_c1.inp"))
runModels(file.path(mixture_lgcm_dir, "sc_gmm_lgcm_c2.inp"))
runModels(file.path(mixture_lgcm_dir, "sc_gmm_lgcm_c3.inp"))
runModels(file.path(mixture_lgcm_dir, "sc_gmm_lgcm_c4.inp"))

# Read the model outputs
sc_gmm_lgcm_c1 <- readModels(file.path(mixture_lgcm_dir, "sc_gmm_lgcm_c1.out"))
sc_gmm_lgcm_c2 <- readModels(file.path(mixture_lgcm_dir, "sc_gmm_lgcm_c2.out"))
sc_gmm_lgcm_c3 <- readModels(file.path(mixture_lgcm_dir, "sc_gmm_lgcm_c3.out"))
sc_gmm_lgcm_c4 <- readModels(file.path(mixture_lgcm_dir, "sc_gmm_lgcm_c4.out"))

## ----------------------------------------------------------------------
## Compare model fit across 1-, 2-, 3-, and 4-class solutions
## ----------------------------------------------------------------------

# Handle missing entropy for the one-class model (if not reported by Mplus)
entropy_values <- c(
  if (!is.null(sc_gmm_lgcm_c1$summaries$Entropy)) sc_gmm_lgcm_c1$summaries$Entropy else NA_real_,
  sc_gmm_lgcm_c2$summaries$Entropy,
  sc_gmm_lgcm_c3$summaries$Entropy,
  sc_gmm_lgcm_c4$summaries$Entropy
)

fit_comparison <- data.frame(
  classes = c(1, 2, 3, 4),
  logLik  = c(sc_gmm_lgcm_c1$summaries$LL,
              sc_gmm_lgcm_c2$summaries$LL,
              sc_gmm_lgcm_c3$summaries$LL,
              sc_gmm_lgcm_c4$summaries$LL),
  AIC     = c(sc_gmm_lgcm_c1$summaries$AIC,
              sc_gmm_lgcm_c2$summaries$AIC,
              sc_gmm_lgcm_c3$summaries$AIC,
              sc_gmm_lgcm_c4$summaries$AIC),
  BIC     = c(sc_gmm_lgcm_c1$summaries$BIC,
              sc_gmm_lgcm_c2$summaries$BIC,
              sc_gmm_lgcm_c3$summaries$BIC,
              sc_gmm_lgcm_c4$summaries$BIC),
  aBIC    = c(sc_gmm_lgcm_c1$summaries$aBIC,
              sc_gmm_lgcm_c2$summaries$aBIC,
              sc_gmm_lgcm_c3$summaries$aBIC,
              sc_gmm_lgcm_c4$summaries$aBIC),
  entropy = entropy_values
)

print(fit_comparison)

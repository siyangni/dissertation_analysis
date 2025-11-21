library(MplusAutomation)

# Run the models
runModels(target = "mplus/invariance_test/threshold_invariance.inp")
conf_invariance <- readModels("mplus/invariance_test/threshold_invariance.out")
print(conf_invariance$summaries)
head(conf_invariance$parameters)

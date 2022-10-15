library(devtools)
install_github("DylanDijk/CTVsuggest")
library(CTVsuggest)

ctv_suggestions()
ctv_suggestions("Agriculture", n = 10)
ctv_suggestions("Epidemiology")
ctv_suggestions("NaturalLanguageProcessing")



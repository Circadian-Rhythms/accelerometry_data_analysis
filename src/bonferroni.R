library(emmeans)

bonferroni <- function(model, variable){
  rg_nuis <- ref_grid(model, non.nuisance = variable)
  emm <- emmeans(rg_nuis, variable, contr = "pairwise", adj = "bonferroni")
  return(emm)
}

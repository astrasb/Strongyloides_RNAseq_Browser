# Run Limma based Differential Expression Analysis
# Use Empirical Bayes Statistics to rank genes in order of evidence for differential expression
# Adjust for Multiple Comparisons if necessary

contrast.matrix <- makeContrasts(contrasts = vals$comparison,
                                 levels = v.DEGList.filtered.norm$design)

fits <- contrasts.fit(fit, contrast.matrix)
vals$ebFit <- limma::eBayes(fits)

# Adjust for multiple comparisons using method = global.
if (vals$multipleCorrection == T) {
    vals$results <- decideTests(vals$ebFit, 
                                method="global", 
                                adjust.method="BH", 
                                p.value = adj.P.thresh)
}
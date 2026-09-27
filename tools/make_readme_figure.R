# Regenerate the README figure. Run from the package root with segqcc installed.
library(segqcc)

png("man/figures/README-example.png", width = 1600, height = 1100, res = 150)
op <- par(mfrow = c(2, 1))
data(segxbar); data(segc)
plot(segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample),
     main = "Segmented xbar chart - mean shifts at samples 120 and 240")
plot(segmented_qcc(segc$value, type = "c"),
     main = "Segmented c chart - Poisson rate shift at sample 150")
par(op)
dev.off()
cat("written: man/figures/README-example.png\n")

# Install and load necessary packages
install.packages("FrF2")

# Installing and loading the FrF2 package
if (!requireNamespace("FrF2", quietly = TRUE)) {
  install.packages("FrF2")
}
library(FrF2)

## factors <- read.delim("data/headers.txt")
factors <- c("channel_1", "stage", "segment",  "weeks_since", "quant_bin","stt1",'stt2')
factors <- c("channel_1", "stage", "segment", "loc", "loc2", "weeks_since", "quant_bin","stt1",'stt2')

frac_design <- FrF2(nfactors = 9, resolution = 3, randomize = TRUE, factor.names = factors)
frac_design <- FrF2(nfactors = 7, resolution = 4, randomize = TRUE, factor.names = factors)

# View the design
print(frac_design)

## results <- read.csv("data/experiment.csv")

## Just generate some data to validate..
#frac_design$Y <- rnorm(nrow(frac_design2))
set.seed(111)
frac_design$Y <- rnorm(nrow(frac_design))
summarylm(Y ~ ., data = frac_design))

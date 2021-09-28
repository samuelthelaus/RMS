
# Own exam number: B082226

# Prepared in collaboration with B081705

library(psych)
library(GPArotation)

data <- read.csv('INSERT FILEPATH')

# Data cleaning

# Scrub for out of range values
data[,3:ncol(data)] <- scrub(data[,3:ncol(data)], min = rep(1, ncol(data[,3:ncol(data)])), 
                             max = c(rep(5, ncol(data[,3:18])), rep(7, ncol(data[,19:47]))))

# Define function that finds NAs and prints column number and NAs if > 0
no.na <- function(x) {
  for(i in 1:ncol(x)) {
    n <- sum(is.na(x[,i]))
    p <- sprintf('Column number %.0f has %.0f NAs', i, n)
    if(n > 0) {
      print(p)
    }
  }
}

# Run on data
no.na(data)

# Reverse coding HEXCO
data[,3:18] <- reverse.code(keys = c(1,1,-1,-1,1,-1,-1,-1,1,-1,-1,-1,1,1,-1,-1), items = data[,3:18],
                            mini = rep(1, ncol(data[,3:18])), max = rep(5, ncol(data[,3:18])))

# Take avg for HEXCO scores and make new variables
data$Sinc <- (data$hsinc1 + data$hsinc2 + data$hsinc3 + data$hsinc4)/4
data$Fair <- (data$hfair1 + data$hfair2 + data$hfair3 + data$hfair4)/4
data$GrAvoid <- (data$hgree1 + data$hgree2 + data$hgree3 + data$hgree4)/4
data$Mod <- (data$hmode1 + data$hmode2 + data$hmode3 + data$hmode4)/4

# Data checks
fa_data <- data[data$Survey == 2, 19:47]

# Look at item distributions
png('Histograms_RMS3_FA.png', width = 1400, height = 1200)
par(mfrow = c(5, 6))
for (i in 1:29) {
  barplot(table(fa_data[ , i]), main = names(fa_data)[i])
}
dev.off()

# Check descriptives for skew
describe(fa_data)

cors <- cor(fa_data)

# Distribution of correlation strength
cors_plot <- cut(abs(cors), c(0, .2, .5, .7, 1), c("Negligible", "Weak", "Moderate", "Strong"))

png('Correlation Strengths.png', width = 800, height = 300)
par(mfrow=c(1,1))
barplot(table(cors_plot[lower.tri(cors_plot)]))
dev.off()
summary(cors_plot)

# KMO
KMO(cors)

# Bartlett test
cortest.bartlett(cors, n=nrow(fa_data))

## Factor analysis

# Parallel Analysis, 7 factors
fa.parallel(fa_data, fa='fa')

png('Parallel Analysis Scree.png', width = 800, height = 300)
fa.parallel(fa_data, fa='fa')
dev.off()

# MAP, 3 factors
VSS(fa_data)

# Look at 3-7

# 50% variance
# Not all factors have 3 salient loadings
fa7 <- fa(cors, nfactors = 7, n.obs = 675, rotate = 'oblimin', 
          fm = 'ml')
fa.sort(fa7)

# 48% variance
# Only 1 loading >.33 on ML6
fa6 <- fa(cors, nfactors = 6, n.obs = 675, rotate = 'oblimin', 
          fm = 'ml')
fa.sort(fa6)

# 45% variance
# Has at least 3 items >.33 on each factor
fa5 <- fa(cors, nfactors = 5, n.obs = 675, rotate = 'oblimin', 
          fm = 'ml')
fa.sort(fa5)

# 43% variance
# Has at least 3 items >.33 on each factor
fa4 <- fa(cors, nfactors = 4, n.obs = 675, rotate = 'oblimin', 
          fm = 'ml')
fa.sort(fa4)

# 40% variance
# Has at least 3 items >.33 on each factor
fa3 <- fa(cors, nfactors = 3, n.obs = 675, rotate = 'oblimin', 
          fm = 'ml')
fa.sort(fa3)


# Go with 3 factor solution
# OVERALL 20, 24, 25, 29 have high complexity and low primary loadings (<.33)
# Remove complex items
which(rowSums(fa3$loadings < .33) == 3)

fa_data2 <- fa_data[, -which(colnames(fa_data) %in% c('wb20', 'wb24', 'wb25', 'wb29'))]
cors2 <- cor(fa_data2)

# Parallel Analysis, 5 factors
fa.parallel(fa_data2)

# MAP, 2-3 factors
VSS(fa_data2)

# 43% variance
# No problematic items
fa3.2 <- fa(cors2, nfactors = 3, n.obs = 675, rotate = 'oblimin', 
            fm = 'ml')
fa.sort(fa3.2)

which(rowSums(fa3.2$loadings < .33) == 3)

fa3.2$loadings

# Factor 1 - 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21 (Positive Self View)
# Factor 2 - 1, 2, 3, 4, 5 (Life Satisfaction)
# Factor 3 - 22, 26, 28 (Optimism, reverse coded)

# Reliability through omega
# alpha = .91
# omega-h = .65
# omega-total = .93
psych::omega(fa_data2, nfactors = 3, fm='ml', 
             key = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,1,-1,1,-1))


# Factor rotation congruence
orth <- fa(fa_data2, nfactors = 3, fm="ml", rotate="varimax")
oblq <- fa(fa_data2, nfactors = 3, fm="ml", rotate="oblimin")

factor.congruence(orth, oblq)

# Make second dataset from Survey 1 to compare factor structure
fa_data2.2 <- data[data$Survey == 1, which(colnames(data) %in% colnames(fa_data2))]

# Factor 1, 2, 3 are same
# In survey 1, item 24 has high complexity
# Item 24 low primary loading to factor 4, and high cross-loading to 5
fa3.2.1 <- fa(cor(fa_data2.2), nfactors = 3, n.obs = 676, rotate = 'oblimin', 
              fm = 'ml')
fa.sort(fa3.2.1)

factor.congruence(fa3.2, fa3.2.1)

# Create new data frame with HEXCO and new factors
new_data <- cbind(data[,c(1:18, 48:51)], data[,which(colnames(data) %in% colnames(fa_data2))])

new_data[,c(43,45,47)] <- reverse.code(keys = c(-1,-1,-1), new_data[,c(43,45,47)],
                                       mini = c(1, 1, 1), maxi = c(7, 7, 7))

# Factor scores
fa3.2.scores <- fa(new_data[,23:47], nfactors = 3, fm="ml", rotate="oblimin",
                   scores = "tenBerge")
fa.sort(fa3.2.scores)

new_data_scores <- data.frame(new_data, fa3.2.scores$scores)

colnames(new_data_scores)[48:50] <- c('Positive_Self', 'Satisfaction',
                                      'Optimism')

# Correlation matrix
final_matrix <- corr.test(new_data_scores[,c(19:22, 48:50)])
write.csv(final_matrix$r[5:7,1:4], 'HEXACO by WB cor.csv')
write.csv(final_matrix$p[5:7,1:4], 'HEXACO by WB p-val.csv')

# Bonferroni
.05/12



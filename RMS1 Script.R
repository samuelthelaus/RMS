#Script for B082226
#Collaboration with B079800 and B081705


#####Question 1#####
####################

#Loading data into R
#data_cw <- read.csv("C:/Users/Sam/Downloads/RMS1_1617_coursework_data.csv", header = TRUE)
#Making study_type into a factor
data_cw$study_type <- factor(data_cw$study_type, levels = c("a", "b", "c", "d"))
#Check for missing data
sum(is.na(data_cw$study_type))
sum(is.na(data_cw$cat_test))
#Install and load psych package
#install.packages("psych")
library(psych)
#Calculating mean, standard deviation and skew
by(data = data_cw[ , "cat_test"], INDICES = data_cw$study_type, FUN = mean)
by(data = data_cw[ , "cat_test"], INDICES = data_cw$study_type, FUN = sd)
by(data = data_cw[ , "cat_test"], INDICES = data_cw$study_type, FUN = skew)
#Create anova model and print summary
cat_aov <- aov(formula = cat_test ~ study_type, data = data_cw)
summary(cat_aov)
#Install and load package car and agricolae
#install.packages("car")
#install.packages("agricolae")
library(car)
library(agricolae)
#Durbin Watson test for independence of errors
durbinWatsonTest(cat_aov)
#Levene Test for homoscedasticity
leveneTest(cat_aov)
#Shapiro Wilks test for normality of errors
shapiro.test(resid(cat_aov))
#Make Cook Distance plot for to clearly see outliers
plot(cat_aov, which =4)
#Create variables for contrasts
c1 <- c(.5, -.5, -.5, .5)
c2 <- c(0, -1, 1, 0)
c3 <- c(-1, 0, 0, 1)
#combine to matrix
mat_cw <- cbind(c1, c2, c3)
#Contrasts onto data
contrasts(data_cw$study_type) <- mat_cw
#Print summary
summary.aov(cat_aov, split = list(study_type = list("Single vs. Paired" = 1, "Within vs. Mixed" = 2, "Random vs. Blocked" = 3)))
#Means plot
barplot(by(data = data_cw[ , "cat_test"], INDICES = data_cw$study_type, FUN = mean), xlab = "Study Type", ylab = "Mean categorization Score", main = "Mean Scores per Study Type", col = "gray30")


#####Question 2#####
####################

#Load psych package
library(psych)
#Check for missing data
sum(is.na(data_cw$cat_RT))
sum(is.na(data_cw$caffeine))
#Create new data frame with caffeine and reaction time
caf_test <- data.frame("Reaction_Time" = data_cw$cat_RT, "Caffeine" = data_cw$caffeine)
#Use describe function to get descriptive stats
describe(caf_test)
#Create linear model and print summary
lm_caf <- lm(Reaction_Time ~ Caffeine, data = caf_test)
summary(lm_caf)
#Check if normality of errors
shapiro.test(lm_caf$residuals)
#Assess outliers with Cook's Distance plot
plot(lm_caf, which = 4)
#Running test for homogeneity of variance
ncvTest(lm_caf)
#durbin watson for inependence of error
durbinWatsonTest(lm_caf)
#Graphical relationship of caffeine and reaction time
plot(x = data_cw$caffeine, data_cw$cat_RT, xlab = "Caffeine (mg)", ylab = "Reaction Time (ms)")
abline(lm_caf, col = "red")


#####Question 3#####
####################

#Check for missing data
sum(is.na(data_cw$study_type))
sum(is.na(data_cw$learned_cat))
#Create table for data and save as variable
table_learn <- table(data_cw$learned_cat, data_cw$study_type)
print(table_learn)
#Create mosaic plot for data
mosaicplot(learned_cat ~ study_type, data = data_cw, col = c("gray20", "gray50", "gray80", "gray99"), 
           main = "Mosaic plot for Categorization Learning and Subject Type", xlab = "", ylab = "", 
           cex.axis = 1)
#Run Pearson's Chi-squared test
chisq.test(table_learn)
#Residuals of chisq
results <- chisq.test(table_learn)
results$residuals
#Cramer's V calculation for effect size
sqrt(10.989/80)

#####Question 4#####
####################

#Create new data frame for variables
df_WM <- data.frame("WM_bf" = data_cw$WM_before, "WM_af" = data_cw$WM_after)
#Check for missing data
sum(is.na(data_cw$WM_before))
sum(is.na(data_cw$WM_after))
#summarize data to use in table
summary(as.factor(df_WM$WM_bf))
summary(as.factor(df_WM$WM_af))
#Create histograms for the two variables
par(mfrow = c(1, 2))
hist(df_WM$WM_bf, main = "Before", xlim = c(1, 6), breaks = 1:6, col = "gray35", xlab = "Working Memory Before")
hist(df_WM$WM_af, main = "After", xlim = c(1, 6), breaks = 1:6, col = "gray35", xlab = "Working Memory After")
#Run Shapiro Wilks Test to test for normality
shapiro.test(df_WM$WM_bf)
shapiro.test(df_WM$WM_af)
#Wilcoxon signed rank test
wilcox.test(df_WM$WM_bf, df_WM$WM_af, paired = TRUE)

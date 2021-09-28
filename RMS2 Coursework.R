####RMS2 COURSEWORK####

# Exam number: B082226

# Collaborated with: B081705

# Load data (change filepath)
cw_data <- read.csv('/Users/sam/Google Drive/Uni/Year 4/RMS2/RMS2_data_1819.csv')

# Load packages if needed
library(car)
library(psych)
library(ggplot2)

# Look at data through psych pkg describe function and save
describe(cw_data)[4:12, c(2:4, 8, 9, 11, 12)]
write.csv(describe(cw_data)[4:12, c(2:4, 8, 9, 11, 12)], 'DescriptiveRMS2.csv')

# Create sum of is.na function
sum_na <- function(x) {
  y <- sum(is.na(x))
  return(y)
}

# Run sum_na with lapply to see if NAs in each col
lapply(cw_data, sum_na)

# Look at classes for each column
lapply(cw_data, class)

# Change gender and age (categorical) to factors
cw_data$gender <- as.factor(cw_data$gender)
cw_data$age <- as.factor(cw_data$age)

# Get summary data for categorical variables
summary(cw_data$gender)
summary(cw_data$age)

# Removing values that are too high or too low
for(i in 1:nrow(cw_data)) {
  
  # Look at i-th row and j-th col, if value above/below range or NA, make NA
  # Also print i and j to see row/col
  for(j in 4:8) {
    if(cw_data[i, j] > 5 | cw_data[i, j] < 1 | is.na(cw_data[i, j])) {
      cw_data[i, j] <- NA
      print(i)
      print(j)
    }
  }
  if(cw_data[i, 9] > 10 | cw_data[i, 9] < 0 | is.na(cw_data[i, 9])) {
    cw_data[i, 9] <- NA
    print(i)
    print(j)
  }
  for(j in 10:12) {
    if(cw_data[i, j] > 4 | cw_data[i, j] < 1 | is.na(cw_data[i, j])) {
      cw_data[i, j] <- NA
      print(i)
      print(j)
    }
  }
}

# Make opposite of 'in' to look for incorrect values in age/gender
'%ni%' <- Negate('%in%')
which(cw_data$gender %ni% c('0', '1'))
which(cw_data$age %ni% c('0', '1', '2', '3', '4'))

# Change categorical level names
cw_data$gender <- factor(cw_data$gender, labels = c('male', 'female'))
cw_data$age <- factor(cw_data$age, labels = c("18-25", "26-34", "35-44", "45-54", "55-64"))

#### Question 1 ####

q1m1 <- lm(scale(anx) ~ gender, data = cw_data)
summary(q1m1)

# Bar plot looking at gender differences
anxGenderBar <- data.frame(Gender = c('male', 'female'),
                           Mean = c(mean(cw_data$anx[cw_data$gender == 'male']),
                                    mean(cw_data$anx[cw_data$gender == 'female'])),
                           SD = c(sd(cw_data$anx[cw_data$gender == 'male']),
                                  sd(cw_data$anx[cw_data$gender == 'female'])))

ggplot(anxGenderBar, aes(y = Mean, x = Gender)) +
  geom_bar(stat = 'identity', col = 'black', fill = 'gray',
           width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.1) +
  labs(y = 'Avg anxiety score - GHQA') +
  theme_minimal()

ggsave('RMS2_CW_q1_plot.png')

## Assumption checks
# Cook's distance for influence
cooks <- cooks.distance(q1m1)
names(which(cooks > (4/(422-5-1))))

png('RMS_CW_Cooks_Q1.png')
plot(q1m1, which = 4)
dev.off()

# Normality of residuals
png('RMS_CW_Norm_Q1.png')
qqPlot(q1m1)
dev.off()

# Homoscedasticity
png('RMS_CW_Hom_Q1.png')
residualPlots(q1m1)
dev.off()

# Linearity
png('RMS_CW_Linearity_Q1.png')
crPlots(q1m1)
dev.off()

# Independence of residuals
durbinWatsonTest(q1m1)



#### Question 2 ####

# Linear model with dummy coding
ageGenderLM <- lm(scale(anx) ~ age + gender, data = cw_data)
summary(ageGenderLM)

anxAgGenBar <- data.frame(Age = c("18-25", "26-34", "35-44", "45-54", "55-64"),
                          Mean = c(mean(cw_data$anx[cw_data$age == '18-25']),
                                    mean(cw_data$anx[cw_data$age == '26-34']),
                                    mean(cw_data$anx[cw_data$age == '35-44']),
                                    mean(cw_data$anx[cw_data$age == '45-54']),
                                    mean(cw_data$anx[cw_data$age == '55-64'])),
                          SD = c(sd(cw_data$anx[cw_data$age == '18-25']),
                                    sd(cw_data$anx[cw_data$age == '26-34']),
                                    sd(cw_data$anx[cw_data$age == '35-44']),
                                    sd(cw_data$anx[cw_data$age == '45-54']),
                                    sd(cw_data$anx[cw_data$age == '55-64'])))

ggplot(anxAgGenBar, aes(y = Mean, x = Age)) +
  geom_bar(stat = 'identity', col = 'black', fill = 'gray',
           width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.1) +
  labs(y = 'Avg anxiety score - GHQA') +
  theme_minimal()

ggsave('RMS2_CW_age_plot.png')

## Assumption checks
# Cook's distance for influence
cooks <- cooks.distance(ageGenderLM)
names(which(cooks > (4/(422-5-1))))

png('RMS_CW_Cooks_Q2.png')
plot(ageGenderLM, which = 4)
dev.off()

# Normality of residuals
png('RMS_CW_Norm_Q2.png')
qqPlot(ageGenderLM)
dev.off()

# Homoscedasticity
png('RMS_CW_Hom_Q2.png')
residualPlots(ageGenderLM)
dev.off()

# Linearity
png('RMS_CW_Linearity_Q2.png')
crPlots(ageGenderLM)
dev.off()

# Independence of residuals
durbinWatsonTest(ageGenderLM)

# Multi-collinearity
# Make table
write.csv(vif(ageGenderLM), 'RMS_CW_VIF_Q2.csv')



#### Question 3 ####

# Create new data frame with effects coding for age
cw_dataAgeE <- cw_data
contrasts(cw_dataAgeE$age) <- contr.sum(5)
contrasts(cw_dataAgeE$age)

q3_lm <- lm(scale(anx) ~ gender + age, data = cw_dataAgeE)
summary(q3_lm)


## Assumption checks
# Cook's distance for influence
cooks <- cooks.distance(q3_lm)
names(which(cooks > (4/(422-5-1))))

png('RMS_CW_CooksQ3.png')
plot(q3_lm, which = 4)
dev.off()

# Normality of residuals
png('RMS_CW_NormResid_Q3.png')
qqPlot(q3_lm)
dev.off()

# Homoscedasticity
png('RMS_CW_Hom_Q3.png')
residualPlots(q3_lm)
dev.off()

# Linearity
png('RMS_CW_Lin_Q3.png')
crPlots(q3_lm)
dev.off()

# Independence of residuals
durbinWatsonTest(q3_lm)

# Multi-collinearity
# Make talbe
write.csv(vif(q3_lm), 'RMS_CW_VIF_Q3.csv')




#### Question 4 ####

AgePerGenLM <- lm(scale(anx) ~ age + gender + scale(O) + scale(C) + scale(E) + scale(A) + scale(N), data = cw_dataAgeE)
summary(AgePerGenLM)


## Assumption checks
# Cook's distance for influence
cooks <- cooks.distance(AgePerGenLM)
names(which(cooks > (4/(422-10-1))))

png('RMS_CW_Cooks_Q4.png')
plot(AgePerGenLM, which = 4)
dev.off()

# Normality of residuals
png('RMS_CW_NormResid_Q4.png')
qqPlot(AgePerGenLM)
dev.off()

# Homoscedasticity
png('RMS_CW_Hom_Q4.png', width = 960, height = 960)
residualPlots(AgePerGenLM)
dev.off()

# Linearity
png('RMS_CW_Lin_Q4.png', width = 960, height = 960)
crPlots(AgePerGenLM)
dev.off()

# Independence of residuals
durbinWatsonTest(AgePerGenLM)

# Multi-collinearity
# Make table
write.csv(vif(AgePerGenLM), 'RMS_CW_VIF_Q4.csv')



#### Question 5 ####

# Make new DF for effects coded DF (from q. 3) without NA
cw_dataAgeE_NARM <- na.omit(cw_dataAgeE)

q4_nona <- lm(scale(anx) ~ age + gender + scale(O) + scale(C) + scale(E) + scale(A) + scale(N), data = cw_dataAgeE_NARM)
q5_nona <- lm(scale(anx) ~ age + gender + scale(O) + scale(C) + scale(E) + scale(A) + scale(N) + scale(PD) + scale(DL) + scale(SUP), data = cw_dataAgeE_NARM)

summary(q4_nona)
summary(q5_nona)

## Assumption checks
# Cook's distance for influence
cooks <- cooks.distance(q5_nona)
names(which(cooks > (4/(421-13-1))))

png('RMS_CW_Cooks_Q5.png')
plot(q5_nona, which = 4)
dev.off()

# Normality of residuals
png('RMS_CW_NormRes_Q5.png')
qqPlot(q5_nona)
dev.off()

# Homoscedasticity
# Export using export button since plotted in 2 steps
residualPlots(q5_nona)

# Linearity
# Export using export button since plotted in 2 steps
crPlots(q5_nona)

# Independence of residuals
durbinWatsonTest(q5_nona)

# Multi-collinearity
# Make table for appendix
write.csv(vif(q5_nona), 'RMS_CW_VIF_Q5.csv')


anova(q4_nona, q5_nona)
AIC(q4_nona, q5_nona)
BIC(q4_nona, q5_nona)



#### Question 6 ####

# Make new DF for effects coded DF (from q. 3) without NA
cw_dataAgeE_NARM <- na.omit(cw_dataAgeE)

q1_nona <- lm(scale(anx) ~ gender, data = cw_data_NARM)
q2_nona <- lm(scale(anx) ~ age + gender, data = cw_data_NARM)
q3_nona <- lm(scale(anx) ~ age + gender, data = cw_dataAgeE_NARM)

# Get comparison data into data frame

cwAIC <- data.frame(AIC = AIC(q1_nona, q2_nona, q3_nona, q4_nona, q5_nona)[,2],
                    Model = c('Simple', 'Dummy', 'Effects',
                              'With Personality', 'With Job Chars'))
cwBIC <- data.frame(BIC = BIC(q1_nona, q2_nona, q3_nona, q4_nona, q5_nona)[,2],
                    Model = c('Simple', 'Dummy', 'Effects',
                              'With Personality', 'With Job Chars'))

# Plot AIC and BIC to barplot for visual comparison

ggplot(cwAIC, aes(y = AIC, x = Model)) +
  geom_bar(stat = 'identity', col = 'black', fill = 'gray') +
  geom_text(aes(label = round(AIC, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_y_continuous(limits = c(0, 1240)) +
  theme_minimal()

ggsave('RMS2_CW_AICComparison.png')


ggplot(cwBIC, aes(y = BIC, x = Model)) +
  geom_bar(stat = 'identity', col = 'black', fill = 'gray') +
  geom_text(aes(label = round(BIC, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_y_continuous(limits = c(0, 1240)) +
  theme_minimal()

ggsave('RMS2_CW_BICComparison.png')

# Final model does best
summary(q5_nona)

# Get regression table
write.csv(summary(q5_nona)[4], 'RMS2_CW_RegTable.csv')

# Plot significant parameters (N, PD, DL)

ggplot(cw_data_NARM, aes(x = N, y = anx)) +
  geom_point(shape = '*', size = 6, col = 'blue') +
  geom_smooth(method = 'lm', col = 'red', fill = 'red') +
  labs(title = 'Anxiety X Neuroticism', x = 'Neuroticism',
       y = 'Anxiety score (GHQA)') +
  theme_minimal()

ggsave('RMS2_CW_q5_Neur.png')

ggplot(cw_data_NARM, aes(x = PD, y = anx)) +
  geom_point(shape = '*', size = 6, col = 'blue') +
  geom_smooth(method = 'lm', col = 'red', fill = 'red') +
  labs(title = 'Anxiety X Psychological Demand', 
       x = 'Psychological Demand', y = 'Anxiety score (GHQA)') +
  theme_minimal()

ggsave('RMS2_CW_q5_PD.png')

ggplot(cw_data_NARM, aes(x = DL, y = anx)) +
  geom_point(shape = '*', size = 6, col = 'blue') +
  geom_smooth(method = 'lm', col = 'red', fill = 'red') +
  labs(title = 'Anxiety X Decision Latitude', x = 'Decision latitude',
       y = 'Anxiety score (GHQA)') +
  theme_minimal()

ggsave('RMS2_CW_q5_DL.png')



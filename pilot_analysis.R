#' ---
#' title: "SOSC 133 Pilot data analysis"
#' ---
setwd("/Users/tiffanie/Documents/SOSC_133/ssi3-data-analysis")

library(estimatr)

pilot_data <- read.csv("../data/pilot_data.csv")
data <- pilot_data[3:nrow(pilot_data), 
                  c('GasTax', 'CarbTax','Treaty', 'RegCarb', 'political_views',
                    'party_id', 'party_id.1','party_id.2', 'QID74',
                    'ScientificConfidence', 'RewardConsequence.',
                    'Attention_Check_1', 'Religiosity', 'Economic_Reasoning',
                    'Attention_Check_2', 'prosociality_1', 'prosociality_2',
                    'prosociality_3', 'prosociality_4','prosociality_5',
                    'prosociality_6', 'prosociality_7', 'prosociality_8',
                    'prosociality_9', 'GasTax_after', 'CarbTax_after',
                    'Treaty_after', 'RegCarb_after', 'treatment_value')]
summary(data)

# filter responses based on 2 attention checks
data <- data[which(data$Attention_Check_1 == "Strongly like" &
                    data$Attention_Check_2 == '1,3'), ]

# 186 observations
dim(data)
dim(pilot_data)

# method 1: "main_party_id" -- consolidate Independent and No preference (should ask the other data group)
data$main_party_id = data$party_id
data[which((data$party_id == "Independent") |
             (data$party_id == "No preference")), ]$main_party_id = "Independent_nopref"

table(data$main_party_id)

# method 2: "party" -- group by Democrat/Republican-leaning, then include or exclude pure Independents/no preference
data[which((data$party_id == "Democrat") | (data$QID74 == 2)), "party"] <- "D"
data[which((data$party_id == "Republican") | (data$QID74 == 4)), "party"] <- "R"
data[which(data$QID74 == 3), "party"] <- "I"

table(data$party)

# average policy support (in [0, 3])
data$avg_policy_support <- rowMeans(sapply(data[, c('GasTax_after',
                                                    'CarbTax_after',
                                                    'Treaty_after',
                                                    'RegCarb_after')], as.numeric),
                                    na.rm=TRUE)

# mapping treatment values to treatment condition names
# represents no treatment, the rest are in order of the framings on the document
treatments = c("No framing",
               "Positive science",
               "Negative science",
               "Religious",
               "Equity",
               "Efficiency",
               "Secular")
data$treatment_frame <- factor(data$treatment_value, labels=treatments)

# distribution of subjects across treatment conditions
# N = 186
table(data$treatment_frame)

#' **1. What is the mean response under each of the different framings, on average, and separately for dems/republicans?**
#' 
#' Difference in means
#' 
#' - A simple table of the average climate policy support under each of the different framings, with standard errors.  
#' - Average policy support for different framing separated by political position
#' 
#' Regression analysis
#' 
#' - Basic regression analysis of framing’s impact on policy position, controlling for de-meaned covariates and de-meaned covariates + treatment interactions. Use robust standard errors.
#' - Include pre-test response as a control, and list other controls based on the data quality group's coding. 
#' - We should use the Lin estimator, as we did in week 3, where we de-mean all covariates, and then control for covariates and covariate-treatment interactions.

mean_sem <- function(x) {
  c(mean(x), sd(x) / sqrt(length(x)))
}

aggregate(x=(data[, "avg_policy_support"]),
          by=list(`treatment frame`=data$treatment_frame),
          FUN=mean_sem)

# temporarily using method 2 (Independents are D/R-leaning or pure Independent)
# including pure Independents
aggregate(x=(data[, "avg_policy_support"]), 
          by=list(`party`=data$party, `treatment frame`=data$treatment_frame),
          FUN=mean_sem)

# SE for avg policy support is NaN: only one person with party I that was
# assigned treatment 2

# excluding pure Independents
two_party_data <- data[which(data$party != "I"),]
aggregate(x=(two_party_data[, "avg_policy_support"]), 
          by=list(`treatment frame`=two_party_data$treatment_frame,
                  `party`=two_party_data$party),
          FUN=mean_sem)

# possible covariates: political party, political views, scientific confidence,
# religious, economic reasoning

# scientific confidence, political views each contain one NaN value

# party ID
data$party_cov <- data$party_id.1
data$party_cov[data$party_id.2 != ""] <- data$party_id.2[data$party_id.2 != ""]
data$party_cov[data$QID74 != ""] <- data$QID74[data$QID74 != ""]

treat_data <- data.frame(data)

# pretest response for control (in [0, 3])
treat_data$pre_avg_policy_support <- rowMeans(sapply(data[,c('GasTax',
                                                             'CarbTax',
                                                             'Treaty',
                                                             'RegCarb')],
                                                     as.numeric),
                                              na.rm=TRUE)

# leaving political_views out for now because of multicollinearity with party_cov
# treat pre-test response like covariates
covariates <- c("Religiosity", "ScientificConfidence", "party_cov",
                "Economic_Reasoning")
treat_data[covariates] <- sapply(data[covariates], as.numeric)

model1 <- lm_lin(avg_policy_support ~ treatment_value,
                 ~ Religiosity +
                   ScientificConfidence +
                   party_cov +
                   Economic_Reasoning +
                   pre_avg_policy_support,
                 data=treat_data, se_type="HC3")

summary(model1)

#' Note: compared to Python version, the estimates for coefficients are slightly different due to lm_lin() having a different method of centering covariates
colMeans(treat_data[, c(covariates, "pre_avg_policy_support")], na.rm=TRUE)
model1$scaled_center

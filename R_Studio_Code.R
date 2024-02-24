# sources:
# ifelse: https://www.datamentor.io/r-programming/ifelse-function/
  
  
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(kableExtra)


data <- read.csv("/Users/ethanyattaw/Documents/College/Harp 325/final proj/compas-scores-two-years.csv")
data_black <- filter(data, race == "African-American")
data_white <- filter(data, race == "Caucasian")
data_b_w <- subset(data, race %in% c("African-American", "Caucasian"))


# regression of recidivism on risk score
lm_compas = lm(formula = is_recid ~ decile_score,
           data = data)

summary(lm_compas)


# scatter plot
ggplot(data)+
  geom_point(aes(x = decile_score, y = is_recid))+
  geom_smooth(method = "lm", aes(x = decile_score, y = is_recid))+
  theme_minimal()+
  labs(title = "Recidivism vs Risk Score")

# bar graph
# getting the proportion of recidivism for each decile score
prop_recidivism <- data %>% 
  group_by(decile_score) %>% 
  summarize(prop_recid = mean(is_recid)) %>% 
  arrange(decile_score)

ggplot(prop_recidivism, aes(x = decile_score, y = prop_recid, fill = "orange")) +
  geom_col() +
  labs(title = "Proportion of Recidivism by Risk Score", x = "Risk Score", y = "Proportion of Recidivism") +
  theme_minimal()+
  guides(fill=FALSE) # prevents legend for the color





# different regressions of recid and risk score

summary(lm(formula = is_recid ~ age,
               data = data))
summary(lm(formula = decile_score ~ age,
           data = data))

summary(lm(formula = is_recid ~ priors_count,
           data = data))
summary(lm(formula = decile_score ~ priors_count,
           data = data))

summary(lm(formula = is_recid ~ sex,
           data = data))
summary(lm(formula = decile_score ~ sex,
           data = data))



# BIG FINDS?
# regression of recidivism and risk on being black vs being white

# creating new binary variable for being black and for being white
# to help with linear regression
data_b_w$black <- ifelse(data_b_w$race == "African-American", 1, 0)
data_b_w$white <- ifelse(data_b_w$race == "Caucasian", 1, 0)

lm_recid_black <- lm(is_recid ~ black, data = data_b_w)
summary(lm_recid_black)
lm_risk_black <- lm(decile_score ~ black, data = data_b_w)
summary(lm_risk_black)

lm_recid_white <- lm(is_recid ~ white, data = data_b_w)
summary(lm_recid_white)
lm_risk_white <- lm(decile_score ~ white, data = data_b_w)
summary(lm_risk_white)




# SUMMARY TABLE BLACK
variables_black <- data_black %>% 
  select(c(age, priors_count, decile_score, is_recid, v_decile_score, is_violent_recid))

mean_b <- variables_black %>% summarise(across(everything(), mean, na.rm=TRUE))
sd_b <- variables_black %>% summarise(across(everything(), sd, na.rm=TRUE))
min_b <- variables_black %>% summarise(across(everything(), min, na.rm=TRUE))
max_b <- variables_black %>% summarise(across(everything(), max, na.rm=TRUE))

table_b <- rbind(mean_b, sd_b, min_b, max_b) # create table
rownames(table_b) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")
table_b <- t(table_b) # transpose
options(scipen = 999) # remove scientific notation

#Tell R to round the data to make it look nicer
table_b <- table_b %>% 
  as.data.frame %>% 
  mutate_if(is.numeric, round, digits=2)

table_b %>%
  kbl(caption = "<center><strong>Figure 1: Black Summary Data</strong></center>",
      format = "html") %>%
  kable_classic_2("striped", full_width = F)





# SUMMARY TABLE WHITE
variables_white <- data_white %>% 
  select(c(age, priors_count, decile_score, is_recid, v_decile_score, is_violent_recid))

mean_w <- variables_white %>% summarise(across(everything(), mean, na.rm=TRUE))
sd_w <- variables_white %>% summarise(across(everything(), sd, na.rm=TRUE))
min_w <- variables_white %>% summarise(across(everything(), min, na.rm=TRUE))
max_w <- variables_white %>% summarise(across(everything(), max, na.rm=TRUE))

table_w <- rbind(mean_w, sd_w, min_w, max_w) # create table
rownames(table_w) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")
table_w <- t(table_w) # transpose
options(scipen = 999) # remove scientific notation

#Tell R to round the data to make it look nicer
table_w <- table_w %>% 
  as.data.frame %>% 
  mutate_if(is.numeric, round, digits=2)

table_w %>%
  kbl(caption = "<center><strong>Figure 2: White Summary Data</strong></center>",
      format = "html") %>%
  kable_classic_2("striped", full_width = F)

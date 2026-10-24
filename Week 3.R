dir.create("data")
viz_data = readr::read_csv("data/viz_data.csv")
followup_data = readr::read_csv("data/followup_data.csv")
followup_data = merge(viz_data, followup_data, by = 'id')
lm_fit = lm(response ~ bmi + group, data = viz_data)
bmi_quantiles <- quantile(viz_data$bmi, probs = c(0.10, 0.90))
bmi_quantiles
bmi_80_range <- bmi_quantiles[2] - bmi_quantiles[1]
bmi_80_range
args(t.test)
alternative = "two.sided"
t.test(
  response ~ group, 
  data = viz_data, 
  subset = group %in% c("High dose", "Low dose"), 
  alternative = "greater"
)

summary(lm_fit)
viz_data$improved = as.integer(viz_data$outcome == "Improved")
logistic_fit = glm(improved ~ bmi + group, data = viz_data, family = binomial)
summary(logistic_fit)
install.packages("MASS")
negbin_fit = MASS::glm.nb(clinic_visits ~ group, data = followup_data)
summary(negbin_fit)
install.packages("survival")
library(survival)
followup_surv = Surv(followup_data$survival_months, followup_data$event)
head(followup_surv)
cox_fit = coxph(
  Surv(survival_months, event) ~ group,
  data = followup_data
)
summary(cox_fit)
coef(lm_fit)
confint(lm_fit)
summary(lm_fit)$coefficients
# which = 1 selects just the first of four default diagnostic plots
# (residuals vs. fitted values), rather than showing all four at once
plot(lm_fit, which = 1)
plot(logistic_fit, which = 1)
cox_survfit = survfit(cox_fit, newdata = data.frame(group = unique(followup_data$group)))
plot(cox_survfit,
     xlab = "Time (months)",
     ylab = "Estimated survival probability")
library(tidyverse)
clinic_raw = read_csv("data/clinic_raw.csv", skip = 2)
head(clinic_raw)
str(clinic_raw)
git config --global user.name "rkapurnus"
git config --global user.email "rishi.kapur@mac.com"

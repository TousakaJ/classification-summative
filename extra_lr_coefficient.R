library("skimr")
library("tidyverse")
library("ggplot2")
library("data.table")
library("mlr3verse")
library("GGally")
library("recipes")


# This file can be used to observe the corddicient of linear regression fitting loan.data
loan.data <- readr::read_csv(
    "https://www.louisaslett.com/Courses/MISCADA/bank_personal_loan.csv"
)
loan.data <- loan.data |>
    select(-ZIP.Code)
fit.lr <- glm(as.factor(Personal.Loan) ~ ., binomial, loan.data)

summary_output <- capture.output(summary(fit.lr))
print(summary_output)
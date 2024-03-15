library("skimr")
library("tidyverse")
library("ggplot2")
library("data.table")
library("mlr3verse")
library("GGally")
library("recipes")


# This file can be used to observe the best parameter 'cp' of CART_cp
# LOADING DATA
loan.data <- readr::read_csv(
    "https://www.louisaslett.com/Courses/MISCADA/bank_personal_loan.csv"
)
View(loan.data)
skimr::skim(loan.data)

loan.data <- loan.data |>
    mutate(Loan.Status = case_when(
        Personal.Loan == 1 ~ "yes",
        TRUE ~ "no"
    )) |>
    mutate(Education.Status = case_when(
        Education == 3 ~ "advanced",
        Education == 2 ~ "graduate",
        Education == 1 ~ "undergraduate",
        TRUE ~ "HOWCANITBE"
    )) |>
    mutate(Mortgage.Status = case_when(
        Mortgage > 0 ~ "positive",
        TRUE ~ "none"
    )) |>
    mutate(Securities.Status = case_when(
        Securities.Account == 1 ~ "yes",
        TRUE ~ "no"
    )) |>
    mutate(Certificate.Status = case_when(
        CD.Account == 1 ~ "yes",
        TRUE ~ "no"
    )) |>
    mutate(Online.Status = case_when(
        Online == 1 ~ "yes",
        TRUE ~ "no"
    )) |>
    mutate(Card.Status = case_when(
        CreditCard == 1 ~ "yes",
        TRUE ~ "no"
    )) |>
    select(
        -Personal.Loan, -ZIP.Code, -Education, -Mortgage,
        -Securities.Account, -CD.Account, -Online, -CreditCard
    )

loan.data$Loan.Status        <- factor(loan.data$Loan.Status, levels = c("yes", "no"))
loan.data$Education.Status   <- factor(loan.data$Education.Status, 
                                      levels = c("undergraduate", "graduate", "advanced"),
                                      ordered = TRUE)
loan.data$Mortgage.Status    <- factor(loan.data$Mortgage.Status, levels = c("positive", "none"))
loan.data$Securities.Status  <- factor(loan.data$Securities.Status, levels = c("yes", "no"))
loan.data$Certificate.Status <- factor(loan.data$Certificate.Status, levels = c("yes", "no"))
loan.data$Certificate.Status <- factor(loan.data$Certificate.Status, levels = c("yes", "no"))
loan.data$Online.Status      <- factor(loan.data$Online.Status, levels = c("yes", "no"))
loan.data$Card.Status <- factor(loan.data$Card.Status, levels = c("yes", "no"))

set.seed(212)

loan.task <- TaskClassif$new(
    id = "LoanUpsell",
    backend = loan.data,
    target = "Loan.Status",
    positive = "yes"
)

cv5 <- rsmp("cv", folds = 5)
cv5$instantiate(loan.task)


cp_values <- seq(0.00, 0.01, by=0.001)
best_cp1 <- NA
best_cp2 <- NA
best_cp3 <- NA
best_cp4 <- NA
best_performance1 <- Inf
best_performance2 <- -Inf
best_performance3 <- Inf
best_performance4 <- Inf

for (cp in cp_values) {
    lrn_cart_cp <- lrn("classif.rpart", predict_type = "prob", cp = cp)
    res <- resample(loan.task, lrn_cart_cp, cv5)
    performance1 <- res$aggregate(msr("classif.ce"))
    performance2 <- res$aggregate(msr("classif.acc"))
    performance3 <- res$aggregate(msr("classif.fpr"))
    performance4 <- res$aggregate(msr("classif.fnr"))
    if (performance1 < best_performance1) {
        best_performance1 <- performance1
        best_cp1 <- cp
    }
    if (performance2 > best_performance2) {
        best_performance2 <- performance2
        best_cp2 <- cp
    }
    if (performance3 < best_performance3) {
        best_performance3 <- performance3
        best_cp3 <- cp
    }
    if (performance4 < best_performance4) {
        best_performance4 <- performance4
        best_cp4 <- cp
    }
}

print(best_cp1)
print(best_cp2)
print(best_cp3)
print(best_cp4)
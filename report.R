#I don't know if the install command is neccessary. But I've put it here still!
# install.packages('skimr')
# install.packages('tidyverse')
# install.packages('ggplot2')
# install.packages('data.table')
# install.packages('mlr3verse')
# install.packages('GGally')
# install.packages('recipes')

library("skimr")
library("tidyverse")
library("ggplot2")
library("data.table")
library("mlr3verse")
library("GGally")
library("recipes")

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
loan.data$Card.Status        <- factor(loan.data$Card.Status, levels = c("yes", "no"))


# REGULARIZATION - feel free to uncomment them
# cake <- recipe(Loan.Status ~ ., data = loan.data) %>%
#   step_center(all_numeric()) %>%
#   step_scale(all_numeric()) %>%
#   prep(training = loan.data)
# loan.data <- bake(cake, new_data = loan.data) # apply preprocessing to training data
View(loan.data)


# INITIALIZATION
set.seed(212)

loan.task <- TaskClassif$new(
    id = "LoanUpsell",
    backend = loan.data,
    target = "Loan.Status",
    positive = "yes"
)

cv5 <- rsmp("cv", folds = 5)
cv5$instantiate(loan.task)

ggpairs(
    loan.data |> select(Loan.Status, Age, Experience, Income, Family, CCAvg, Education.Status),
    aes(color = Loan.Status)
)


# MODEL FITTING
lrn_baseline <- lrn("classif.featureless", predict_type = "prob")
lrn_cart     <- lrn("classif.rpart", predict_type = "prob")
lrn_cart_cp  <- lrn("classif.rpart", predict_type = "prob", cp = 0.003, id = "cartcp")
lrn_ranger   <- lrn("classif.ranger", predict_type = "prob")
lrn_xgboost  <- lrn("classif.xgboost", predict_type = "prob")
lrn_log_reg  <- lrn("classif.log_reg", predict_type = "prob")

lrnsp_log_reg <- lrn("classif.log_reg", predict_type = "prob", id = "super")

pl_missing <- po("fixfactors") %>>%
  po("removeconstants") %>>%
  po("imputesample", affect_columns = selector_type(c("ordered", "factor"))) %>>%
  po("imputemean")
pl_factor <- po("encode")

pl_ranger <- pl_missing %>>%
    po(lrn_ranger)
pl_log_reg <- pl_missing %>>%
    po(lrn_log_reg)
pl_xgb <- pl_factor %>>%
    po(lrn_xgboost)

res_nor <- benchmark(data.table(
  task       = list(loan.task),
  learner    = list(lrn_baseline,
                    lrn_cart,
                    lrn_cart_cp,
                    pl_ranger,
                    pl_log_reg,
                    pl_xgb
                    ),
  resampling = list(cv5)
), store_models = TRUE)

spr_lrn <- gunion(list(
  gunion(list(
    po("learner_cv", lrn_baseline),
    po("learner_cv", lrn_cart),
    po("learner_cv", lrn_cart_cp)
  )),
  pl_missing %>>%
    gunion(list(
      po("learner_cv", lrn_ranger),
      po("learner_cv", lrn_log_reg),
      po("nop")
    )),
  pl_factor %>>%
    po("learner_cv", lrn_xgboost)
)) %>>%
  po("featureunion") %>>%
  po(lrnsp_log_reg)

spr_lrn$plot()

res_spr <- resample(loan.task, spr_lrn, cv5, store_models = TRUE)
print(res_nor$aggregate(list(
    msr("classif.ce"),
    msr("classif.acc"),
    msr("classif.fpr"),
    msr("classif.fnr")
)))
print(res_spr$aggregate(list(
    msr("classif.ce"),
    msr("classif.acc"),
    msr("classif.fpr"),
    msr("classif.fnr")
)))
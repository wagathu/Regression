
# k fold validation -------------------------------------------------------

ins_kfold <- lm_kfold(formula = charges ~ category + age2 + children + bmi + sex +
                        bmi_above30*smoker+ region,
                      df=ins_train, predictors=13, folds = 200, times = 30)


# modelfit
cat <- case_when(
  ins_test$category == "Old" ~ ins_kfold[2],
  ins_test$category == "Prime" ~ ins_kfold[3],
  ins_test$category == "Young" ~ ins_kfold[4],
  TRUE ~ 0
)
age <- (ins_test$age**2)*ins_kfold[5]
children <- ins_test$children*ins_kfold[6]
bmi <- ins_test$bmi*ins_kfold[7]

sex <- case_when(
  ins_test$sex == "male" ~ ins_kfold[8],
  TRUE ~ 0
)
bmiabove30 <- ins_test$bmi_above30*ins_kfold[9]

smokerstatus <- case_when(
  ins_test$smoker == "yes" ~ ins_kfold[10],
  TRUE ~ 0
)

region <- case_when(
  ins_test$region == "southwest" ~ ins_kfold[13],
  ins_test$region == "southeast" ~ ins_kfold[12],
  ins_test$region == "northwest" ~ ins_kfold[11],
  TRUE ~ 0
)

ba30smyes <- ins_test$bmi30smokeryes*ins_kfold[14]

kfold_pred <- (cat+age+children+bmi+sex+
                 bmiabove30+smokerstatus+region+ba30smyes)+ins_kfold[1]

# testting MSE
ins_pred <- predict.lm(ins_eval_remout, ins_test, type = "response")

mse(ins_test$charges, ins_pred)
mse(ins_test$charges, kfold_pred)


# fitting a loop on the k fold --------------------------------------------

kfold_mse <- vector()
span <- seq(5,100,5)
for (i in 1:length(span))
{
  ins_kfold <- lm_kfold(formula = charges ~ category + age2 + children + bmi + sex +
                          bmi_above30*smoker+ region,
                        df=ins_train, predictors=13, folds = 200, times = span[i])
  
  
  # modelfit
  cat <- case_when(
    ins_test$category == "Old" ~ ins_kfold[2],
    ins_test$category == "Prime" ~ ins_kfold[3],
    ins_test$category == "Young" ~ ins_kfold[4],
    TRUE ~ 0
  )
  age <- (ins_test$age**2)*ins_kfold[5]
  children <- ins_test$children*ins_kfold[6]
  bmi <- ins_test$bmi*ins_kfold[7]
  
  sex <- case_when(
    ins_test$sex == "male" ~ ins_kfold[8],
    TRUE ~ 0
  )
  bmiabove30 <- ins_test$bmi_above30*ins_kfold[9]
  
  smokerstatus <- case_when(
    ins_test$smoker == "yes" ~ ins_kfold[10],
    TRUE ~ 0
  )
  
  region <- case_when(
    ins_test$region == "southwest" ~ ins_kfold[13],
    ins_test$region == "southeast" ~ ins_kfold[12],
    ins_test$region == "northwest" ~ ins_kfold[11],
    TRUE ~ 0
  )
  
  ba30smyes <- ins_test$bmi30smokeryes*ins_kfold[14]
  
  kfold_pred <- (cat+age+children+bmi+sex+
                   bmiabove30+smokerstatus+region+ba30smyes)+ins_kfold[1]
  kfold_mse[i] <- mse(ins_test$charges, kfold_pred)
}


# leave one out validation ------------------------------------------------

ins_loocv <- lm_loocv(formula = charges ~ category + age2 + children + bmi + sex +
                        bmi_above30*smoker+ region,
                      df=ins_train,
                      predictors = 13)

# model fitting
cat <- case_when(
  ins_test$category == "Old" ~ ins_loocv[2],
  ins_test$category == "Prime" ~ ins_loocv[3],
  ins_test$category == "Young" ~ ins_loocv[4],
  TRUE ~ 0
)
age <- (ins_test$age**2)*ins_loocv[5]
children <- ins_test$children*ins_loocv[6]
bmi <- ins_test$bmi*ins_loocv[7]

sex <- case_when(
  ins_test$sex == "male" ~ ins_loocv[8],
  TRUE ~ 0
)
bmiabove30 <- ins_test$bmi_above30*ins_loocv[9]

smokerstatus <- case_when(
  ins_test$smoker == "yes" ~ ins_loocv[10],
  TRUE ~ 0
)

region <- case_when(
  ins_test$region == "southwest" ~ ins_loocv[13],
  ins_test$region == "southeast" ~ ins_loocv[12],
  ins_test$region == "northwest" ~ ins_loocv[11],
  TRUE ~ 0
)

ba30smyes <- ins_test$bmi30smokeryes*ins_loocv[14]

loocv_pred <- (cat+age+children+bmi+sex+
                 bmiabove30+smokerstatus+region+ba30smyes)+ins_loocv[1]

mse(loocv_pred, ins_test$charges)

# fitting an averaged LOOCV of predicted values directly on test data
avgloocv_pred <- lm_loocv_test(formula = charges ~ category + age2 + children + 
                                 bmi + sex + bmi_above30*smoker+ region,
              df_train = ins_train, df_test = ins_test,
              predictors = 13, return_type = "pred")


mse(avgloocv_pred, ins_test$charges)

# the mean square errors
ins_pred <- predict.lm(ins_eval_remout, ins_test)
mse_pred <- mse(ins_pred, ins_test$charges)
mse_loocv <- mse(loocv_pred, ins_test$charges)

plot(kfold_mse, type="o", pch=20, axes=F, main="MSE OF K FOLD BATCHES AND COMPARISON MODELS",
     xlab="Rounds taken by batches in k fold", ylab="Mean square error",
     sub="The red line is the single model MSE || The purple line is loocv MSE", col.sub="blue")
axis(1, at=1:20, labels=seq(5,100,5), las=2)
axis(2)
abline(h=c(mse_pred, 
           mse_loocv), 
               col=c("red", "purple"));grid()
box()

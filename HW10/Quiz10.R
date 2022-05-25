?mtcars
fit_glm = glm(am ~ mpg + hp + qsec, data = mtcars, family = binomial)
fit_null = glm(am ~ 1, data = mtcars, family = binomial)
summary(fit_glm)
predict(fit_glm, newdata = data.frame(mpg = c(22), hp = c(123), 
                                              qsec = c(18)), type = "response")

anova(fit_null, fit_glm,  test = "LRT")       


library(MASS)
Pima.tr
Pima.te
?MASS::Pima.tr

pima_glm = glm(type ~ glu + ped + I(glu ^ 2) +
               I(ped ^ 2) + glu:ped, data = Pima.tr, family = binomial)
summary(pima_glm)

dia = predict(pima_glm, newdata = Pima.te, type = "response") > 0.8
mean(dia)

pima_add_full = glm(type ~ ., data = Pima.tr, family = binomial)
red_mod = step(pima_add_full, direction = "backward")
red_mod

pima_add_full_2 = glm(type ~ . ^ 2, data = Pima.tr, family = binomial)
pima_add_full_2_both_reduced = step(pima_add_full_2, direction = "both")
summary(pima_add_full_2_both_reduced)


library(MASS)
library(boot)

# fit the models here

set.seed(42)
# get cross-validated results for the polynomial model here
cv.glm(Pima.tr, pima_glm, K = 5)$delta[1]
set.seed(42)
# get cross-validated results for the additive model here
cv.glm(Pima.tr, pima_add_full, K = 5)$delta[1]
set.seed(42)
# get cross-validated results for the model selected from additive model here
cv.glm(Pima.tr, red_mod, K = 5)$delta[1]

set.seed(42)
# get cross-validated results for the interaction model here
cv.glm(Pima.tr, pima_add_full_2, K = 5)$delta[1]


set.seed(42)
# get cross-validated results for the model selected from interaction model here
cv.glm(Pima.tr, pima_add_full_2_both_reduced, K = 5)$delta[1]

mean(ifelse(predict(pima_add_full, newdata = Pima.te) > 0, "Yes", "No") != Pima.te$type)
View(Pima.te)


make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

pima_tst_pred = ifelse(predict(pima_add_full, Pima.te) > 0, 
                       "Yes", 
                       "No")

(conf_mat_50 = make_conf_mat(predicted = pima_tst_pred, actual = Pima.te$type))

66/109

pima_tst_pred_prob = ifelse(predict(pima_add_full, Pima.te, type = "response") > 0.3, 
                       "Yes", 
                       "No")
(conf_mat_50 = make_conf_mat(predicted = pima_tst_pred_prob, actual = Pima.te$type))
87/(22 + 87)

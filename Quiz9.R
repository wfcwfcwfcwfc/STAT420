mod_lifecyclesavings = lm(sr ~ . ^ 2, data = LifeCycleSavings)
lifecyclesavings_mod_forw_bic = step(mod_lifecyclesavings, direction = "backward", k = log(n))

lifecyclesavings_mod_forw_aic = step(mod_lifecyclesavings, direction = "backward")

lifecyclesavings_mod_add = lm(sr ~ ., data = LifeCycleSavings)

calc_loocv_rmse(mod_lifecyclesavings)

calc_loocv_rmse(lifecyclesavings_mod_forw_aic)

calc_loocv_rmse(lifecyclesavings_mod_add)

summary(mod_lifecyclesavings)$adj.r.squared
summary(lifecyclesavings_mod_forw_aic)$adj.r.squared
summary(lifecyclesavings_mod_add)$adj.r.squared
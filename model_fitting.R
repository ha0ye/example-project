library(tidyverse)
library(rEDM)
library(viridis)
library(cowplot)

rm(list = ls())

f <- function(x) {
    return(sin(12*x) + rnorm(NROW(x), sd = x))
}

# make observations
set.seed(42)
num_obs <- 100
x <- runif(num_obs)
y <- f(x)
observed <- data.frame(x = x, y = y)

# actual function
num_pred <- 200
xx <- seq(from = 0, to = 1, length = num_pred)
yy <- sin(12*xx)
actual <- data.frame(x = xx, y = yy)

theta_list <- c(0, 1e-04, 3e-04, 0.001,
                0.003, 0.01, 0.03, 0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8)
smap_fit <- block_lnlp(observed, 
                       method = "s-map", theta = theta_list, tp = 0, 
                       columns = 1, target_column = 2, silent = TRUE)
best_theta <- smap_fit$theta[which.min(smap_fit$rmse)]
smap_out <- block_lnlp(rbind(observed, actual), 
                       lib = c(1, num_obs), 
                       pred = c(num_obs + 1, num_obs + num_pred), 
                       method = "s-map", theta = best_theta, tp = 0, 
                       columns = 1, target_column = 2, 
                       stats_only = FALSE, short_output = TRUE)

gp_out <- block_gp(rbind(observed, actual), 
                   lib = c(1, num_obs), 
                   pred = c(num_obs + 1, num_obs + num_pred), 
                   tp = 0, 
                   columns = 1, target_column = 2,
                   save_covariance_matrix = TRUE, 
                   stats_only = FALSE)

block <- rbind(#data_frame(x = xx, y = yy, sd = NA, model = "actual"), 
               data_frame(x = xx, y = smap_out[[1]]$model_output$pred, 
                          sd = sqrt(smap_out[[1]]$model_output$pred_var), 
                          model = "s-map"), 
               data_frame(x = xx, y = gp_out$model_output[[1]]$pred,
                          sd = sqrt(diag(gp_out$covariance_matrix[[1]])), 
                          model = "GP")) %>%
    mutate(model = as.factor(model), 
           y_low = y - sd, 
           y_high = y + sd)

observed$model = "data"

my_figure <- ggplot(block, aes(x = x, y = y, group = model, color = model)) + 
    geom_line(size = 1, linetype = 2) + 
    geom_point(data = observed, mapping = aes(x = x, y = y), color = "gray") + 
    geom_line(mapping = aes(x = x, y = y_low), size = 0.5) + 
    geom_line(mapping = aes(x = x, y = y_high), size = 0.5) + 
    theme_cowplot() + 
    scale_color_viridis(3, discrete = TRUE)

print(my_figure)  


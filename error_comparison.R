library(ggplot2)
library(viridis)
library(rEDM)
rm(list = ls())

data("block_3sp")

# Setup

## setup data for EDM
n <- NROW(block_3sp)
obs_var <- block_3sp$x_t
yy <- (obs_var - min(obs_var)) / (max(obs_var) - min(obs_var))
yy <- obs_var
my_block <- cbind(yy, 
               c(NA, yy[1:(n-1)]), 
               c(NA, NA, yy[1:(n-2)]), 
               c(NA, NA, NA, yy[1:(n-3)]))
lib <- c(51, 150)
pred <- c(152, 176)

## setup data for GP calcs
x <- my_block[lib[1]:lib[2], 2:NCOL(my_block)]
obs_x <- split(x, row(x))
obs_y <- my_block[lib[1]:lib[2], 1]

x <- my_block[pred[1]:pred[2], 2:NCOL(my_block)]
pred_x <- split(x, row(x))

# Analysis

## make simplex predictions
simplex_out <- block_lnlp(my_block, lib = lib, pred = pred, tp = 0, 
                     columns = 2:NCOL(my_block), target_column = 1, 
                     stats_only = FALSE, short_output = TRUE)[[1]]$model_output

df <- simplex_out[, c("time", "obs")]
df$simplex_pred <- simplex_out$pred
df$simplex_upper <- simplex_out$pred + sqrt(simplex_out$pred_var)
df$simplex_lower <- simplex_out$pred - sqrt(simplex_out$pred_var)

## make s-map predictions
smap_out <- block_lnlp(my_block, lib = lib, pred = pred, tp = 0, 
                       columns = 2:NCOL(my_block), target_column = 1, 
                       stats_only = FALSE, short_output = TRUE, 
                       method = "s-map", theta = 3)[[1]]$model_output
df$smap_pred <- smap_out$pred
df$smap_upper <- smap_out$pred + sqrt(smap_out$pred_var)
df$smap_lower <- smap_out$pred - sqrt(smap_out$pred_var)

##  make GP predictions
l <- 0.18 # covariance scale parameter
SE <- function(Xi, Xj) exp(-0.5 * sum((Xi - Xj) ^ 2) / l ^ 2)
cov <- function(X, Y) outer(X, Y, Vectorize(SE))

cov_xx_inv <- solve(cov(obs_x, obs_x))
Ef <- cov(pred_x, obs_x) %*% cov_xx_inv %*% obs_y
Cf <- cov(pred_x, pred_x) - cov(pred_x, obs_x)  %*% cov_xx_inv %*% cov(obs_x, pred_x)

message("SMAP: rho = ", round(cor(smap_out$pred, smap_out$obs), 3), 
        ", RMSE = ", round(sqrt(sum((smap_out$pred - smap_out$obs)^2)), 3))
message("GP:   rho = ", round(cor(Ef, smap_out$obs), 3), 
        ", RMSE = ", round(sqrt(sum((Ef - smap_out$obs)^2)), 3))


# Figure
my_colors <- viridis(4)[2:4]
my_alpha <- 0.8
my_theme <- theme(panel.grid.major = element_line(color = "#555555"),
                  panel.grid.minor = element_line(color = "#555555"),
                  axis.text = element_text(color = "black", size = 14),
                  axis.title = element_text(color = "black", size = 14),
                  legend.key = element_rect(fill = "#222222"), 
                  legend.position = "right", 
                  legend.text = element_text(size = 14), 
                  legend.title = element_text(size = 14), 
                  panel.background = element_rect(color = "black", fill = "#222222"))

my_plot <- ggplot(data = df, 
                  mapping = aes(x = time, y = obs)) + 
    labs(x = "Time", y = "Normalized Abundance") + 
    geom_ribbon(mapping = aes(ymin = simplex_upper, ymax = simplex_lower, fill = "simplex"), 
                alpha = my_alpha) + 
    # geom_line(mapping = aes(y = pred), 
    #           color = my_colors[1]) + 
    geom_ribbon(mapping = aes(ymin = smap_upper, ymax = smap_lower, fill = "smap"),
                alpha = my_alpha) +
    # geom_line(data = smap_out,
    #           mapping = aes(y = pred),
    #           color = my_colors[2]) +
    geom_line(mapping = aes(y = Ef - sqrt(diag(Cf))), 
              color = my_colors[3], linetype = 2) + 
    geom_line(mapping = aes(y = Ef + sqrt(diag(Cf))), 
              color = my_colors[3], linetype = 2) + 
    # geom_ribbon(mapping = aes(ymin = (Ef - sqrt(diag(Cf))),
    #                            ymax = (Ef + sqrt(diag(Cf)))),
    #              fill = my_colors[3], alpha = my_alpha) + 
    geom_line(mapping = aes(y = Ef, color = "gp")) + 
    geom_line(mapping = aes(color = "obs"), size = 1) + 
    scale_color_manual(guide = "legend", name = "", 
                       labels = c("Gaussian Process", 
                                  "Observed"), 
                       values = c("gp" = my_colors[3], 
                                  "obs" = "white")) + 
    scale_fill_manual(guide = "legend", name = "", 
                      labels = c("Simplex", "S-map", "Gaussian Process"), 
                      values = c("simplex" = my_colors[1], 
                                 "smap" = my_colors[2], 
                                 "GP" = my_colors[3])) + 
    my_theme
#   theme_dark(base_size = 14)

print(my_plot)
pdf("error_comparison.pdf", width = 9, height = 4.5)
print(my_plot)
dev.off()


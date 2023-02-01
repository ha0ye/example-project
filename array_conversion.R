# Make A as (3 x 4 x 5, each layer has constant value)
A <- rep(1:5, each = 12) + 10*rep(1 + 0:19 %% 4, each = 3) + 100 * rep(1:3, 20)
dim(A) <- c(3, 4, 5)

# resize A?
A_size <- dim(A)
dim(A) <- c(A_size[1], A_size[2] * A_size[3])
A <- data.frame(t(A))
A$layer_3 <- rep(, each = A_size[2])
A$layer_2 <- rep(c("a", "b", "c", "d"))
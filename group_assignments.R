xx <- c("Natercia", "Dan", "Margarita", "Ann", "Erin", "Laurie", 
        "Aida", "Colleen", "Haven", "Ginessa", "Twanna", "Suzanne")

n_groups <- 4
idx <- sample(rep(seq(n_groups), length.out = length(xx)))

yy <- list()
for(i in seq(n_groups))
{
    yy[[i]] <- xx[which(idx == i)]
}

print(yy)

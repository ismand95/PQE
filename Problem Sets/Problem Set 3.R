### Problem 1 ###
fx <- function(vector) {
    vector <- (vector**2)
    vector <- sum(vector)

    return(sqrt(vector))
}

fx(seq(69, 420))


### Problem 2 ###
set.seed(999)
generate_grid <- function(iN) {
    cell_grid <- matrix(round(runif(iN**2)), nrow = iN, ncol = iN)

    return(cell_grid)
}

expand_grid <- function(grid) {
    grid_expand <- rbind(grid, grid[1, ])
    grid_expand <- rbind(grid_expand[dim(grid_expand)[1] - 1, ], grid_expand)
    grid_expand <- cbind(grid_expand, grid_expand[, 1])
    grid_expand <- cbind(grid_expand[, dim(grid_expand)[2] - 1], grid_expand)

    # set corners
    grid_expand[1, 1] <- grid_expand[dim(grid_expand)[1] - 1, dim(grid_expand)[2] - 1]
    grid_expand[1, dim(grid_expand)[2]] <- grid_expand[dim(grid_expand)[1] - 1, 2]
    grid_expand[dim(grid_expand)[1], 1] <- grid_expand[2, dim(grid_expand)[2] - 1]
    grid_expand[dim(grid_expand)[1], dim(grid_expand)[2]] <- grid_expand[2, 2]

    return(grid_expand)
}

reduce_grid <- function(grid) {
    results <- matrix(data = 0, nrow = dim(grid)[1], ncol = dim(grid)[1])

    for (j in seq(2, dim(grid)[1] - 1)) {
        for (i in seq(2, dim(grid)[2] - 1)) {
            # select sub-matrix
            sub_matrix <- grid[(i - 1):(i + 1), (j - 1):(j + 1)]
            neigh <- sum(sub_matrix)

            if (grid[i, j] != 0) {
                results[i, j] <- neigh - 1 # dont count self
            } else {
                # dead cell comes to life
                if (neigh == 3) {
                    results[i, j] <- 3 # count self :-)
                }
            }
        }
    }

    results[results[, ] < 2 | results[, ] > 3] <- 0
    results[results[, ] > 0] <- 1

    return(results)
}

drop_grid <- function(grid) {
    grid <- grid[2:(dim(grid)[2] - 1), 2:(dim(grid)[2] - 1)]

    return(grid)
}

g <- generate_grid(10)
for (i in 1:420) {
    g <- expand_grid(g)
    g <- reduce_grid(g)
    g <- drop_grid(g)
}

g


### Problem 3 ###
binomial_recursion <- function(r, n) {
    if (n == r | r == 0) {
        return(1)
    } else {
        return(binomial_recursion(n = (n - 1), r = (r - 1)) +
               binomial_recursion(n = (n - 1), r = (r)))
    }
}

binomial_recursion(n = 10, r = 3)


### Problem 6 ###
set.seed(42069)

gen_gauss <- function(n, mu, sig) {
    gauss <- matrix(data = rnorm((n * n), mean = mu, sd = sig),
                    nrow = n,
                    ncol = n)
    gauss <- round(gauss)

    return(gauss)
}

gaus <- gen_gauss(10, 20, 100)

apply(gaus, 1, function(x) x %% 3)

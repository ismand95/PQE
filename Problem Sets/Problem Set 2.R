### Problem 1 ###
# a)
x <- 420
a <- 0.69
b <- 0.69

(z <- x**a**b)

# b)
x <- 420
a <- 0.69
b <- 0.69

(z <- (x**a)**b)

# c)
x <- 420

(z <- 3 * (x**3) + 2 * (x**2) + 6 * x + 1)

# d)
z <- 69
(z <- z + 1)


### Problem 2 ###
# a)
c(seq(1, 8), seq(7, 1))

# b)
rep(seq(1, 5), seq(1, 5))

# c)
matrix(data = 1, nrow = 3, ncol = 3) - diag(3)

# d)
matrix(data = c(0, 2, 3, 0, 4, 0, 7, 0, 0), nrow = 3, ncol = 3, byrow = TRUE)


### Problem 3 ###
x <- seq(1, 100, 1)
x <- x[!x %in% seq(0, 100, 2)]
x <- x[!x %in% seq(0, 100, 3)]
x <- x[!x %in% seq(0, 100, 7)]
(x)


### Problem 4 ###
x <- diag(10)
(x * 5) # property of identity matrix
x[x != 0] <- 5 # other way using boolean indexing
x


### Problem 5 ###
fx <- function(x) {
    if (x <= 0) {
        return((-1) * (x)**3)
    }

    if (x <= 1) {
        return((x)**2)
    }

    else {
        return(sqrt(x))
    }
}

(fx(-10))

(fx(0.5))

(fx(2))

### Problem 6 ###
fx <- function(n, x) {
    result <- 0

    for (i in seq(0, n)) {
        result <- result + (x**i)
    }

    return(result)
}

(fx(5, 2))


### Problem 7 ###
# while loop
fx <- function(n, x) {
    result <- 0
    i <- 0

    while (i <= n) {
        result <- result + (x**i)
        i <- i + 1
    }

    return(result)
}

(fx(5, 2))

# vector-approach
x <- 2
n <- 5
sum(rep(x, n + 1)**seq(0, n))


### Problem 8 ###
x <- seq(0, 100)
x <- x[seq(0, 100, 3)]
x <- sum(x)
(x)


### Problem 9 ###
minimize <- function(x) {
    result <- Inf

    for (i in x) {
        if (i < result) {
            result <- i
        }
    }
    return(result)
}

# smallest value in range(1, 100, step=1)
(minimize(seq(1:100)))


### Problem 10 ###
switches <- rep(-1, 100)

for (i in seq(1, 100)) {
    switches[seq(0, 100, i)] <- switches[seq(0, 100, i)] * (-1)
}

(which(switches > 0))


### Problem 11 ###
library(ggplot2)

# read data
df <- read.csv("MAERSK-B.CO.csv",
    sep = ",",
    dec = ".",
    header = TRUE,
    na.strings = "null"
)

# log returns
df$pct_log_returns <- c(NaN, diff(log(df$Adj.Close), lag = 1))
# drop NaN
df <- df[!is.na(df$pct_log_returns), ]

# parse date-time
df$date <- as.Date(df$Date)

# descriptive stats
mu <- mean(df$pct_log_returns)
std_lower <- mu - sqrt(var(df$pct_log_returns))
std_upper <- mu + sqrt(var(df$pct_log_returns))

ggplot(df, aes(x = date, y = pct_log_returns)) +
    geom_line() +
    geom_hline(
        yintercept = c(std_lower, std_upper),
        color = "blue",
        linetype = "dashed"
    ) +
    geom_hline(yintercept = mu, color = "red")
fibo <- function(n) {
    if (n < 2) {
        return(n)
    }

    return(fibo(n - 1) + fibo(n - 2))
}

fibo(10)

# into vector
sapply(seq(1, 20), FUN = fibo)

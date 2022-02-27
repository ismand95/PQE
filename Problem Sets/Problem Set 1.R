# Problem 1,
v1 <- c(420, 69)
v2 <- c(420, 69)

print(v1[1] + v2[2])
print(v1[1] - v2[2])
print(v1[1] * v2[2])


# Problem 2
v1 <- c(1, 2, 2, 1)
v2 <- c(2, 3, 3, 2)


print(v1 - v2)
print(v1 * v2)
print(v1 + v2)

v3 <- c(v1, v2)

print(v3)


# Problem 3
vX <- c(420,69)
mX <- matrix(vX, ncol = 2, nrow = 2, byrow = FALSE)

print(mX)


# Problem 4
mA <- matrix(c(420, 69, 0.420, 0.69), ncol = 4, nrow = 3, byrow = FALSE)
print(mA)

# 1)
maxRow <- apply(mA, 1, max)
minRow <- apply(mA, 1, min)
print(maxRow)
print(minRow)

# 2)
colSum <- apply(mA, 2, sum)
print(colSum)

# 3)
rowCumSum <- apply(mA, 1, cumsum)
print(rowCumSum)

# 4)
rowCumProd <- apply(mA, 1, cumprod)
print(rowCumProd)

# 5)
colSorted <- mA[order(mA[,1], decreasing = FALSE),]
print(colSorted)


# Problem 5
library(moments)
uniVector <- runif(250)

print( mean(uniVector) )
print( var(uniVector) )
print( skewness(uniVector) )
print( kurtosis(uniVector) )


# Problem 6
sR <- "RisGreat"

gsub("[R]", "S", sR)
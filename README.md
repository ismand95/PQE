# PQE
Code for exam in Programming in Quantitative Economics

## Problem Sets
The folder `Problem Sets` contains _my (Anders)_ solutions to the exercies.
There are likely a lot of errors and anti-patterns, so use at own discression

## How to make an R package in RStudio

### Create Package

    - Open RStudio
    - Click project -> new project
        - New Directory
        - R Package using RcppArmadillo
        - Navigate to directory and type directory name
    - Place .cpp files in ./src folder
    - Place .R files in ./R folder

### Build Package

    - Click build in upper-right panel
        - Click "more"
        - Click "Build Source Package"
        - If there's no errors, you are good to go! ;)

### Test Package

    - Click packages in bottom-right panel
    - Click install
    - Install from tar.gz file and navigate to location
    - Load library in R and test

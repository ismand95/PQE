# PQE
Code for exam in Programming in Quantitative Economics


## How to make an R package in RStudio

__Create Package__

    - Open RStudio
    - Click project -> new project
        - New Directory
        - R Package using RcppArmadillo
        - Navigate to directory and type directory name
    - Place .cpp files in ./src folder
    - Place .R files in ./R folder

__Build Package__
 
    - Click build in upper-right panel
        - Click "more"
        - Click "Build Source Package"
        - If there's no errors, you are good to go! ;)

__Test Package__

    - Click packages in bottom-right panel
    - Click install
    - Install from tar.gz file and navigate to location
    - Load library in R and test

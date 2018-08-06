### Function calculateEigenvalues.R
### Modified: 9/15/15
### Purpose: Calculate the eigenvalues of a linear differential equation

### Output:  
### phase plane diagram of linear system

# Enter in the entries of the matrix row wise, so the matrix
###   4  3
###   2  1
### would be entered in c(4,3,2,1)

matrixEntries = c(3,-1,0,3) 
### Specify the number of rows in your SQUARE matrix
matrixRows = 2

### The following code shapes the entries into a matrix for R
aMatrix = matrix(matrixEntries, nrow=matrixRows, byrow=TRUE)

### Now calculate the eigenvectors and eigenvalues
result = eigen(aMatrix)

# Note that whale_eigen is a list with two elements (denoted by the “$”), values and vectors. 
# result$values are the eigenvalues, stored as a vector. The leading eigenvalue is the first entry in the vector.
# result$vectors gives the corresponding right eigenvectors, sequentially stored as columns of a matrix (so eigenvector #1 corresponds to eigenvalue #1.

print(result)


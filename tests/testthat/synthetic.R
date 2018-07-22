## helper script to test suite
## this creates a small dataset with synthetic data


## syn0 is a manually created dataset (3 sets of 3 points)
syn0 = matrix(0, ncol=3, nrow=9)
syn0[1,] = c(0,0,0)
syn0[2,] = c(0.3, 0.2, 0.4)
syn0[3,] = c(0.2, 0.3, 0.6)
syn0[4,] = c(0.2, 0.5, 2.3)
syn0[5,] = c(0.1, 0.4, 2.1)
syn0[6,] = c(0.6, 0.2, 2.6)
syn0[7,] = c(0.2, 3.5, 0.3)
syn0[8,] = c(0.4, 3.1, 0.1)
syn0[9,] = c(0.1, 3.3, 0.5)
rownames(syn0) = letters[1:nrow(syn0)]

syn0.dist = dist(syn0)

## classic cmdscale
syn0.cmd = cmdscale(syn0.dist)


## umap ouput from python
syn0.umap = matrix(0, ncol=2, nrow=9)
syn0.umap[1,] = c(-4.86586969, 4.64761199)
syn0.umap[2,] = c( -5.12196453,   4.39334657)
syn0.umap[3,] =  c( -5.39783062,   4.11607503)
syn0.umap[4,] =  c( 15.32949595,   1.56756928)
syn0.umap[5,] =  c( 14.7533471 ,   2.13971786)
syn0.umap[6,] =  c( 15.02181159,   1.85976588)
syn0.umap[7,] =  c( -1.53737485,  -6.05376747)
syn0.umap[8,] =  c( -2.10594469,  -6.62200199)
syn0.umap[9,] =  c( -1.83912037,  -6.35611235)


## syn1 is a manually created dataset with degenerate neighbors
syn1 = matrix(0, ncol=3, nrow=1200)
syn1G1 = 100 + (1:300)
syn1G2 = 900 + (1:300)
syn1[syn1G1, 1] = 1
syn1[syn1G1, 2] = 1
syn1[syn1G1, 3] = 2
syn1[syn1G2, 1] = 3
syn1[syn1G2, 2] = 1.5
syn1[syn1G2, 3] = 1


## package umap
## functions used in umap that manipulate coo graphs



##' Create a coo representation of a square matrix
##'
##' @param x square matrix
##'
##' @return matrix with three columns (from index, to index, value)
coo = function(x) {
  
  if (class(x)!="matrix") {
    stop("x must be a square matrix\n")
  }
  if (nrow(x)!=ncol(x)) {
    stop("x must be a square matrix\n")
  }
  
  ## construct coo matrix (relies on as.vector linearizing matrix column-by-column)
  nx = nrow(x)
  coo = matrix(0, ncol=3, nrow=nx*nx)
  coo[,1] = rep(1:nx, nx)
  coo[,2] = rep(1:nx, each=nx)
  coo[,3] = as.vector(x)
  colnames(coo) = c("from", "to", "value")
  ## for convention, sort by (from, to)
  coo = coo[order(coo[,1], coo[,2]),]
  
  ## construct object
  make.coo(coo, rownames(x), nrow(x))
}




##' Helper to construct coo objects
##'
##'
##' @param x coo matrix
##' @param names character vector
##' @param n.elements integer
##'
##' @return coo object
make.coo = function(x, names, n.elements) {
  x = x[,1:3]
  colnames(x) = c("from", "to", "value")
  result = list(coo=x, names=names, n.elements=n.elements)
  class(result) = "coo"
  result
}




## ############################################################################
## Argument checking


##' Stop execution with a custom message
##'
##' @param msg1 character
##' @param msg2 character
stop.coo = function(msg1, msg2="") {
  if (msg2!="") {
    msg1 = paste0(msg1, "(", msg2, ")")
  }
  stop(paste0(msg1, "\n"), call.=FALSE)
}


##' Check class for coo
##'
##' @param x object of class coo
##' @param msg character, message to print alongside error
check.coo = function(x, msg="") {
  if (class(x)!="coo") {
    stop.coo("expecting object of class coo ", msg)
  }
}


##' Check that two coo objects are compatible for addition, multiplication
##'
##' @param x object of class coo
##' @param y object of class coos
##' @param msg character, message to print alongside error
check.compatible.coo = function(x, y, msg="") {
  if (!identical(x$n.elements, y$n.elements)) {
    stop.coo("error: incompatible coo objects (n.elements) ", msg)
  }
  if (!identical(x$names, y$names)) {
    stop.coo("error: incompatible coo objects (names)\n", msg)
  }
}




## ############################################################################
## Utility functions on coo objects


##' Remove some entires in a coo matrix where values are zero
##'
##' @param x coo object
##'
##' @return matrix based on x, perhaps with some lines in original removed
reduce.coo = function(x) {
  check.coo(x, "reduce")
  bad.rows = x$coo[, "value"] == 0 | !is.finite(x$coo[, "value"])
  x$coo = x$coo[!bad.rows,, drop=FALSE]
  rownames(x$coo) = NULL
  x
}




##' Transpose a coo matrix
##'
##' @param x coo object
##'
##' @return another coo object describing a transposed matrix
t.coo = function(x) {
  check.coo(x, "transpose")
  old.from = x$coo[, "from"]
  x$coo[,"from"] = x$coo[, "to"]
  x$coo[,"to"] = old.from
  x
}




##' Multiply two coo objects element-wise
##'
##' The two input objects must be compatible (have equivalent names)
##'
##' @param x coo object
##' @param y coo object
##' @param a numeric, scalar for multiplication
##'
##' @return new coo object with produce a*x*y
multiply.coo = function(x, y, a=1) {
  check.coo(x, "multiply")
  check.coo(y, "multiply")
  check.compatible.coo(x, y, "multiply")
  
  ## perform the multiplication using a merge on the from/to columns
  product = merge(x$coo, y$coo, by=c("from", "to"))
  product = cbind(from=product[,"from"],
                  to=product[,"to"],
                  value=a*product[,"value.x"]*product[, "value.y"])
  
  result = list(coo=product, names=x$names, n.elements=x$n.elements)
  class(result) = "coo"
  
  result        
}




##' Add two coo objects element-wise
##'
##' @param x coo object
##' @param y coo object
##' @param a numeric, scalar for addition
##' @param b numeric, scalar for addition
##' 
##' @return new coo object with (a*x) + (b*y)
add.coo = function(x, y, a=1, b=1) {
  check.coo(x, "add")
  check.coo(y, "add")
  check.compatible.coo(x, y, "add")

  added = merge(x$coo, y$coo, by=c("from", "to"), all=TRUE)
  added[is.na(added[, "value.x"]), "value.x"] = 0
  added[is.na(added[, "value.y"]), "value.y"] = 0
  added = cbind(from=added[, "from"],
                to=added[, "to"],
                value=a*added[, "value.x"] + b*added[, "value.y"])
  result = list(coo=added, names=x$names, n.elements=x$n.elements)
  class(result) = "coo"
  
  result
}



##' Matrix multiplication of a coo matrix with a vector
##'
##' @param x coo object
##' @param v numeric vector
##'
##' @return new vector x*v
vectormultiplication.coo = function(x, v) {
  check.coo(x, "matrixveector")
  if (length(v)!= x$n.elements) {
    stop.coo("incompatible multiplication")
  }

  ## split the coo matrix by "from"
  x.to = split(x$coo[, "to"], x$coo[, "from"])
  x.value = split(x$coo[, "value"], x$coo[, "from"])
  
  ## perform the multiplication
  result = rep(0, length(v))
  for (i in names(x.to)) {
    xi = x[[i]]
    index.from = as.integer(i)
    result[index.from] = sum(v[x.to[[i]]]*x.value[[i]])
  }
  names(result) = names(v)
  result
}

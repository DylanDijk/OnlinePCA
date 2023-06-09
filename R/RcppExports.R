# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Algorithm 1 for Online PCA
#'
#' @param x1 numeric vector
#' @param x2 numeric vector
#' @return dot product, that is \code{t(x1)%*%x2}
algorithm1 <- function(x, k, eps) {
    .Call(`_onlinePCA_algorithm1`, x, k, eps)
}

rcpparma_hello_world <- function() {
    .Call(`_onlinePCA_rcpparma_hello_world`)
}

rcpparma_outerproduct <- function(x) {
    .Call(`_onlinePCA_rcpparma_outerproduct`, x)
}

rcpparma_innerproduct <- function(x) {
    .Call(`_onlinePCA_rcpparma_innerproduct`, x)
}

rcpparma_bothproducts <- function(x) {
    .Call(`_onlinePCA_rcpparma_bothproducts`, x)
}


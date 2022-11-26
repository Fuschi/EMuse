#' Get adjacency with the p-adjusted values from correlations
#' 
#' @description User wrapper of \code{\link{corr.test}} to get the 
#' adjacency from correlation matrix. The function apply a
#' threshold on correlation matrix using the adjusted p-value with multiple test
#' corrections.
#' 
#' @param x (Required) matrix of data.
#' @param method (Optional) correlation type, possible choices are "pearson",
#' "spearman" or "kendall" (default spearman).
#' @param adjust (Optional) multiple test choosen, possible choices are "holm",
#' "hochberg","hommel","bonferroni","BH","BY","fdr","none".
#' @param alpha (Optional) level of confidence interval (default 0.01).
#' @param keep.psych (Optional) logical paramater which indicates if return the
#' results of corr.test (default FALSE).
#' 
#' @importFrom psych corr.test
#' @export
adjacency_p_adjust <- function(x,method="pearson",adjust="bonferroni",alpha=.01,
                            keep.psych=FALSE){
  
  method <- match.arg(method, c("pearson","spearman","kendall"))
  adjust <- match.arg(adjust, c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"))
  if(!is.numeric(alpha)) stop("alpha must be a number")
  if(alpha<0 | alpha>1) stop("alpha must be a number in range [0,1]")
  if(!is.logical(keep.psych)) stop("keep.psych must be logical")
  
  result <- psych::corr.test(x=x,use="pairwise",method=method,adjust=adjust,
                             alpha=alpha,ci=FALSE)
  pmat <- triu2mat(result$p.adj,diag=1)
  adj <- result$r*(pmat<=alpha)
  
  if(keep.psych){
    return(list("adj"=adj,
                "psyc"=result))
  } else {
    return(adj)
  }
}
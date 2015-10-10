## Pre-define the S3 class so that the `R CMD BUILD` command does not yell
## at us when it checks the type signatures of methods like `getinfo` below.
setClass('xgb.DMatrix')

#' Get information of an xgb.DMatrix object
#' 
#' Get information of an xgb.DMatrix object
#' 
#' The information can be one of the following:
#' 
#' \itemize{
#'     \item \code{label}: label Xgboost learns from ;
#'     \item \code{weight}: to do a weight rescale ;
#'     \item \code{base_margin}: base margin is the base prediction Xgboost will boost from ;
#'     \item \code{nrow}: number of rows of the \code{xgb.DMatrix}.
#' }
#' 
#' @examples
#' data(agaricus.train, package = "xgboost")
#' train   <- agaricus.train
#' dtrain  <- xgb.DMatrix(train$data, label = train$label)
#' labels  <- getinfo(dtrain, "label")
#' setinfo(dtrain, "label", 1 - labels)
#' labels2 <- getinfo(dtrain, "label")
#' stopifnot(all(labels2 == 1 - labels))
#' @rdname getinfo
#' @export
getinfo <- function(object, name) {
  UseMethod("getinfo")
}

#' @param object Object of class \code{xgb.DMatrix}
#' @param name character. Name of the information field. Can be one
#'    of \code{c("label", "weight", "base_margin", "nrow")}.
#' @rdname getinfo
#' @method getinfo xgb.DMatrix
setMethod("getinfo", signature = "xgb.DMatrix", definition = function(object, name) {
  if (!is.simple_string(name)) {
    stop(sprintf("When calling %s, %s must be length 1 character",
                 sQuote("xgb.getinfo"), sQuote("name")),
         call. = FALSE)
  }
  
  if (!is(object, "xgb.DMatrix")) {
    stop(sprintf("When calling %s, first argument %s must be %s",
                 sQuote("xgb.getinfo"), sQuote("dtrain"), sQuote("xgb.DMatrix")),
                 call. = FALSE)
  }

  name <- match.arg(tolower(name), c("label", "weight", "base_margin", "nrow"))

  if (name == "nrow"){
    xgb.numrow(object)
  } else {
    .Call("XGDMatrixGetInfo_R", object, name, PACKAGE = "xgboost")
  }
})


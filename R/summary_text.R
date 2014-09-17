#' Summary Text
#'
#' This function creates summary paragraphs for htest (hypothesis test) objects.
#' @param test htest object that you want to summarize
#' @export
#' @examples
#' summary_text(prop.test(x = 500,n = 1005))
summary_text <- function(test){
  stmt <- NULL
  
  stmt <- paste0(
    "This was a ",
    test$method,
    " testing against the null hypothesis that the true population proportion is equal to ",
    test$null.value,
    ". The number of observations is ",
    test$data.name
  )
  
  return(stmt)
  
  # figure out how to make [1] go away.
  # todo: flesh out for one sample prop test
  # then add two sample prop test
  # then do one sample t-test
  # then two-sample t-text
  # will need to distinguish between these (maybe by header?)
}
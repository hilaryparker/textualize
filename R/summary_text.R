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
    test$data.name,
    '.
    
    The p-value for this test is ',
    test$p.value,
    '. This, formally, is defined as the probability of observing a sample proportion that is as or more extreme than the observed sample proportion, assuming that the null hypothesis is true. In this case, this is the probability of observing a sample proportion that is greater than ',
    test$null.value + abs(test$estimate - test$null.value),
    ' or less than ',
    test$null.value - abs(test$estimate - test$null.value), 
    ' asumming the null hypothesis (that the population proportion is ',
    test$null.value,
    ') were really true. A smaller p-value can be used to reject the null hypothesis, and conclude that the data are not consistent with a true population proportion of ', 
    test$null.value,
    '. A common cutoff convention is a p-value of less than 0.05.'
  )
  
  return(stmt)
  
  # figure out how to make [1] go away.
  # todo: flesh out for one sample prop test
  # then add two sample prop test
  # then do one sample t-test
  # then two-sample t-text
  # will need to distinguish between these (maybe by header?)
}

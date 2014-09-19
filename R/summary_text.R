#' Summary Text
#'
#' This function creates summary paragraphs for htest (hypothesis test) objects.
#' @param test htest object that you want to summarize
#' @export
#' @examples
#' summary_text(prop.test(x = 500,n = 1005))
summary_text <- function(test, alpha=0.05){
  stmt <- NULL
  
  stmt <- paste0(
    "This was a one-sample proportion test of the null hypothesis that the true population proportion is equal to ",
    test$null.value,
    ". Using a significance cut-off of ",
    alpha,
    ", we ",
    if (test$p.value < alpha) {
      "reject the null hypothesis, and conclude that the true population proportion is different than "
    } else {
      "do not reject the null hypothesis, and cannot conclude that the true population proportion is different than "
    }
    ,test$null.value,
    ". The observed number of events is ",
    prettyNum(unlist(strsplit(tmp$data.name, " "))[1], big.mark=","),
    ", out of a total sample size of ",
    prettyNum(unlist(strsplit(unlist(strsplit(tmp$data.name, ","))[1], " "))[4], big.mark=","),
    '. 

    The confidence interval for the true population proportion is (',
    test$conf.int[1],
    ', ',
    test$conf.int[2],
    '). That is, 95 times out of 100, this interval will contain the true population proportion.
        
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

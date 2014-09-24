#' Summary Text
#'
#' This function creates summary paragraphs for htest (hypothesis test) objects.
#' @param test htest object that you want to summarize
#' @export
#' @importFrom dplyr %>%
#' @examples
#' summary_text(prop.test(x = 500,n = 1005))
summary_text <- function(test, alpha=0.05){
  
  if (class(test) != "htest") stop ("Not a hypothesis test (htest) object.")
  
  stmt <- NULL
  onesamp <- TRUE  
  if (length(test$estimate) == 2) onesamp <- FALSE
  
  
  # make description paragraph ----------------------------------------------

  if (onesamp == TRUE) {
    stmt <- paste0(
      "This was a one-sample proportion test of the null hypothesis that the ",
      "true population proportion is equal to ", test$null.value, ". Using a ",
      "significance cut-off of ", alpha, ", we "
    )
  }
  
  if (onesamp == FALSE) {
    stmt <- paste0(
      "This was a two-sample proportion test of the null hypothesis that the ",
      "two population proportions are equal to each other. Using a ",
      "significance cut-off of ", alpha, ", we "
    )
  }
  
  if (test$p.value < alpha) {
    stmt <- paste0(
      stmt, "reject the null hypothesis, and conclude that the ")
    
    if (onesamp == TRUE) {
      stmt <- paste0(
        stmt, "true population proportion is different than ", test$null.value) 
    }
    
    if (onesamp == FALSE) {
      stmt <- paste0(
        stmt, "two population proportions are not equal") 
    }
    
  } 
  if (test$p.value >= alpha){
    stmt <- paste0(
      stmt, "do not reject the null hypothesis, and cannot conclude that the "
    )
    
    if (onesamp == TRUE) {
      stmt <- paste0(
        stmt, "true population proportion is different than ", test$null.value
      ) 
    }
    
    if (onesamp == FALSE) {
      stmt <- paste0(
        stmt, "two population proportions are different from one another"
      ) 
    }
  }

  if (onesamp == TRUE) {
    
    samps <- strsplit(test$data.name, ",") %>%
      unlist() %>%
      strsplit(split = " ") %>%
      unlist()
    
    x <- as.numeric(samps[1])
    n <- as.numeric(samps[4])
        
    stmt <- paste0(
      stmt, ". The observed number of events is ",
      prettyNum(x, big.mark=","),
      ", out of a total sample size of ",
      prettyNum(n, big.mark=","),
      '.'
    )    
  }
  
  if (onesamp == FALSE) {

    samps <- strsplit(test$data.name, "\\(" ) %>%
      unlist() %>%
      strsplit(split="\\)") %>%
      unlist() %>%
      strsplit(split=",") %>%
      unlist()
    
    x1 <- as.numeric(samps[2])
    x2 <- as.numeric(samps[3])
    n1 <- as.numeric(samps[5])
    n2 <- as.numeric(samps[6])    
    
    stmt <- paste0(
      stmt, ". The observed number of events in the first group is ",
      prettyNum(x1, big.mark=","), ", out of a total sample size of ",
      prettyNum(n1, big.mark=","), ". For the second group, the observed number of events is ",
      prettyNum(x2, big.mark=","), ", out of a total sample size of ",
      prettyNum(n2, big.mark=","), "."
    )
    
    
  }


  # make CI paragraph ---------------------------------------------------------
  
  stmt <- paste0(stmt, "\n\nThe confidence interval for the ")

  if(onesamp == TRUE) stmt <- paste0(stmt, "true population proportion")
  if(onesamp == FALSE) stmt <- paste0(stmt, "true difference in population proportions")

  stmt <- paste0(
    stmt, "is (",
    test$conf.int[1], ', ', test$conf.int[2], '). That is, 95 times out of 100, ',
    "this interval will contain the "
  )

  if(onesamp == TRUE) stmt <- paste0(stmt, "true population proportion.")
  if(onesamp == FALSE) stmt <- paste0(stmt, "true difference in population proportions.")

#### need to two-samp-ify below here

  # make hypothesis test paragraph ---------------------------------------------
  
  ################################################################################
  stmt <- paste0(
    stmt, '\n\nThe p-value for this test is ', test$p.value, '. This, formally',
    ', is defined as the probability of observing a sample proportion that is ',
    'as or more extreme than the observed sample proportion, assuming that the ',
    'null hypothesis is true. In this case, this is the probability of ',
    'observing a sample proportion that is greater than ',
    test$null.value + abs(test$estimate - test$null.value), ' or less than ',
    test$null.value - abs(test$estimate - test$null.value), ' asumming the null',
    'hypothesis (that the population proportion is ', test$null.value, ') were ',
    'really true. A smaller p-value can be used to reject the null hypothesis, ',    
    'and conclude that the data are not consistent with a true population ',
    'proportion of ', test$null.value, '. A common cutoff convention is a ',
    'p-value of less than 0.05.'
  )
  
  return(cat(stmt))
  
  # todo: 
  # add two sample prop test
  # then do one sample t-test
  # then two-sample t-text
  # will need to distinguish between these (maybe by header?)
}

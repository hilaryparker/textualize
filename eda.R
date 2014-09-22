library(dplyr)
library(devtools)

setwd("~/Desktop")
document('./textualize')
install('textualize')


setwd("~/Desktop/textualize/")

# one samp, do not reject
tmp <- prop.test(x=500, n=1008, correct=FALSE)
summary_text(tmp)

# one samp, reject
tmp <- prop.test(x=100, n=1008, correct=FALSE)
summary_text(tmp)

# two samps, do not reject
tmp <- prop.test(x=c(500,501), n=c(1008,1002), correct=FALSE)
summary_text(tmp)

# two samps, reject
tmp <- prop.test(x=c(500,600), n=c(1008,1050), correct=FALSE)
summary_text(tmp)



tmp$statistic  
tmp$parameter	
tmp$p.value	
tmp$conf.int	
tmp$estimate	
tmp$null.value	
tmp$alternative	
tmp$method	
tmp$data.name	

tmp$null.value - abs(tmp$estimate - tmp$null.value)

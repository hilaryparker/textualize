tmp <- prop.test(x=500, n=1008, correct=FALSE)
summary_text(tmp)


unlist(strsplit(tmp$data.name, ","))

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

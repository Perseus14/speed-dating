library("arules");
library("arulesViz");
options(bitmapType="cairo")
for(name in c("female_male.csv"))#,"male_female.csv"))
{
	print(name)
	data=read.csv(name)
#data<-na.omit(data)
	
	for (n in colnames(data)[31:36])
	{
	    print(head(data[[n]]))
	    m = mean(data[[n]], na.rm=TRUE)
	    #str=sprintf("c_%s", n)    
	    data[[n]] <- cut(data[[n]], c(0, (2.0/3.0)*m, (4.0/3.0)*m, 100), labels=c("L", "M", "H"))
	}

	for(i in 9:30)
	{
		data[i]<-cut(as.matrix(data[i]), seq(0,10,3), right=FALSE, labels=c("L", "M", "H"))
	}	
	#print(str(data))
	data <- cbind(data[9:30],data[31:36],data[59],data[60])
	data<-data.frame(sapply(data,as.factor))
	if(name!="female_male.csv")
	{
		rules <- apriori(data,parameter = list(support = 0.005, confidence = 0.85),appearance = list(rhs=c("dec_o=0"),default="lhs"))
	}
	else
	{
		rules <- apriori(data,parameter = list(support = 0.01, confidence = 0.8),appearance = list(rhs=c("dec_o=0"),default="lhs"))
	}
	rules.sorted <- sort(rules, by="lift")
	inspect(head(rules.sorted,10));
	setEPS()
	cairo_ps("f_rules_out.eps")	
	plot(rules)
	dev.off()
	cairo_ps("f_graph_out.eps")	
	plot(head(sort(rules, by = "lift"), n=20), method = "graph", control=list(cex=.8))	
	dev.off()
	#subrules <- rules[quality(rules)$confidence > 0.8]
	#plot(subrules, method="matrix", measure="lift")
	setEPS()
	cairo_ps("f_group_out.eps")		
	plot(rules, method="grouped")
	dev.off()
	setEPS()
	subrules2 <- head(sort(rules, by="lift"), 20)
	cairo_ps("f_para_out.eps")	
	plot(subrules2, method="paracoord")
	dev.off()
	if(FALSE)
	{
	subset.matrix <- is.subset(rules.sorted, rules.sorted)
	subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
	redundant <- colSums(subset.matrix, na.rm=T) >= 1
#which(redundant)
# remove redundant rules
	rules.pruned <- rules.sorted[!redundant]
	inspect(head(rules.pruned,10))
	}
}
getOption("bitmapType")

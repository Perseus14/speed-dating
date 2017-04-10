library("arules");
library("arulesViz");
options(warn=0)
patterns = random.patterns(nItems = 1000);
#summary(patterns);
 
trans = random.transactions(nItems = 1000, nTrans = 1000, method = "agrawal",  patterns = patterns);
#image(trans);

#data("AdultUCI");
#head(AdultUCI)

data=read.csv("f_m.csv")
data<-na.omit(data)
data<-data.frame(sapply(data,as.factor))

#AdultUCI <- data.frame(sapply(AdultUCI,as.factor))
#Adult = as(AdultUCI, "transactions");
Adult = data
rules <- apriori(data,parameter = list(support = 0.01, confidence = 0.5),appearance = list(rhs=c("dec_o=0", "dec_o=1"),default="lhs"))
#rules;
#print("Hello")
#print(rules)

inspect(head(sort(rules, by="confidence"),10));

if(FALSE)
{
plot(rules);
head(quality(rules));
plot(rules, measure=c("support","lift"), shading="confidence");
plot(rules, shading="order", control=list(main ="Two-key plot"))

sel = plot(rules, measure=c("support","lift"), shading="confidence", interactive=TRUE);
}
subrules = rules[quality(rules)$confidence > 0.8];
inspect(head(sort(rules, by="confidence"),10));

if(FALSE)
{
plot(subrules, method="matrix", measure="lift"); 
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE));
plot(subrules, method="matrix3D", measure="lift");
plot(subrules, method="matrix3D", measure="lift", control = list(reorder=TRUE));
plot(subrules, method="matrix", measure=c("lift", "confidence"));
plot(subrules, method="matrix", measure=c("lift","confidence"), control = list(reorder=TRUE));
plot(rules, method="grouped");
plot(rules, method="grouped", control=list(k=50));
sel = plot(rules, method="grouped", interactive=TRUE);
}

subrules2 = head(sort(rules, by="lift"), 30);

if(FALSE)
{
plot(subrules2, method="graph");
plot(subrules2, method="graph", control=list(type="items"));
plot(subrules2, method="paracoord");
plot(subrules2, method="paracoord", control=list(reorder=TRUE));
}

oneRule = sample(rules, 10); 
inspect(oneRule);

#itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8);
fsets = eclat(trans, parameter = list(support = 0.05), control = list(verbose=FALSE));
singleItems = fsets[size(items(fsets)) == 1];
singleSupport = quality(singleItems)$support;
names(singleSupport) = unlist(LIST(items(singleItems), decode = FALSE));
head(singleSupport, n = 5);
itemsetList = LIST(items(fsets), decode = FALSE);
allConfidence = quality(fsets)$support / sapply(itemsetList, function(x)
max(singleSupport[as.character(x)]));
quality(fsets) = cbind(quality(fsets), allConfidence);
summary(fsets);


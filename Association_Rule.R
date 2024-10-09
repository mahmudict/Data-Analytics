library(arules)
library(arulesViz)

data("Groceries")
summary(Groceries)
itemLabels(Groceries)
inspect(Groceries)
itemFrequencyPlot(Groceries, topN=20,  cex.names=1)

image(Groceries)

itemsets <- apriori(Groceries, parameter = list(minlen=1,  support=0.2, target="frequent itemsets"))
summary(itemsets)
inspect(itemsets)


grocery_rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5))
summary(grocery_rules)
inspect(grocery_rules)
plot(grocery_rules)
plot(grocery_rules, method = "graph", limit = 20)


subrules <- head(grocery_rules, n = 10, by = "confidence")
plot(subrules, method = "graph",  engine = "htmlwidget")



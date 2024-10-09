library(arules)

# Define your transactions
transaction_list <- list(
  c("Chips", "Cookies", "Regular Soda", "Ham"),
  c("Chips", "Ham", "Boneless Chicken", "Diet Soda"),
  c("Ham", "Bacon", "Whole Chicken", "Regular Soda"),
  c("Chips", "Ham", "Boneless Chicken", "Diet Soda"),
  c("Chips", "Bacon", "Boneless Chicken"),
  c("Chips", "Ham", "Bacon", "Whole Chicken", "Regular Soda"),
  c("Chips", "Cookies", "Boneless Chicken", "Diet Soda")
)

# Convert transactions to a transaction object
transactions <- as(transaction_list, "transactions")

# Mine frequent itemsets for length 2 to 4
frequent_itemsets <- apriori(transactions, parameter = list(support = 0.5, minlen = 2, maxlen = 4))

# Find single-item frequent itemsets
#single_itemsets <- itemFrequency(transactions)
#single_itemsets <- names(single_itemsets[single_itemsets >= 0.5])

# Create transactions for single-item frequent itemsets
#single_transactions <- as(sapply(single_itemsets, function(x) { list(x) }), "transactions")

#inspect(single_transactions)
inspect(frequent_itemsets)

# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi
#
#
# Perform chi-square tests for proportions before and after retraction
#
#
# PREPARATIONS #################################################################

# TESTING ######################################################################

# The Chi-Square test, often written as χ2, is a non-parametric test that evaluates whether there is a significant association between two categorical variables. It compares the observed frequencies in each category to the expected frequencies, assuming the null hypothesis of independence.
# Our assumptions are:
# Data must be in the form of counts or frequencies for each combination of categories in the variables.
# Each cell should have an expected frequency of at least 5 to ensure the Chi-Square approximation is valid. If this assumption is not met, consider using Fisher’s Exact Test.
# Observations should be independent of each other, unlike the McNemar’s test, where we took pairs, although each pair is still independent from other pairs. This means each participant or observation should belong to only one category combination.
# First one is the Pearson’s Chi-Square Test of Independence, which is used to assess if there is a relationship between two categorical variables in a contingency table.
# First, we need to create a contingency table with one variable as the rows and the other as the columns.

# H0: Variables (editorial decision and correction type) are independent from another
# Ha: Variables are associated

# Placeholder data

set.seed(300)

# Create data
df <- data.frame(
  stage = sample(c("pre-retraction", "post-retraction"), 100, replace = T),
  category = sample(c("correction", "other"), 100, replace = T))

# Tabulate the data
contigency_table <- table(df$stage, df$category)
contigency_table

chi_square_test <- chisq.test(contigency_table)
chi_square_test

# Print list components
chi_square_test$statistic
chi_square_test$p.value
chi_square_test$observed
chi_square_test$expected
chi_square_test$residuals

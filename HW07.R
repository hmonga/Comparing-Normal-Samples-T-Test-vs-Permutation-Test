library(psych)
library(EnvStats)

set.seed(12345)
sample1 = rnorm(10, mean = 4, sd = 1)
sample2 = rnorm(10, mean = 4.4, sd = 1)

par(mfrow = c(1,2))
qqnorm(sample1, main = "Q-Q Plot 1")
qqline(sample1, main = "red")

qqnorm(sample2, main = "Q-Q Plot  2")
qqline(sample2, col = "blue")

t_test_result = t.test(sample1, sample2, alternative = "two.sided")
p_value_t_test = t_test_result$p.value
print(paste("Two-sample t-test p-value:", p_value_t_test))


num_permutations = choose(20,10)

perm_test_result = twoSamplePermutationTestLocation(
  x = sample1,
  y = sample2,
  fcn = "mean", 
  alternative = "two.sided",
  mu1.minus.mu2 = 0,
  paired = FALSE, exact = FALSE, 
  n.permutations = num_permutations, 
  seed = 123
)

p_value_perm_test = perm_test_result$p.value 
print(paste("Permutation test p-value", p_value_perm_test))
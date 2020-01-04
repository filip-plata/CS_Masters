N = 110
p = (0.76 - 0.667)/ 0.36
probs = matrix(c(0.4 * p, (1-0.4) * p, 0.76*(1-p), (1-0.76) *(1-p)), ncol=2)
contingency <- round(probs * N)

row.sums <- apply(contingency, 1, sum)
col.sums <- apply(contingency, 2, sum)

expectation <- (contingency - (row.sums %*% t(col.sums)) / (sum(contingency))) ^ 2 / (row.sums %*% t(col.sums) / sum(contingency))

chi.stat <- sum(expectation)
pchisq(chi.stat, df=4)

chisq.test(contingency, correct=F)

# Białka

protein = read.csv('Protein_length_data.csv')

tapply(protein$Protein_length, protein$Organism, mean)
t.test(Protein_length~Organism, protein)
wilcox.test(Protein_length~Organism, protein)

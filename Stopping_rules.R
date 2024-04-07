# Install development version from GitHub
#devtools::install_github("mcallaghan/buscarR")
library(buscarR)

N <- 60000 # number of documents
prevalence <- 0.01 # prevalence of relevant documents
r <- N*0.01 # number of relevant documents
bias <- 10


docs <- rep(0,N)
docs[1:r] <- 1
weights = rep(1,N)
weights[1:r] <- bias
set.seed(2023)
docs <- sample(
  docs, prob=weights, replace=F
)

df <- data.frame(relevant=docs)

plot(
  cumsum(df$relevant),
  type='l',
  main="Simulation of ML-prioritised screening",
  xlab="Documents seen",
  ylab="Relevant  documents seen"
)

df$seen <- 0
df$seen[1:20000] <- 1
plot(
  cumsum(df[df$seen==1,"relevant"]),
  type='l',
  main="Simulation of ML-prioritised screening",
  xlab="Documents seen",
  ylab="Relevant  documents seen",
  xlim=c(0,nrow(df))
)

p <- calculate_h0(df)
p

df$seen <- 0
df$seen[1:30000] <- 1
plot(
  cumsum(df[df$seen==1,"relevant"]),
  type='l',
  main="Simulation of ML-prioritised screening",
  xlab="Documents seen",
  ylab="Relevant  documents seen",
  xlim=c(0,nrow(df))
)
p <- calculate_h0(df)
p


1779/10402
N=9890 # number of documents
prevalence <- 1779/10402 # prevalence of relevant documents
r <- N*prevalence # number of relevant documents
bias <- 10
docs <- rep(0,N)
docs[1:r] <- 1
weights = rep(1,N)
weights[1:r] <- bias
set.seed(2023)
docs <- sample(
  docs, prob=weights, replace=F
)

df <- data.frame(relevant=docs)

plot(
  cumsum(df$relevant),
  type='l',
  main="Simulation of ML-prioritised screening",
  xlab="Documents seen",
  ylab="Relevant  documents seen"
)

abline(v=5500, col="red", lty=2)

df$seen <- 0
df$seen[1:5500] <- 1

p <- calculate_h0(df)
p




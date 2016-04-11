library("bnlearn")

source("00.includes.R")

ys = array(paste("y", 1:n.ys, sep=""))
names(ys) = ys

xs = array(paste("x", 1:n.xs, sep=""))
names(xs) = xs

zs = array(paste("z", 1:n.zs, sep=""))
names(zs) = zs

vars.dimnames = c(
  lapply(xs, function(x) {factor(paste((1:x.dim)-1))}),
  lapply(ys, function(y) {factor(paste((1:y.dim)-1))}),
  lapply(zs, function(z) {factor(paste((1:z.dim)-1))})
)

bn.fit = vector(mode="list", length=n.reps)

for (lp.size in lp.sizes) {
  
  ms = paste("[", xs, "]", sep="", collapse="")
  for (i in 1:n.ys) {
    ms = sprintf("%s[%s|%s]",
                 ms, ys[i], paste(c(xs,
                                    # Yi->Yj, forall i<j
                                    ys[i-(((i-1)%%lp.size+1):1)[-1]]
#                                     # Yi->Yi+1 only
#                                     ys[i-((min((i-1)%%lp.size, 1)+1):1)[-1]]
                 ), collapse=":"))
  }
  
  # add noisy input
  ms = sprintf("%s%s", ms, paste("[", zs, "]", sep="", collapse=""))
  
  bn = model2network(ms)
  set.seed(seed)
  
  for (r in 1:n.reps) {
    
    cat(sprintf("make.bn bn.lp%i %i / %i\n", lp.size, r, n.reps))
    
    dist = random.dist(bn, vars.dimnames)
    bn.fit[[r]] = custom.fit(bn, dist)
  }
  
  dir.create("bns", showWarnings=FALSE, recursive=TRUE)
  save(bn.fit, file=sprintf("bns/%ix%i.%iz%i.%iy%i.bn.lp%i.seed%i.reps%04i.rda", n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, lp.size, seed, n.reps))
}

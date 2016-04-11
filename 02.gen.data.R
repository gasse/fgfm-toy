library("bnlearn")

for (n.train in n.train.reps) {
  for (bn.t in bn.types) {
    
    load(sprintf("bns/%ix%i.%iz%i.%iy%i.bn.%s.seed%i.reps%04i.rda", n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, bn.t, seed, n.reps))
    
    set.seed(seed)
    
    for (r in 1:n.reps) {
    
      cat("gen.data bn.", bn.t, " (train=", n.train, " test=", n.test, ") ", r, "/", n.reps, "\n", sep="")
      
      d = rbn(bn.fit[[r]], n.train + n.test)
      
      train = d[1:n.train, ]
      test = d[n.train + (1:n.test), ]
      
      out.dir = sprintf("data/%ix%i.%iz%i.%iy%i.bn.%s.seed%i", n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, bn.t, seed)
      out.file = sprintf("%s/train%04i.test%04i.r%04i.rda", out.dir, n.train, n.test, r)
      
      dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
      save(train, test, file=out.file)
    }
  }
}
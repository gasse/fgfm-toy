library("bnlearn")

ys = array(paste("y", 1:n.ys, sep=""))
names(ys) = ys

xs = array(paste("x", 1:n.xs, sep=""))
names(xs) = xs

zs = array(paste("z", 1:n.zs, sep=""))
names(zs) = zs

for (n.train in n.train.reps) {

  ydeps = array(NA, dim=c(length(ys), length(ys), n.reps),
                dimnames=list(y1=ys, y2=ys, rep=1:n.reps))
  
  ymb = array(NA, dim = c(length(ys), length(xs)+length(zs), n.reps),
                dimnames=list(y=ys, x=c(xs, zs), rep=1:n.reps))
  
  for (bn.t in bn.types) {
  
    set.seed(seed)
    
    for (r in 1:n.reps) {
      
      cat("learn.ydeps ", bn.t, ".bn (train=", n.train, " test=", n.test, ") ", r, "/", n.reps, "\n", sep="")
      
      data.file = sprintf("data/%ix%i.%iz%i.%iy%i.bn.%s.seed%i/train%04i.test%04i.r%04i.rda",
                          n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, bn.t, seed, n.train, n.test, r)
      
      load(data.file)
      
      for (y1 in ys) {
        
        mb = learn.mb(
          x=train[, c(ys[y1], xs, zs)], node=ys[y1],
          method="iamb",
          test=ci.test,
          alpha=ci.alpha,
          test.args=ci.test.args)
        
        for (y2 in setdiff(ys, y1)) {
          ci = ci.test(x=y1, y=y2, z=mb, data=train, test=ci.test, test.args=ci.test.args)
          ydeps[y1, y2, r] = ci$p.value
        }
        
        ymb[y1, c(xs, zs), r] = c(xs, zs) %in% mb
      }
    }
    
    out.dir = sprintf("ydeps/%ix%i.%iz%i.%iy%i.bn.%s.seed%i", n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, bn.t, seed)
    out.file = sprintf("%s/ydeps.train%04i.test%04i.rda", out.dir, n.train, n.test, r)
    
    dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
    save(ydeps, ymb, file=out.file)
  }
}

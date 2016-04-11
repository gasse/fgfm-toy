
for (n.train in n.train.reps) {
  for (bn.t in bn.types) {
    
    set.seed(seed)
    
    loss = array(rep(NA, n.reps*(n.ys+1)*3), dim=c(3, 4, n.reps),
                 dimnames=list(loss=c("hloss", "sloss", "f1"), method=c("i-gfm", "gfm", "f-gfm", "t-gfm"), rep=1:n.reps))
    
    time = array(rep(NA, n.reps*(n.ys+1)*3), dim=c(2, 4, n.reps),
                 dimnames=list(loss=c("train", "pred"), method=c("i-gfm", "gfm", "f-gfm", "t-gfm"), rep=1:n.reps))
    
    factors.true = list()
    lp.size = as.integer(substring(bn.t, 3))
    for (k in 1:ceiling(n.ys / lp.size)) {
      factors.true[[k]] = (lp.size * (k-1)+1):min((lp.size * (k)), n.ys)
    }
    
    factors.all.ind = as.list(1:n.ys)
    
    for (r in 1:n.reps) {
      
      cat("run.models bn.", bn.t, " (train=", n.train, ") ", r, "/", n.reps, "\n", sep="")
      
      load(sprintf("data/%ix%i.%iz%i.%iy%i.bn.%s.seed%i/train%04i.test%04i.r%04i.rda",
                   n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, bn.t, seed, n.train, n.test, r))
      
      xs = which(colnames(train) %in% c(paste("x", 1:n.xs, sep=""), paste("z", 1:n.zs, sep="")))
      ys = which(colnames(train) %in% paste("y", 1:n.ys, sep=""))
      
      features = colnames(train)[xs]
      labels = colnames(train)[ys]
      
      labels = as.array(labels)
      names(labels) = labels
      
      learner = "multinom"
      
      train.true = train[, ys, drop=F]
      test.true = test[, ys, drop=F]
      
      # GFM
      
      t = proc.time()
      cat(" GFM...")
      
      factors = list(labels)
      factors = lapply(factors, function(x) {vapply(x, function(x) {which(colnames(train[, ys, drop=F]) == x)}, integer(1))})
      
      train.t = proc.time()
      model = learn.fgfm(train[, xs, drop=F], train[, ys, drop=F], factors, learner)
      train.t = proc.time() - train.t
      
      pred.test.t = proc.time()
      test.pred = pred.fgfm(model, test[, xs, drop=F])
      colnames(test.pred) = colnames(train[, ys, drop=F])
      pred.test.t = proc.time() - pred.test.t
      
      loss[c("hloss", "sloss", "f1"), "gfm", r] = eval.pred(test.true, test.pred)[c("hloss", "sloss", "f1")]
      time[c("train", "pred"), "gfm", r] = c(sum(train.t[c("user.self", "user.child")]),
                                             sum(pred.test.t[c("user.self", "user.child")]))
      
      t = proc.time() - t
      cat(" done (", sum(t[c("user.self", "user.child")]), "/", t["elapsed"], " sec)\n", sep="")
      
      # F-GFM (ind. labels)
      
      t = proc.time()
      cat(" F-GFM (ind)...")
      
      factors = factors.all.ind
      
      train.t = proc.time()
      model = learn.fgfm(train[, xs, drop=F], train[, ys, drop=F], factors, learner)
      train.t = proc.time() - train.t
      
      pred.test.t = proc.time()
      test.pred = pred.fgfm(model, test[, xs, drop=F])
      colnames(test.pred) = colnames(train[, ys, drop=F])
      pred.test.t = proc.time() - pred.test.t
      
      loss[c("hloss", "sloss", "f1"), "i-gfm", r] = eval.pred(test.true, test.pred)[c("hloss", "sloss", "f1")]
      time[c("train", "pred"), "i-gfm", r] = c(sum(train.t[c("user.self", "user.child")]),
                                             sum(pred.test.t[c("user.self", "user.child")]))
      
      t = proc.time() - t
      cat(" done (", sum(t[c("user.self", "user.child")]), "/", t["elapsed"], " sec)\n", sep="")
      
      # F-GFM (ILF-Compo)
      
      t = proc.time()
      cat(" F-GFM (ILF-Compo)...")
      
      load(sprintf("ydeps/%ix%i.%iz%i.%iy%i.bn.%s.seed%i/ydeps.train%04i.test%04i.rda",
                   n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, bn.t, seed, n.train, n.test))
      
      lps = list()
      done = rep(FALSE, n.ys)
      for (y.i in 1:length(ys)) {
        if(done[y.i])
          next
        lp = y.i
        done[y.i] = TRUE
        for (y.j in (1:n.ys)[!done]) {
          # OR
          if (any(ydeps[lp, y.j, r] < ci.alpha)) {
            lp = c(lp, y.j)
            done[y.j] = TRUE
          }
        }
        lps[[length(lps)+1]] = lp
      }
      factors = lps
      
      train.t = proc.time()
      model = learn.fgfm(train[, xs, drop=F], train[, ys, drop=F], factors, learner)
      train.t = proc.time() - train.t
      
      pred.test.t = proc.time()
      test.pred = pred.fgfm(model, test[, xs, drop=F])
      colnames(test.pred) = colnames(train[, ys, drop=F])
      pred.test.t = proc.time() - pred.test.t
      
      loss[c("hloss", "sloss", "f1"), "f-gfm", r] = eval.pred(test.true, test.pred)[c("hloss", "sloss", "f1")]
      time[c("train", "pred"), "f-gfm", r] = c(sum(train.t[c("user.self", "user.child")]),
                                             sum(pred.test.t[c("user.self", "user.child")]))
      
      t = proc.time() - t
      cat(" done (", sum(t[c("user.self", "user.child")]), "/", t["elapsed"], " sec)\n", sep="")
      
      # F-GFM (true ILFs)
      
      t = proc.time()
      cat(" F-GFM (true ILFs)...")
      
      factors = factors.true
      
      train.t = proc.time()
      model = learn.fgfm(train[, xs, drop=F], train[, ys, drop=F], factors, learner)
      train.t = proc.time() - train.t
      
      pred.test.t = proc.time()
      test.pred = pred.fgfm(model, test[, xs, drop=F])
      colnames(test.pred) = colnames(train[, ys, drop=F])
      pred.test.t = proc.time() - pred.test.t
      
      loss[c("hloss", "sloss", "f1"), "t-gfm", r] = eval.pred(test.true, test.pred)[c("hloss", "sloss", "f1")]
      time[c("train", "pred"), "t-gfm", r] = c(sum(train.t[c("user.self", "user.child")]),
                                               sum(pred.test.t[c("user.self", "user.child")]))
      
      t = proc.time() - t
      cat(" done (", sum(t[c("user.self", "user.child")]), "/", t["elapsed"], " sec)\n", sep="")
      
      cat("\n")
    }
    
    out.dir = sprintf("loss/%ix%i.%iz%i.%iy%i.bn.%s.seed%i", n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, bn.t, seed)
    out.file = sprintf("%s/loss.train%04i.test%04i.reps%04i.rda", out.dir, n.train, n.test, n.reps)
    
    dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
    save(loss, time, file=out.file)
  }
}

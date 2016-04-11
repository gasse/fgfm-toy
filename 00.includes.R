library("hitandrun")

random.dist = function(dag, node.dimnames) {
  
  dist = list()
  for (node in names(dag$nodes)) {
    
    dimnames = node.dimnames[node]
    dim = length(dimnames[[1]])
    
    parents = dag$nodes[[node]]$parents
    for (parent in parents) {
      dimnames = c(dimnames, node.dimnames[parent])
      dim = c(dim, length(dimnames[[length(dimnames)]]))
    }
    names(dimnames) = c(node, parents)
    
    data = array(0, dim, dimnames)
    mods = prod(dim(data)[-1])
    for (j in 1:mods) {
      data[(j-1) * dim[1] + 1:dim[1]] = simplex.sample(dim[1], 1)$samples[1, ]
    }
    
    dist = c(dist, list(array(
      dim = dim,
      data = data,
      dimnames = dimnames)))
  }
  names(dist) = names(dag$nodes)
  
  return(dist)
  
}

learn.fgfm = function(train.x, train.y, factors, learner) {
  
  models = vector("list", length(factors))
  for (k in seq(length(factors))) {
    
    f.ys = factors[[k]]
    
    f.train.yi = apply(train.y[, f.ys, drop=F] == "1", 2, as.numeric)
    f.train.ys = apply(f.train.yi, 1, sum)
    
    # p(y_s)
    ps.model = learn.model(train.x, factor(f.train.ys), learner)
    
    pis.models = vector("list", length(f.ys))
    for (i in seq(length(f.ys))) {
      
      # p(y_i | y_s)
      pis.models[[i]] = learn.model(cbind(train.x, f.train.ys), factor(f.train.yi[, i]), learner)
    }
    
    models[[k]] = list(
      factor = f.ys,
      ps.model = ps.model,
      pis.models = pis.models)
  }
  
  return(models)
}

pred.fgfm = function(model, test.x) {
  
  # n label factors
  n = length(model)
  
  nrows = nrow(test.x)
  
  factors = lapply(model, function(m){m$factor})
  
  # recover P and d for each factor
  params = vector("list", n)
  for (k in seq(n)) {
    
    m = length(factors[[k]])
    
    # get d vector from p(s_y)
    d = array(0, dim=c(nrows, m+1))
    pred = pred.model(model[[k]]$ps.model$model, test.x, "multinom")
    d[, as.integer(colnames(pred))+1] = pred
    dimnames(d) = list(row=seq(nrows), s=seq(m+1)-1)
    
    # get P matrix from p(y_i | s_y) * p(s_y)
    p = vapply(seq(m), function(s) {
      pis = vapply(seq(m), function(i) {
        pis = array(0, dim=c(nrows, 2))
        f.train.ys = s
        pred = pred.model(model[[k]]$pis.models[[i]]$model, cbind(test.x, f.train.ys), "multinom")
        pis[, as.integer(colnames(pred))+1] = pred
        pis = pis[, -1] # return only p(i=1|s), not p(i=0|s)
        return(pis)
      }, array(numeric(1), dim=nrows))
      pis = pis * d[, s+1]
      return(pis)
    }, array(numeric(1), dim=c(nrows, m)))
    
    dimnames(p) = list(row=seq(nrows), i=seq(m), s=seq(m))
    
    params[[k]] = list(p = p, d = d)
  }
  
  # combine factors
  while(length(factors) > 1) {
    
    # deal with smallest factors first
    todo = order(vapply(factors, length, integer(1)))[1:2]
    
    k1 = todo[1]
    k2 = todo[2]
    
    m1 = length(factors[[k1]])
    m2 = length(factors[[k2]])
    m = m1 + m2
    
    d1 = params[[k1]]$d
    d2 = params[[k2]]$d
    
    p1 = params[[k1]]$p
    p2 = params[[k2]]$p
    
    d = array(as.numeric(0), dim = c(nrows, m+1), dimnames = list(row=seq(nrows), s=seq(m+1)-1))
    s2 = seq(m2 + 1) - 1
    for (s1 in seq(m1 + 1) - 1) {
      d[, s1+s2+1] = d[, s1+s2+1] + d1[, s1+1] * d2[, s2+1]
    }
    
    p = array(as.numeric(0), dim = c(nrows, m, m), dimnames = list(row=seq(nrows), i=seq(m), s=seq(m)))
    s1 = seq(m1)
    for (s2 in seq(m2 + 1) - 1) {
      p[, seq(m1), s1+s2] = p[, seq(m1), s1+s2] + p1[, , s1] * d2[, s2+1]
    }
    s2 = seq(m2)
    for (s1 in seq(m1 + 1) - 1) {
      p[, seq(m2)+m1, s1+s2] = p[, seq(m2)+m1, s1+s2] + p2[, , s2] * d1[, s1+1]
    }
    
    params[[k1]] = list(p = p, d = d)
    params[[k2]] = NULL
    
    factors[[k1]] = c(factors[[k1]], factors[[k2]])
    factors[[k2]] = NULL
  }
  
  p = params[[1]]$p
  d = params[[1]]$d
  
  # re-order labels
  p = p[, sort(unlist(factors), index.return=TRUE)$ix, ]
  dimnames(p) = list(row=seq(nrows), i=seq(m), s=seq(m))
  
  # re-order P dimensions row, s, i
  p = margin.table(p, c(1, 3, 2))
  
  pred = gfm(p, d[, 1])
  
  return(pred)
}

gfm = function(p, d0) {
  
  nrows = dim(p)[1]
  m = dim(p)[2]
  
  # compute W matrix
  w = vapply(seq(m), function(s) {
    2/(s+seq(m))
  }, numeric(m))
  dimnames(w) = list(s=seq(m),k=seq(m))
  
  # compute Delta matrix from P * W
  delta = vapply(seq(nrows), function(i) t(p[i, , ]) %*% w, t(p[1, , ]))
  dimnames(delta) = list(i=seq(m), k=seq(m), row=seq(nrows))
  
  # recover each h_k optimal prediction (k>0)
  h = vapply(seq(nrows), function(r) {
    vapply(seq(m), function(k) {
      hk = rep(FALSE, m)
      hk[order(delta[, k, r], decreasing = TRUE)[seq(k)]] = TRUE
      return(hk)
    }, logical(m))
  }, array(logical(1), dim=c(m, m)))
  
  # compute each h_k optimal F-measure (k>0)
  h.score = vapply(seq(nrows), function(r) {
    vapply(seq(m), function(k) {
      sum(delta[h[, k, r], k, r])
    }, numeric(1))
  }, numeric(m))
  
  # recover the best h_k prediction (k>0)
  h.best.k = apply(h.score, 2, which.max)
  
  # recover h*, either best h_k (k>0) or h_0
  pred = vapply(seq(nrows), function(r) {
    if(h.score[h.best.k[r], r] > d0[r]) {
      # best h_k (k>0)
      return(h[, h.best.k[r], r])
    }
    else {
      # h_0
      return(rep(FALSE, m))
    }
  }, logical(m))
  
  pred = as.data.frame(t(pred))
  pred = as.data.frame(lapply(pred, factor, levels=c(FALSE, TRUE), labels=c("0", "1")))
  
  return(pred)
}

pred.model = function(model, test.x, learner) {
  
  pred = NULL
  
  if (learner == "multinom") {
    require("nnet")
  }
  else {
    stop(sprintf("unknown base learner: %s", learner))
  }
  
  # special cases:
  # - one class only in training (constant)
  # - no available input
  # - no discriminant input (RF bug : randomForest(data.frame(c("0", "0")), factor(c("0", "1"))) )
  if (is.null(model$obj)) {
    
    pred = array(
      rep(model$p, nrow(test.x)),
      dim = c(nrow(test.x), length(model$p)),
      dimnames = list(seq(nrow(test.x)), names(model$p)))
  }
  else {
      
    pred = array(as.numeric(0),
                 dim = c(nrow(test.x), length(model$p)),
                 dimnames = list(seq(nrow(test.x)), names(model$p)))
    
    # obtain predictions
    m.pred = predict(model$obj, test.x, type = "prob")
    
    # fix inconsistent output format (1 column per class)
    if (length(model$obj$lev) == 2) {
      pred[, model$obj$lev[1]] = 1 - m.pred
      pred[, model$obj$lev[2]] = m.pred
    }
    else {
      pred[, model$obj$lev] = m.pred
    }
  }
  
  return(pred)
}

learn.model = function(train.x, train.y, learner) {
  
  model = list(
    p = prop.table(table(train.y))
  )
  
  const.input = TRUE
  i = 0
  while(const.input && i < ncol(train.x)) {
    i = i+1
    const.input = all(train.x[-1, i] == train.x[-nrow(train.x), i])
  }
  
  const.output = all(train.y[-1] == train.y[-length(train.y)])
  
  # special cases:
  # - one class only in training (constant output)
  # - no discriminant input (constant input) (RF bug : randomForest(data.frame(c("0", "0")), factor(c("0", "1"))) )
  # - no available input
  if (const.input || const.output || ncol(train.x) < 1) {
    
    t = system.time(NULL)
    model$obj = NULL
  }
  else {
    
    if (learner == "multinom") {
      
      require("nnet")
      
      d = cbind(train.x, train.y)
      f = as.formula(paste(as.name(colnames(d)[ncol(d)]), "~", ".")) # y against all
      
      t = proc.time()
      
      MaxNWts = 100000
      maxit = 100 # 200
      decay.grid = c(1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3)
      
      # cross-validation for decay
      cv.nreps = 1
      cv.nsplits = 3
      decay.logloss = array(NA, dim=c(cv.nreps, cv.nsplits, length(decay.grid)))
      for (r in seq(cv.nreps)) {
        splits = sample(rep(seq(cv.nsplits), length.out = nrow(d)))
        for (s in seq(cv.nsplits)) {
          for (par in seq(length(decay.grid))) {
            
            if (length(unique(d[splits != s, ncol(d)])) < 2) {
              decay.logloss[r, s, par] = 0
            }
            else {
              v.model = multinom(f, d[splits != s, ], MaxNWts=MaxNWts, model=TRUE, maxit = maxit, decay = decay.grid[par], trace=FALSE)
              
              v.true = d[splits == s, ncol(d)]
              
              v.probs = array(as.numeric(0),
                              dim = c(sum(splits == s), length(levels(v.true))),
                              dimnames = list(rownames(d[splits == s, ]), levels(v.true)))
              
              # obtain predictions
              v.pred = predict(v.model, d[splits == s, ], type="prob")
              if (length(v.model$lev) == 2) {
                v.probs[, v.model$lev[1]] = 1 - v.pred
                v.probs[, v.model$lev[2]] = v.pred
              }
              else {
                v.probs[, v.model$lev] = v.pred
              }
              
              # compute log-loss
              eps = 1e-15
              v.probs[v.probs > 1 - eps] = 1 - eps
              v.probs[v.probs < eps] = eps
              v.probs = v.probs[cbind(seq(nrow(v.probs)),
                                      match(v.true, colnames(v.probs)))]
              
              decay.logloss[r, s, par] = mean(-log(v.probs), na.rm = TRUE)
            }
          }
        }
      }
      
      model$decay = decay.grid[which.min(apply(decay.logloss, 3, mean))]
      model$obj = multinom(f, d, MaxNWts=MaxNWts, model=TRUE, maxit = maxit, decay = model$decay, trace=FALSE)
      
      t = proc.time() - t
      
    }
    else {
      stop(sprintf("unsupported base learner: %s", learner))
    }
  }
  
  return(list(
    t = t,
    model = model))
}

eval.pred = function(true, pred) {
  
  nys = ncol(true)
  nobs = nrow(true)
  measures = c("hloss", "ma.recall", "ma.precision", "ma.f1",
               "sloss", "recall", "precision", "f1")
  values = array(NA, dim=c(length(measures)), dimnames=list(measure=measures))
  
  if (!is.null(true)) {
    
    true = data.matrix(true)
    pred = data.matrix(pred)
    
    # label-wise measures
    values[c("hloss", "ma.recall", "ma.precision", "ma.f1")] = apply(vapply(seq(nys), function(y.i) {
      
      ct = table(true=factor(true[, y.i], levels=1:2), pred=factor(pred[, y.i], levels=1:2))
      tp = ct[2, 2]
      tn = ct[1, 1]
      fp = ct[1, 2]
      fn = ct[2, 1]
      
      acc = (tp + tn) / (tp + tn + fp + fn)
      rec = if (fn == 0) 1 else tp / (tp + fn)
      pre = if (fp == 0) 1 else tp / (tp + fp)
      f1 = if (fn + fp == 0) 1 else 2 * tp / (2 * tp + fn + fp)
      
      hloss = 1 - acc
      
      return(c(hloss, rec, pre, f1))
    }, numeric(4)), 1, mean)
    
    # instance-wise measures
    values[c("sloss", "recall", "precision", "f1")] = apply(vapply(seq(nobs), function(r) {
      
      ct = table(true=factor(true[r, ], levels=1:2), pred=factor(pred[r, ], levels=1:2))
      tp = ct[2, 2]
      tn = ct[1, 1]
      fp = ct[1, 2]
      fn = ct[2, 1]
      
      rec = if (fn == 0) 1 else tp / (tp + fn)
      pre = if (fp == 0) 1 else tp / (tp + fp)
      f1 = if (fn + fp == 0) 1 else 2 * tp / (2 * tp + fn + fp)
      
      sloss = as.numeric(any(true[r, ] != pred[r, ]))
      
      return(c(sloss, rec, pre, f1))
    }, numeric(4)), 1, mean)
  }
  
  return(values)
}

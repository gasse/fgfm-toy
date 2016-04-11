
methods = c("GFM", "T-GFM", "F-GFM") # "I-GFM"
losses = c("f1")

m.id = list(
  "GFM" = "gfm",
  "I-GFM" = "i-gfm",
  "T-GFM" = "t-gfm",
  "F-GFM" = "f-gfm"
)

bn.t.name = list(
  "lp1" = "DAG 1",
  "lp2" = "DAG 2",
  "lp3" = "DAG 3",
  "lp4" = "DAG 4",
  "lp5" = "DAG 5",
  "lp6" = "DAG 6",
  "lp7" = "DAG 7",
  "lp8" = "DAG 8"
)

m.lgd = list(
  "GFM" = "GFM",
  "I-GFM" = "I-GFM",
  "T-GFM" = "F-GFM (true)",
  "F-GFM" = "F-GFM (ILF)"
)

m.lty = list(
  "GFM" = 1,
  "T-GFM" = 2,
  "I-GFM" = 3,
  "F-GFM" = 4
)

m.pch = list(
  "GFM" = 1,
  "T-GFM" = 2,
  "I-GFM" = 3,
  "F-GFM" = 4
)

m.col = list(
  "T-GFM" = "green3", # c("red", "green3")
  "I-GFM" = "deepskyblue2", # c("red", "green3")
  "GFM" = "red", # c("deepskyblue2", "goldenrod1")
  "F-GFM" = "black" # c("darkorchid", "seagreen3")
)

out.dir = sprintf("figs/%ix%i.%iz%i.%iy%i", n.xs, x.dim, n.zs, z.dim, n.ys, y.dim)
dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
  
for (bn.t in bn.types) {
  
  out.file = sprintf("loss.inc.prop.log.%ix%i.%iz%i.%iy%i.bn.%s.seed%i.reps%04i.test%04i",
                     n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, bn.t, seed, n.reps, n.test)
  
  all.loss = array(NA, dim=c(n.reps, length(methods), length(n.train.reps), length(losses)),
                   dimnames=list(rep = seq(n.reps), method = methods,
                     n.train = n.train.reps, loss = losses))
  
  for (n.train in n.train.reps) {
    
    load(sprintf("loss/%ix%i.%iz%i.%iy%i.bn.%s.seed%i/loss.train%04i.test%04i.reps%04i.rda",
                 n.xs, x.dim, n.zs, z.dim, n.ys, y.dim, bn.t, seed, n.train, n.test, n.reps))
    
    all.loss[, methods, as.character(n.train), losses] = loss[losses, unlist(m.id[methods]), ]
  }
  
  mean.loss = apply(all.loss, 2:4, mean)
  for (loss.t in losses) {
    
    xlim = range(n.train.reps)
    ylim = range(mean.loss[, , loss.t])
    ylab = sprintf("mean %s", loss.t)
    xlab="train.size (log scale)"
    
    eps.width = 2.0# 1.8
    eps.height = 1.8
    eps.par = list(cex=0.60, cex.axis = 0.75, cex.lab = 0.8, tcl = -0.25, las = 2, mex=1.0, lwd=1.2, mgp=c(2, 0.5, 0), mar=c(3, 3, 2, 2) + 0.1)
    
    postscript(file = sprintf("%s/%s.%s.all.eps", out.dir, out.file, loss.t),
               paper="special", width=eps.width, height=eps.height, horizontal=FALSE)
    par(eps.par)
    
    plot(x=xlim, y=ylim, type="n", log="x",
         main=bn.t.name[[bn.t]],
         xlab=xlab, ylab=ylab)
    
    legend("bottomright", legend=m.lgd[methods], lty=unlist(m.lty[methods]),
           pch=unlist(m.pch[methods]), col=unlist(m.col[methods]))
    
    for (m in methods) {
      lines(x=n.train.reps, y=mean.loss[m, , loss.t], type="b",
            lty=m.lty[[m]], pch=m.pch[[m]], col=m.col[[m]])
    }
    
    dev.off()
  }
}

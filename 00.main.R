source("00.includes.R")

seed = 0

ci.test = "sp-mi"
ci.alpha = 0.01
ci.test.args = list(power.rule=NULL, B=100)

n.reps = 100

n.test = 5000
n.train.reps = c(50, 100, 200, 500, 1000, 2000, 5000)

n.xs = 4
x.dim = 2

n.zs = 2
z.dim = 2

n.ys = 8
y.dim = 2

lp.sizes = c(2, 4, 6, 8)
bn.types = paste("lp", lp.sizes, sep="")


source("01.make.bn.R")
source("02.gen.data.R")
source("03.learn.ydeps.R")
source("04.run.models.R")
source("05.plot.loss.R")

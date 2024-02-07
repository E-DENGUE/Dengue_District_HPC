### define space domain as a grid
grid <- SpatialGrid(GridTopology(c(0,0), c(1, 1), c(4, 5)))
(n <- nrow(xy <- coordinates(grid)))

### build a spatial neighborhood list
jj <- lapply(1:n, function(i) 
  which(sqrt((xy[i,1]-xy[,1])^2 + (xy[i,2]-xy[,2])^2)==1))

### build the spatial adjacency matrix
graph1 <- sparseMatrix(rep(1:n, sapply(jj, length)),
                      unlist(jj), x=1, dims=c(n, n))

### some random data at 10 time points
dat <- inla.knmodels.sample(graph, m=10, tau.t=2, tau.s=2, tau.st=3)
str(dat)
sapply(dat$x, summary)

nd <- length(dat$x$eta)
dat$e <- runif(nd, 0.9, 1.1)*rgamma(n, 40, 2)
dat$y <- rpois(nd, dat$e*exp(dat$x$eta-3))
summary(dat$y)

### fit the type 4 considering three different approaches 
tgraph <- sparseMatrix(i=c(2:10, 1:9), j=c(1:9, 2:10), x=-1)

dat <- inla.knmodels.sample(graph, m=10, tau.t=2, tau.s=2, tau.st=5, type=4)
sapply(dat$x, summary)

nd <- length(dat$x$eta)
dat$e <- runif(nd, 0.9, 1.1)*rgamma(n, 40, 2)
dat$y <- rpois(nd, dat$e*exp(dat$x$eta-3))
summary(dat$y)

time_id1 = dat$time
time_id2 = dat$time

space_id1 = dat$space
space_id2 = dat$space
INIT_THETA <- 1
fix_th <- FALSE

hyper = list(prec=list(prior="pc.prec", param=c(1,0.5), 
                       fixed = fix_th,
                       initial = INIT_THETA[1] ))

formula.ST1 <- y ~ 1 + f(space_id1, 
                                   model='besag', 
                                   constr= TRUE, 
                                   graph=graph1,
                                   group=time_id1, 
                                   control.group=list(model='ar1'),
                                   hyper = list(hyper),
                                   scale.model = TRUE) +
                            f(space_id2, 
                              model='iid', 
                              group=time_id2, 
                              control.group=list(model='iid'),
                              hyper = list(hyper))

r <- inla(formula.ST1,
          family = "poisson",
          E = e,
          data =mydata,
          #num.threads ="8:6",
          control.inla=list(strategy="gaussian",
                            control.vb=list(enable=TRUE),
                            h = 5E-3,use.directions = TRUE,num.hessian="central"),
          control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE),
          control.fixed = list(prec.intercept =1e-3),
          verbose=T)

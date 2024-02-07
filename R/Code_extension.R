#setwd("~/Downloads/Data_and_shapefile/Wala")
load("my_workspace.RData")
library(INLA)

mydata <- list()
mydata$y <- c1$m_DHF_cases
mydata$E <- c1$offset1
mydata$cov <- c1$t

##---------------------------
#-------> Time
##---------------------------
order_t <- 2
nt <- length(unique(c1$t))
Qt <- INLA:::inla.rw(nt, order = order_t)
con_t <- eigen(Qt)$vectors[,(nt-order_t+1):nt]
Qt = inla.scale.model(Qt,list(A=t(con_t),e=rep(0,order_t)))
mydata$id_t <- c1$t

##---------------------------
#-------> Space
##---------------------------
order_s <- 1
R <- inla.read.graph(paste(getwd(),"/MDR.graph", sep=""))
graph_M <- inla.graph2matrix(R)
graph_M[graph_M != 0] <- -1
diag(graph_M) <- 0
diag(graph_M) <- abs(rowSums(graph_M))
Qs <- graph_M
ns <- dim(Qs)[1]
con_s <- eigen(Qs)$vectors[,ns]
Qs = inla.scale.model(Qs,list(A=t(con_s),e=0))
mydata$id_t <- c1$t


##---------------------------
#-------> Time x Space
##---------------------------
id_ts=rep(seq(1,ns*nt),each=1)
R_QtQs <- Qt%x%Qs
nts <- dim(R_QtQs)[1]
num_con_ts <- nts - (nt-order_t)*(ns-1)
con_ts <- eigen(R_QtQs)$vectors[,(dim(R_QtQs)[1]-num_con_ts+1):dim(R_QtQs)[1]]
mydata$id_s <- c1$districtID


##---------------------------
#-------> Formula
##---------------------------
INIT_THETA <- rep(1,3)
fix_th <- FALSE

formula = y~ 1 + cov +
  f(id_t,model="generic0",
    Cmatrix= Qt,
    constr= FALSE,
    rankdef = order_t,
    diagonal = 1e-4,
    extraconstr= list(A=t(con_t),e=rep(0,nrow(t(con_t)))),
    hyper = list(prec=list(prior="pc.prec", param=c(1,0.5), 
                           fixed = fix_th,
                           initial = INIT_THETA[1] ))) +
  
  f(id_s,model="generic0",
    Cmatrix= Qs,
    constr= FALSE,
    diagonal = 1e-4,
    rankdef = order_s,
    extraconstr= list(A=t(con_s),e=rep(0,nrow(t(con_s)))),
    hyper = list(prec=list(prior="pc.prec", param=c(1,0.5), 
                           fixed = fix_th,
                           initial = INIT_THETA[2]))) +

  f(id_ts,model="generic0",
    Cmatrix= R_QtQs,
    constr= FALSE,
    diagonal = 1e-4,
    rankdef = num_con_ts,
    extraconstr= list(A=t(con_ts),e=rep(0,nrow(t(con_ts)))),
    hyper = list(prec=list(prior="pc.prec", param=c(1,0.5), 
                           fixed = fix_th,
                           initial = INIT_THETA[5]))) 
   

r <- inla(formula,
          family = "poisson",
          E = E,
          data =mydata,
          #num.threads ="8:6",
          control.inla=list(strategy="gaussian",
                            control.vb=list(enable=TRUE),
                            h = 5E-3,use.directions = TRUE,num.hessian="central"),
          control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE),
          control.fixed = list(prec.intercept =1e-3),
          verbose=T)



#### ---- > Proposed Model:

districtID1 <- districtID
districtID2 <- districtID
time_id1 <- c1$t
time_id2 <- c1$t

formula.ST1 <- m_DHF_cases ~ 1 + f(districtID1, 
                                   model='besag', 
                                   constr= TRUE, 
                                   graph=MDR.adj
                                   group=time_id1, 
                                   control.group=list(model='ar1'),
                                   hyper = list(hyper),
                                   scale.model = TRUE) +
                                f(districtID2, 
                                  model='iid', 
                                  group=time_id2, 
                                  control.group=list(model='iid'),
                                  hyper = list(hyper),
                                  scale.model = TRUE)
              




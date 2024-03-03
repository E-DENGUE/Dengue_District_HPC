#Type 4 spatial model in INLA; code provided by 	Esmail Abdul Fattah
#Called when all_district_fwd1() has a value of T for type4mod

##---------------------------
#-------> Time
##---------------------------
order_t <- 2
nt <- length(unique(c1$t))
Qt <- INLA:::inla.rw(nt, order = order_t)
con_t <- eigen(Qt)$vectors[,(nt-order_t+1):nt]
Qt = inla.scale.model(Qt,list(A=t(con_t),e=rep(0,order_t)))
c1$id_t <- c1$t

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
c1$id_t <- c1$t


##---------------------------
#-------> Time x Space
##---------------------------
id_ts=rep(seq(1,ns*nt),each=1)
R_QtQs <- Qt%x%Qs
nts <- dim(R_QtQs)[1]
num_con_ts <- nts - (nt-order_t)*(ns-1)
con_ts <- eigen(R_QtQs)$vectors[,(dim(R_QtQs)[1]-num_con_ts+1):dim(R_QtQs)[1]]
c1$id_s <- c1$districtID


##---------------------------
#-------> Formula
##---------------------------
INIT_THETA <- rep(1,3)
fix_th <- FALSE

mod1 <- inla(form2,
          family = "poisson",
          E = E,
          data =c1,
          #num.threads ="8:6",
          control.inla=list(strategy="gaussian",
                            control.vb=list(enable=TRUE),
                            h = 5E-3,use.directions = TRUE,num.hessian="central"),
          control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE),
          control.fixed = list(prec.intercept =1e-3),
          verbose=T)

#### RLQ and fourth corner analysis                                 
## species data

clad_sp_RLQ=data.frame(cbind(sample=clado_env_data3$sample,clado_comm_env2))
rownames(clad_sp_RLQ)<-clad_sp_RLQ[,1]
clad_sp_RLQ[1] <- NULL
ncol(clad_sp_RLQ)
### environmental data
clado_env_RLQ=data.frame(cbind(sample=clado_env_data3$sample,cca_clado_env))
edit(clado_env_RLQ)
rownames(clado_env_RLQ)<-clado_env_RLQ[,1]
clado_env_RLQ[1] <- NULL  
names(clado_env_RLQ)
clado_env_RLQ=clado_env_RLQ[,-3]
edit(clado_env_RLQ)
### functional trait data
# original data
clado_func_trait_env=as.data.frame(new_func_traits_env)
nrow(clado_func_trait_env)
# conversion into dataframe
names(clado_func_trait_env)
clado_trait_RLQ=as.data.frame(select(clado_func_trait_env,Species,Feeding_type:Presence_Ocellus,Color:Trophic_Regime))
rownames(clado_trait_RLQ)=clado_trait_RLQ$Species
clado_trait_RLQ[1] <- NULL
names(clado_trait_RLQ)
#unclass the factors to convert them into integers
clad_RLQ1_fac=sapply(clado_trait_RLQ,is.factor)
clad_trait_RLQ=sapply(clado_trait_RLQ[clad_RLQ1_fac],unclass)
clad_trait_RLQ=cbind(clad_trait_RLQ,clado_trait_RLQ$Average_egg_clutch,clado_trait_RLQ$MBL,clado_trait_RLQ$SWA_Length,clado_trait_RLQ$Eye_Size...)
edit(clad_trait_RLQ)
#assigning rownames to the data
rownames(clad_trait_RLQ)=clado_trait_RLQ$Species

clad_trait_RLQ[1] <- NULL
#RLQ analysis
#Species
species_RLQ_clado <- dudi.coa(clad_sp_RLQ, scannf = FALSE)
species_RLQ_clado$lw
#environment
env_RLQ_clado <- dudi.hillsmith(clado_env_RLQ,scannf = FALSE,row.w = species_RLQ_clado$lw)
#traits
traits_RLQ_clado <- dudi.pca(clad_trait_RLQ,scannf = FALSE,row.w=species_RLQ_clado$cw)

#RLQ analysis
rlq_clado <- rlq(env_RLQ_clado, species_RLQ_clado, traits_RLQ_clado,
                 scannf = FALSE)
plot(rlq_clado,boxes=F)

par(mfrow = c(3,1))
s.arrow(rlq_clado$l1)
s.arrow(rlq_clado$c1)
s.label(rlq_clado$lQ)

summary()

#fourth corner analysis
fourth_corner_clado=fourthcorner(clado_env_RLQ, clad_sp_RLQ,
                                 clad_trait_RLQ, modeltype = 6, p.adjust.method.G = "none",
             p.adjust.method.D = "none",nrepet = 999)
plot(fourth_corner_clado,stat = "D",alpha=0.06)


library(ade4)
install.packages("ade4")

data(aravo)
edit(aravo$traits)

afcL.aravo <- dudi.coa(aravo$spe, scannf = FALSE)

acpR.aravo <- dudi.hillsmith(aravo$env, row.w = afcL.aravo$lw,
                             scannf = FALSE)

acpQ.aravo <- dudi.pca(aravo$traits, scannf = FALSE)
rlq.aravo <- rlq(acpR.aravo, afcL.aravo, acpQ.aravo,
                 scannf = FALSE)
?dudi.pca
plot(rlq.aravo)

par(mfrow = c(1, 3))
s.arrow(rlq.aravo$l1)
s.arrow(rlq.aravo$c1)
s.label(rlq.aravo$lQ, boxes = FALSE)


edit(cladocera_pool_env)


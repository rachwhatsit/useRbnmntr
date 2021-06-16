library("bnmonitor")
library("bnlearn")
library("qgraph")

data(asia)
data(coronary)
data(survey)
data(lizards)
# create and plot the network structure.
lzrd.dag = model2network("[Species][Diameter|Species][Height|Species]")

global_monitor(dag = lzrd.dag , df = lizards,alpha=2)
plot(seq_marg_monitor(lzrd.dag, lizards, "Species"))
plot(seq_marg_monitor(lzrd.dag, lizards, "Diameter"))
plot(seq_marg_monitor(lzrd.dag, lizards, "Height"))

plot(1:409,lizards$Diameter)
plot(1:409,lizards$Species[sample(1:409,409)])

lzrd.ordr <- lizards[sample(1:409,409),]
p.spcs <- plot(seq_marg_monitor(lzrd.dag, lzrd.ordr, "Species"))
p.spcs.cond <- plot(seq_cond_monitor(lzrd.dag, lzrd.ordr, "Species"))
p.dmtr <- plot(seq_marg_monitor(lzrd.dag, lzrd.ordr, "Diameter"))
p.dmtr.cond <- plot(seq_cond_monitor(lzrd.dag, lzrd.ordr, "Diameter"))
p.hght <- plot(seq_marg_monitor(lzrd.dag, lzrd.ordr, "Height"))
p.hght.cond <- plot(seq_cond_monitor(lzrd.dag, lzrd.ordr, "Height"))

p.spcs + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') + 
  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')

p.spcs.cond + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') + 
  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')

p.dmtr + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') + 
  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
p.dmtr.cond + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') + 
  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')

p.hght + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') + 
  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
p.hght.cond + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') + 
  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')


dag  <- hc(diabetes)
qgraph(dag)


plot(seq_marg_monitor(dag , diabetes , "DIAB"))
plot(seq_marg_monitor(dag , diabetes , "PED"))
plot(seq_cond_monitor(dag , diabetes , "DIAB"))
plot(seq_cond_monitor(dag , diabetes , "PED"))

plot(seq_pa_ch_monitor(dag , diabetes , "DIAB",pa.names = c("GLUC","MASS"), pa.val = c("low","low")))
influence  <- influential_obs(dag , diabetes)plot(influence)subset(unique(influence),score > 8.5)
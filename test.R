library("bnmonitor")
library("bnlearn")
library("qgraph")
library("ggplot2")


load("diabetes.RData")

dag  <- hc(diabetes)
qgraph(dag)
         
global_monitor(dag = dag , df = diabetes,alpha=2)
marg.preg <- plot(seq_marg_monitor(dag, diabetes, "PREG"))
marg.gluc <- plot(seq_marg_monitor(dag, diabetes, "GLUC")) #weird, but ROOT NODE
marg.pres <- plot(seq_marg_monitor(dag, diabetes, "PRES"))
marg.tric <- plot(seq_marg_monitor(dag, diabetes, "TRIC"))
marg.ins <- plot(seq_marg_monitor(dag, diabetes, "INS"))
marg.mass <- plot(seq_marg_monitor(dag, diabetes, "MASS"))
marg.ped <- plot(seq_marg_monitor(dag, diabetes, "PED"))
marg.age <- plot(seq_marg_monitor(dag, diabetes, "AGE"))
marg.diab <- plot(seq_marg_monitor(dag, diabetes, "DIAB"))#mildly weird,

marg.preg + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
marg.gluc + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
marg.pres + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
marg.tric + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
marg.ins + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
marg.mass + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
marg.ped + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
marg.age + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
marg.diab + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')

cond.preg <- plot(seq_cond_monitor(dag, diabetes, "PREG"))#some weird issues 
cond.gluc <- plot(seq_cond_monitor(dag, diabetes, "GLUC"))
cond.pres <- plot(seq_cond_monitor(dag, diabetes, "PRES"))
cond.tric <- plot(seq_cond_monitor(dag, diabetes, "TRIC"))
cond.ins <- plot(seq_cond_monitor(dag, diabetes, "INS"))
cond.mass <- plot(seq_cond_monitor(dag, diabetes, "MASS"))
cond.ped <- plot(seq_cond_monitor(dag, diabetes, "PED"))
cond.age <- plot(seq_cond_monitor(dag, diabetes, "AGE"))
cond.diab <- plot(seq_cond_monitor(dag, diabetes, "DIAB"))

cond.preg + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
cond.gluc + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
cond.pres + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
cond.tric + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
cond.ins + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
cond.mass + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
cond.ped + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
cond.age + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
cond.diab + geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')

dim(diabetes)
plot(1:392,ifelse(diabetes$PREG=="low",1,0))

plot(seq_pa_ch_monitor(dag , diabetes , "DIAB",pa.names = c("GLUC","MASS"), pa.val = c("low","low"), alpha=2))+ geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
plot(seq_pa_ch_monitor(dag , diabetes , "DIAB",pa.names = c("GLUC","MASS"), pa.val = c("high","low"),alpha=2))+ geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
plot(seq_pa_ch_monitor(dag , diabetes , "DIAB",pa.names = c("GLUC","MASS"), pa.val = c("low","high"),alpha=2))+ geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
plot(seq_pa_ch_monitor(dag , diabetes , "DIAB",pa.names = c("GLUC","MASS"), pa.val = c("high","high"),alpha=2))+ geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
#low glucose, high body mass hard to predict diabetes

plot(seq_pa_ch_monitor(dag , diabetes , "PREG",pa.names = "AGE", pa.val = "low",alpha=2))+ geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
plot(seq_pa_ch_monitor(dag , diabetes , "PREG",pa.names = "AGE", pa.val = "high",alpha=2))+ geom_hline(yintercept = 1.96, linetype='dashed', col = 'red') +  geom_hline(yintercept = -1.96, linetype='dashed', col = 'red')
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
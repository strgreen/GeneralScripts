library(reshape)

#Clear Workspace
rm(list=ls())

raw.dataTable = read.csv("I:/Backup/Aupperle/Experiments/FrustGoNoGo/Data/aug.data.csv")

#Convert subject into numeric
raw.dataTable$Subject.Number = as.numeric(raw.dataTable$Subject)
raw.dataTable$Affect.Probe = as.numeric(raw.dataTable$Affect.Probe)
raw.dataTable$Reaction.Time = as.numeric(raw.dataTable$Reaction.Time)

#Form a table inlcluding only probe responses

probe.data = raw.dataTable[raw.dataTable$Type=="Probe Response",]

#Melt probe data into a table ready for casting
probe.data.sub = melt(probe.data, measure.vars = "Affect.Probe", id.vars= c("Block.Label", "Subject.Number") )

#cast probe.data inot a new table computing mean 
probe.data.mn.sub = cast(probe.data.sub,Subject.Number~Block.Label, value = "Affect.Probe", fun = "mean")
#turn object into a data frame
probe.data.mn.sub = as.data.frame(probe.data.mn.sub)

#cast probe.data inot a new table computing sd
probe.data.sd.sub = cast(probe.data.sub,Subject.Number~Block.Label, value = "Affect.Probe", fun = "sd")
#turn object into a data frame
probe.data.sd.sub = as.data.frame(probe.data.sd.sub)

#compute power variables pre block - frust block
pooled.variance = ((probe.data.sd.sub$`1 - Pre Frustration Block`*14) + (probe.data.sd.sub$`2 - Frustration Block`* 14)) / 28
mean.difference = probe.data.mn.sub$`1 - Pre Frustration Block`- probe.data.mn.sub$`2 - Frustration Block`
mn.mean.difference = mean(mean.difference)
mn.pooled.variance = mean(pooled.variance)

#compute power variables post block - frust block
mean.difference = probe.data.mn.sub$`3 - Post Frustration Block`- probe.data.mn.sub$`2 - Frustration Block`
pooled.variance = ((probe.data.sd.sub$`3 - Post Frustration Block`*14) + (probe.data.sd.sub$`2 - Frustration Block`* 14)) / 28
mn.mean.difference = mean(mean.difference)
mn.pooled.variance = mean(pooled.variance)

#cohen's D
cohens.d = mean.difference/pooled.variance
mn.cohens.d = mean(cohens.d)

#compute delta
delta = cohens.d * sqrt(15)
mn.delta = mean(delta)

#Samples
amt.samples = 20

powers = vector(mode="numeric", length=amt.samples)


#Produce a vector containing the power for the amount of samples
for(sample in 1:amt.samples) {

  #compute power  
  power.analysis = power.t.test(sample,mn.mean.difference,mn.pooled.variance, type ="two.sample")

  #gather power
  powers[sample] = power.analysis$power

}

#Plot power
plot(powers, type='p', 
     main = "Power level by samples", 
     xlab ="Number of Samples", 
     ylab = "Power")

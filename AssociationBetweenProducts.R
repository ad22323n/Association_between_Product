library(arules)
library(arulesViz)

Electronic<- read.transactions("file:///C:/Users/User/Desktop/Ubiqum/Task4/ElectronidexTransactions2017.csv", format = 'basket', sep=",",rm.duplicates = FALSE, cols = NULL)

inspect(Electronic[1:15])
length(Electronic) #9835 Transaction
size(Electronic) #number of item
LIST(Electronic) # list the transaction by conversion
itemLabels(Electronic[,1:25])

#====================================================visualize data

itemFrequencyPlot(Electronic)
itemFrequencyPlot(Electronic, topN=25)
itemFrequencyPlot(Electronic, type="absolute")
itemFrequencyPlot(Electronic,support=1500, type='absolute')
image(sample(Electronic[1:125]))
image(sample(Electronic,50))

#==================================================Apriori algorithm RUles=================================

MyFirstRule<-apriori(Electronic, parameter = list(supp=0.002, conf= 0.75))
inspect(MyFirstRule)


SnRules<-apriori(Electronic, parameter = list(supp=0.009, conf=0.50))
inspect(SnRules)


Rules<-apriori(Electronic, parameter = list(supp=0.003, conf=0.6))
inspect(Rules)
inspectDT(sort(Rules[1:10], by="lift"))

#================================================single rule============================================
size(Electronic) #a lot of transaction include iMac(1)
SingleTransaction<-Electronic[which(size(Electronic)==1),]
SingleCrossTable<-crossTable(SingleTransaction)
SingleCrossTable['iMac', 'iMac'] #121 
SingleCrossTable['Acer Desktop', 'Acer Desktop'] #60
inspect(SingleTransaction)

#-----------------------------------------Single Rule for FirstRule==================================================
iMacRule<-subset(MyFirstRule, rhs %in% 'iMac')
inspect(iMacRule)
summary(iMacRule)

HpRule<-subset(MyFirstRule, rhs %in% 'HP Laptop')
inspect(HpRule)
summary(HpRule)

#-----------------------------------------2nd Second Rule single RUle================================================
iMacRule2<-subset(SnRules, rhs %in% 'iMac')
inspect(iMacRule2)
summary(iMacRule2)

HpRule2<-subset(SnRules, rhs %in% 'HP Laptop')
inspect(HpRule2)
summary(HpRule2)

#===================================3rd Rule(Game) ===================================================================
Game_rule<-apriori(Electronic, parameter = list(supp=0.0032, conf=0.13), appearance = list(default='lhs',rhs='Computer Game'))

inspect(Game_rule)
summary(Game_rule)
#_---__--___-_________________________No iMac and HP rule ========================================================
NoHP_iMacRule<-apriori(Electronic, parameter = list(supp=0.0015, conf= 0.6,minlen=2),
                       appearance = list(none=c('iMac','HP Laptop')))

inspectDT(sort(NoHP_iMacRule, by='support'))
inspect(NoHP_iMacRule)
inspectDT(NoHP_iMacRule)
plot(NoHP_iMacRule)
summary(NoHP_iMacRule)

#_--_-__---------------_______________subset of No iMac and HP Rule===============================================

dell.rules<-subset(NoHP_iMacRule, rhs %in% 'Dell Desktop')
inspect(dell.rules)
plot(dell.rules, by='lift',method='paracoord')

#---_---____________________________No dell lenove Rule======================================================
NoDell_Lenove<-apriori(Electronic, parameter = list(supp=0.0012, conf=0.7), 
                       appearance = list(none=c('iMac','HP Laptop','Dell Desktop','Lenovo Desktop Computer')))

inspectDT(NoDell_Lenove)

#____________--------________________Looking for redundant================================================
is.redundant(MyFirstRule)
rules.sorted<-sort(MyFirstRule, by='lift')
subset.matrix<-is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)]<-NA
redundant<-colSums(subset.matrix, na.rm = T) >= 1
which(redundant)
rules.pruned<-rules.sorted[!redundant]
inspect(rules.pruned)

#-------__________________________________Sub rules =======================================================
subFirsRule<-MyFirstRule[quality(MyFirstRule)$confidence>0.6]
plotly_arules(subFirsRule)


myRule<-head(sort(MyFirstRule, by="lift"),10)
plot(myRule, method='paracoord', shading='order')

#___________-----________________Visualizing the results===================================================
#Firs RUle
plot(MyFirstRule[1:5], method = 'graph', control = list(type='HP Laptop'))
plot(MyFirstRule[1:10], method = 'paracoord', control = list(reorder=TRUE))

inspectDT(sort(MyFirstRule[1:5], by='lift'))
inspectDT(sort(SnRules[1:10], by='lift'))

#2nd Rule
inspect(SnRules[1:5])
inspectDT(sort(SnRules, by='lift'))
inspectDT(sort(SnRules, by='count'))

#HPRULE 
plotly_arules(sort(HpRule, method = 'graph'))
inspectDT(sort(HpRule, method='graph'))
inspect(sort(HpRule, topN=10))
plot(HpRule, topN=25, method = 'paracoord')

#hpRUle2
inspectDT(sort(HpRule2, method='paracoord'))
plot(HpRule2, method='paracoord')


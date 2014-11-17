
wilcoxon_test <- function(single, dual)
{
	print(sprintf('Single view median: %.1f', median(single)))
	print(sprintf('Dual view median: %.1f', median(dual)))
	
	print(single)
	print(dual)
	
	wilcox.test(single, dual, paired = TRUE, alternative = 'two.sided')
}

#	By type
data.type <- as.data.frame(read.csv('correction_type.csv', header = T))

single.set <- data.type[data.type$view == 'single', ]
dual.set <-  data.type[data.type$view == 'dual', ]

#	General score
single.all <- single.set$all_types
dual.all <- dual.set$all_types

wilcoxon_test(single.all, dual.all)
boxplot(list(single.all, dual.all), 
		names = c('single view', 'dual view'), 
		main = 'General score', ylab = 'Score', 
		col=rep(c('white', 'grey')) )
legend(x=1.32, y=8, legend=c('single', 'dual'), fill=c('white', 'grey'))

#	type1 score
single.type1 <- single.set$type1
dual.type1 <- dual.set$type1
wilcoxon_test(single.type1, dual.type1)

#	type2 score
single.type2 <- single.set$type2
dual.type2 <- dual.set$type2
wilcoxon_test(single.type2, dual.type2)

#	type3 score
single.type3 <- single.set$type3
dual.type3 <- dual.set$type3
wilcoxon_test(single.type3, dual.type3)

#	type4 score
single.type4 <- single.set$type4
dual.type4 <- dual.set$type4
wilcoxon_test(single.type4, dual.type4)

boxplot(list(single.type1, dual.type1, single.type2, dual.type2, single.type3, dual.type3, single.type4, dual.type4), 
		main = 'Different types', ylab = 'Score', 
		col=rep(c('white', 'grey')) )
legend(x=.5, y=4, legend=c('single', 'dual'), fill=c('white', 'grey'))
		
		
#	By project
data.type <- as.data.frame(read.csv('correction_proj.csv', header = T))

single.set <- data.type[data.type$view == 'single', ]
dual.set <-  data.type[data.type$view == 'dual', ]

#	JHotDraw score
single.jhot <- single.set$jhot
dual.jhot <- dual.set$jhot
wilcoxon_test(single.jhot, dual.jhot)

#	JRefactory score
single.jref <- single.set$jref
dual.jref <- dual.set$jref
wilcoxon_test(single.jref, dual.jref)

#	PADL score
single.padl <- single.set$padl
dual.padl <- dual.set$padl
wilcoxon_test(single.padl, dual.padl)

boxplot(list(single.jhot, dual.jhot, single.jref, dual.jref, single.padl, dual.padl), 
		main = 'Different projects', ylab = 'Score', 
		col=rep(c('white', 'grey')) )
legend(x=5.5, y=4, legend=c('single', 'dual'), fill=c('white', 'grey'))


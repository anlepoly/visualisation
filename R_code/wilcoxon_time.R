
wilcoxon_test <- function(single, dual, plotname)
{
	print(sprintf('Single view median: %.0f', median(single)))
	print(sprintf('Dual view median: %.0f', median(dual)))
	wilcox.test(single, dual, paired = TRUE, alternative = 'two.sided')
}

cum.single <- c(2520, 2635, 2114, 2677, 2135, 1693, 2557, 1349, 2129, 1735, 2033, 4620, 2400, 1200, 1415, 4084, 929, 3600)
cum.dual <- c(2820, 3029, 2194, 3502, 1654, 785, 2120, 1344, 2010, 2638, 1978, 2740, 2228, 1620, 1738, 1440, 3300, 1754)
wilcoxon_test(cum.single, cum.dual, 'Cumulated time')

boxplot(list(cum.single, cum.dual), 
		names = c('single', 'dual'),
		main = 'Cumulated time', ylab = 'Time in second', 
		col=rep(c('white', 'grey')) )
legend(x=2.14, y=4630, legend=c('single', 'dual'), fill=c('white', 'grey'))

type1.single <- c(1260, 330, 193, 1134, 780, 181, 1015, 490, 382, 186, 1620, 1200, 960, 420, 550, 120, 211, 900)
type1.dual <- c(180, 822, 327, 1080, 38, 114, 399, 310, 456, 275, 1134, 1420, 195, 420, 343, 90, 600, 892)
wilcoxon_test(type1.single, type1.dual, 'Type 1')

type2.single <- c(360, 955, 781, 688, 1080, 578, 514, 241, 327, 292, 62, 120, 780, 300, 534, 717, 228, 900)
type2.dual <- c(1080, 761, 199, 397, 840, 150, 780, 407, 958, 1183, 300, 474, 590, 300, 329, 294, 900, 344)
wilcoxon_test(type2.single, type2.dual, 'Type 2')

type3.single <- c(720, 960, 672, 525, 147, 664, 649, 487, 1200, 331, 300, 2700, 120, 180, 267, 3101, 153, 900)
type3.dual <- c(1020, 475, 1408, 850, 480, 294, 599, 470, 125, 921, 201, 553, 1200, 300, 883, 627, 900, 289)
wilcoxon_test(type3.single, type3.dual, 'Type 3')

type4.single <- c(180, 390, 468, 320, 128, 270, 379, 131, 220, 926, 51, 600, 540, 300, 64, 146, 337, 900)
type4.dual <- c(540, 971, 260, 1175, 296, 227, 342, 157, 471, 259, 343, 293, 243, 600, 183, 429, 900, 229)
wilcoxon_test(type4.single, type4.dual, 'Type 4')

boxplot(list(type1.single, type1.dual, type2.single, type2.dual, type3.single, type3.dual, type4.single, type4.dual), 
		main = 'Different types', ylab = 'Time in second', 
		col=rep(c('white', 'grey')) )
legend(x=.5, y=3100, legend=c('single', 'dual'), fill=c('white', 'grey'))


jhot.single <- c(1260, 180, 330, 193, 1134, 320, 780, 181, 514, 379, 241, 327, 331, 926, 300, 2700, 780, 540, 300, 534, 3101, 146, 153, 900)
jhot.dual <- c(180, 540, 475, 199, 1080, 1175, 38, 114, 780, 342, 407, 958, 921, 259, 201, 553, 590, 243, 300, 329, 627, 429, 900, 289)
wilcoxon_test(jhot.single, jhot.dual, 'JHotDraw')

jref.single <- c(360, 955, 390, 781, 525, 147, 128, 664, 1015, 490, 131, 382, 186, 1620, 51, 1200, 120, 180, 300, 267, 717, 228, 337, 900)
jref.dual <- c(1080, 822, 971, 1408, 850, 480, 296, 294, 399, 310, 157, 456, 275, 1134, 343, 1420, 1200, 300, 600, 883, 294, 900, 900, 344)
wilcoxon_test(jref.single, jref.dual, 'JRefactory')

padl.single <- c(720, 960, 672, 468, 688, 1080, 578, 270, 649, 487, 1200, 220, 292, 62, 120, 600, 960, 420, 550, 64, 120, 211, 900, 900)
padl.dual <- c(1020, 761, 327, 260, 397, 840, 150, 227, 599, 470, 125, 471, 1183, 300, 474, 293, 195, 420, 343, 183, 90, 600, 892, 229)
wilcoxon_test(padl.single, padl.dual, 'PADL')

boxplot(list(jhot.single, jhot.dual, jref.single, jref.dual, padl.single, padl.dual), 
		main = 'Different projects', ylab = 'Time in second', 
		col=rep(c('white', 'grey')) )
legend(x=5.5, y=3100, legend=c('single', 'dual'), fill=c('white', 'grey'))

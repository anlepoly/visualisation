
chisq_test <- function(file) {
	data <- as.data.frame(read.csv(file, header = T))

	tbl <- table(data$education, data$score)
	print(chisq.test(tbl))

	tbl <- table(data$institution, data$score)
	print(chisq.test(tbl))

	tbl <- table(data$Java, data$score)
	print(chisq.test(tbl))    

	tbl <- table(data$UML, data$score)
	print(chisq.test(tbl))

	tbl <- table(data$SE, data$score)
	print(chisq.test(tbl) )

	tbl <- table(data$English, data$score)
	print(chisq.test(tbl))
}

chisq_test('time_background.csv')
chisq_test('correction_background.csv')

library(ggplot2)

#age, birth order, ethnicity, sex, mom ed

model_age = function() #for all rows with just age
{

	vocab = read.table("./wordbank/vocabulary_norms_data.csv",header = TRUE, sep =",",quote="") 

    summary(lm(vocab$X.vocab ~ vocab$X.age.))

}


model_gen = function() #for all rows with just age and sex
{
	vocab = read.table("./wordbank/vocabulary_norms_data.csv",header = TRUE, sep =",",quote="")

	genCodes = as.integer(vocab$X.sex.)
	f = as.integer(genCodes == 1)
	m = as.integer(genCodes == 2)

    print(summary(lm(vocab$X.vocab ~ f + m)))
    print(summary(lm(vocab$X.vocab ~ m + f)))
} 

model_race = function()
{

    vocab = read.table("./wordbank/vocabulary_norms_data.csv",header = TRUE, sep =",",quote="")

    ethCodes = as.integer(vocab$X.ethnicity.)
	asian = as.integer(ethCodes == 1)
	black = as.integer(ethCodes == 2)
	hisp = as.integer(ethCodes == 3)
	other = as.integer(ethCodes == 4)
	white = as.integer(ethCodes == 5)

    print(summary(lm(vocab$X.vocab ~ asian + black + hisp + other + white)))
    print(summary(lm(vocab$X.vocab ~ white + asian + black + hisp + other)))

}


model_mom = function()
{

	vocab = read.table("./wordbank/vocabulary_norms_data.csv",header = TRUE, sep =",",quote="")

	edCodes = as.integer(vocab$X.mom_ed.)
	college = as.integer(edCodes == 1)
	grad = as.integer(edCodes == 2)
	prim = as.integer(edCodes == 3)
	sec = as.integer(edCodes == 4)
	someCollege = as.integer(edCodes == 5)
	someGrad = as.integer(edCodes == 6)
	someSec = as.integer(edCodes == 7)

	lmout = summary(lm(vocab$X.vocab ~ prim + someSec +sec +   someCollege  +college + someGrad + grad ))
	print(lmout)
	print(summary(lm(vocab$X.vocab ~  someSec +sec +   someCollege  +college + someGrad + grad )))
	
	y =lmout$coefficients[2:7]
	xlab = c("Primary", "Some Secondary", "Secondary", "Some College", "College", "Some Graduate")	
	x = 1:6
	df = data.frame(momed = x, coeff = y)
	ggplot(df) + geom_line(aes(x = momed, y = coeff)) + 
	labs(x ="Mother's Education", y = "Coefficients") +
	scale_x_continuous(breaks=1:6, labels = xlab) +
	ggtitle("Coefficients for Vocabulary vs Mother's Education Linear Model");
  	ggsave("MomEd.png")
}


model_bo = function() #for all rows with all variable entries
{
	vocab = read.table("./wordbank/vocabulary_norms_data.csv",header = TRUE, sep =",",quote="")
	# vocab[is.na(vocab)] = 0
	#order1 = c("\"First\"", "\"Second\"", "\"Third\"", "\"Fourth\"", "\"Fifth\"", "\"Sixth\"", "\"Seventh\"", "\"Eighth\"")
	#order2 = c(1, 2, 3, 4, 5, 6, 7, 8)
	#for(i in 1:8)
	#{
		#vocab[vocab$X.birth_order. == order1[i]] = order2[i]
	#}

	#dummy vars
	#ethnicity
	

	boCodes = as.integer(vocab$X.birth_order.)
	b8 = as.integer(boCodes == 1)
	b5 = as.integer(boCodes == 2)
	b1 = as.integer(boCodes == 3)
	b4 = as.integer(boCodes == 4)
	b2 = as.integer(boCodes == 5)
	b7 = as.integer(boCodes == 6)
	b6 = as.integer(boCodes == 7)
	b3 = as.integer(boCodes == 8)

	lmout = summary(lm(vocab$X.vocab ~ b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8))
	print(summary(lm(vocab$X.vocab ~ b2 + b3 + b4 + b5 + b6 + b7 +b8)))
		    
	print(lmout)
	y =lmout$coefficients[2:8]
	x = 1:7	
	df = data.frame(birth_order = x, coeff = y)
	ggplot(df) + geom_line(aes(x = birth_order, y = coeff)) + 
	labs(x = "Birth Order", y = "Coefficients") +
	ggtitle("Coefficients for Vocabulary vs Birth Order Linear Model");
  	ggsave("BirthOrder.png")
}

model_all = function()
{
	print(model_age())
	model_gen()
	model_bo()
	model_mom()
	model_race()
}


plot = function(A)
{


}

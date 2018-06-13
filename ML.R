library(ggplot2)

mergeall = function()
{
	item= read.table("./ml-100k/u.item",header = FALSE, sep ="|",quote="", col.names = c("movieId", "movieTitle", "releaseDate", "videoReleaseDate", "url", "unk", "act", "adv" ,"ani", "child", "comedy", "crime", "doc" , "drama", "fant", "noir", "horr", "mus", "mys", "rom" ,"sci", "thiller" , "war", "west"))

	data= read.table("./ml-100k/u.data",header = FALSE, col.names = c("UserId","movieId", "rating", "time"))


	user= read.table("./ml-100k/u.user",header = FALSE, sep ="|",quote="", col.names = c("UserId", "age", "gender", "occupation", "zipcode"))

	m1 = merge(data, user)
	m2 = merge(m1, item)


	write.table(m2, file = "./u.all", row.names = FALSE, append = FALSE, quote = FALSE, sep = "|")


}

propf = function()
{
	user= read.table("./ml-100k/u.user",header = FALSE, sep ="|",quote="", col.names = c("UserId", "age", "gender", "occupation", "zipcode"))	
	gen = user[,3]
	numF = length(which(gen == "F"))

	numF/length(gen)
}

oldest = function()
{
	user = read.table("./ml-100k/u.user",header = FALSE, sep ="|",quote="", col.names = c("UserId", "age", "gender", "occupation", "zipcode"))	
	ages = user[,2]
	max(ages)
}

mostactive = function()
{
	data= read.table("./ml-100k/u.data",header = FALSE, col.names = c("UserId","movieId", "rating", "time"))
	ids = data[,1]
	
	numratings = rep(0, 943)
	for(i in 1:length(ids))
	{
		numratings[ids[i]] = numratings[ids[i]] + 1
	}

	return (max(numratings))
}

#
# ^^^^^^^^^^^^^^^^^^^^^   ABOVE FROM HW4    ^^^^^^^^^^^^^^^^
#	============	final project below		===========
#

findA = function()
{
	all = read.table("u.all",header = TRUE, sep ="|",quote="")
	#only care about: gender, userId, rating
	gender = all$gender
	UserId = all$UserId
	rating = all$rating

	allRed = data.frame(gender = gender, UserId = UserId, rating = rating)	#reduced version of all
	# to sort: allRed[ do.call(order, allRed), ]

	#find average ratings

	A = tapply(rating, UserId, mean, simplify = TRUE)	#take mean of ratings by UserId

}

#part a
meanM = function(A)
{
	user = read.table("./ml-100k/u.user",header = FALSE, sep ="|",quote="", col.names = c("UserId", "age", "gender", "occupation", "zipcode"))
	userGenderm = which(user$gender == 'M') #indices of the men
	mm = A[userGenderm]	#mean ratings among men
}

#part b
meanF = function(A)
{
	user = read.table("./ml-100k/u.user",header = FALSE, sep ="|",quote="", col.names = c("UserId", "age", "gender", "occupation", "zipcode"))
	userGenderf = which(user$gender == 'F')
	mf = A[userGenderf]	#mean ratings among women
}
#use t.test(mm) or t.test(mf) for confidence intervals

#part c
meanDiff = function(A)
{
	#need previous values
	user= read.table("./ml-100k/u.user",header = FALSE, sep ="|",quote="", col.names = c("UserId", "age", "gender", "occupation", "zipcode"))
	userGenderm = which(user$gender == 'M') #indices of the men
	mm = A[userGenderm]	#mean ratings among men
	userGenderf = which(user$gender == 'F')
	mf = A[userGenderf]	#mean ratings among women

	Mbar = 3.588604	#from t.test results
	Fbar = 3.587179

	#find s by eqn 9.23
	Nmen = length(mm)
	Nwomen = length(mf)
	s2men = 1/Nmen*sum((mm - Mbar)^2)
	s2women = 1/Nwomen*sum((mf - Fbar)^2)

	# find interval with eqn 10.21
	marginOfError = 1.96*sqrt(s2men/Nmen + s2women/Nwomen)
	
	interval = c(Mbar - Fbar - marginOfError, Mbar - Fbar + marginOfError)
}

#part d
sigtest = function(A)
{
	#need previous values
	user= read.table("./ml-100k/u.user",header = FALSE, sep ="|",quote="", col.names = c("UserId", "age", "gender", "occupation", "zipcode"))
	userGenderm = which(user$gender == 'M') #indices of the men
	mm = A[userGenderm]	#mean ratings among men
	userGenderf = which(user$gender == 'F')
	mf = A[userGenderf]	#mean ratings among women

	Mbar = 3.588604	#from t.test results
	Fbar = 3.587179

	#find s by eqn 9.23
	Nmen = length(mm)
	Nwomen = length(mf)
	s2men = 1/Nmen*sum((mm - Mbar)^2)
	s2women = 1/Nwomen*sum((mf - Fbar)^2)
    
    #standard error of Mbar - Fbar using eqn (10.18-20)
    error = sqrt(s2men/Nmen + s2women/Nwomen)
    
    # find Z with eqn 11.6
    z = (Mbar - Fbar) / error
}

#part e
plot = function(A)
{
  mm = data.frame(rating = meanM(A))
  mf = data.frame(rating = meanF(A)) 
  
  mm$sex = 'male'
  mf$sex = 'female'
  ratings = rbind(mm, mf)
  
  ggplot(ratings, aes(x=rating, fill=sex)) + 
  geom_histogram(binwidth=0.5, color="black", alpha=0.5) + 
  ggtitle("Average Ratings by Gender");
  ggsave("plot.png")
}

#part f
makeIdFile = function()	#create ids file for testing
{
	data= read.table("./ml-100k/u.data",header = FALSE, col.names = c("UserId","movieId", "rating", "time"))
	ids = data$UserId
	write.table(ids, file = "./ids.all", row.names = FALSE, append = FALSE, quote = FALSE, sep = "|")
}

#this part is analogous to part c
findNumRat = function()	#finds the number of ratings for each user 
{

	all = read.table("u.all",header = TRUE, sep ="|",quote="")
	#only care about: gender, userId, rating
	gender = all$gender
	UserId = all$UserId
	rating = all$rating

	allRed = data.frame(gender = gender, UserId = UserId, rating = rating)	#reduced version of all
	# to sort: allRed[ do.call(order, allRed), ]

	#find average ratings

	NumRat = tapply(rating, UserId, length, simplify = TRUE)	#find amount of ratings by UserId

}
#finds the differnece between the mean NUMBER of ratings among the men and women
DiffNumRat = function(NumRat)	
{
	user = read.table("./ml-100k/u.user",header = FALSE, sep ="|",quote="", col.names = c("UserId", "age", "gender", "occupation", "zipcode"))
	userGenderm = which(user$gender == 'M') #indices of the men
	nm = NumRat[userGenderm]	#number of ratings among men
	userGenderf = which(user$gender == 'F') #indices of the women
	nf = NumRat[userGenderf]	#number of ratings among women

	#run t.test(nm) and t.test(nf) to find bar vals

	#same method as part c
	Mbar = 110.8358	#from t.test results
	Fbar = 94.28571

	#find s by eqn 9.23
	Nmen = length(nm)
	Nwomen = length(nf)
	s2men = 1/Nmen*sum((nm - Mbar)^2)
	s2women = 1/Nwomen*sum((nf - Fbar)^2)

	# find interval with eqn 10.21
	marginOfError = 1.96*sqrt(s2men/Nmen + s2women/Nwomen)
	
	interval = c(Mbar - Fbar - marginOfError, Mbar - Fbar + marginOfError)
}

#part g
PopPropM = function(A)
{
    #need previous values
    user = read.table("./ml-100k/u.user", header = FALSE, sep ="|", quote="", col.names = c("UserId", "age", "gender", "occupation", "zipcode"))
    userGenderm = which(user$gender =='M')
    mm = A[userGenderm]

    #t.test results
    Mbar = 3.588604

    all = read.table("u.all", header = TRUE, sep = "|", quote="")
    gender = all$gender

    #create frame with only gender
    allRed = data.frame(gender = gender, UserId = UserId)
    #creates frame with all duplicate UserIds removed
    allUnique = unique(allRed, by = UserId)

    #number of males
    numM = sum(allUnique == 'M')
    #number of females
    numF = sum(allUnique == 'F')
    #find total of males and females
    total = numM + numF

    #phat
    phat = numM / total

    #find margin of error
    Nmen = length(mm)
    smen = sqrt(1/Nmen*sum((mm-Mbar)^2))
    marginOfError = 1.96*smen/sqrt(Nmen)

    interval = c(phat - marginOfError, phat + marginOfError)

}

#part h
BetaAge = function()
{
	#from section 12.6
	all = read.table("u.all",header = TRUE, sep ="|",quote="")
	summary(lm(formula = all$rating ~ all$age + all$gender))
	#results
		# 	Call:
		# lm(formula = all$rating ~ all$age + all$gender)

		# Residuals:
		#     Min      1Q  Median      3Q     Max 
		# -2.7407 -0.5617  0.3921  0.5355  1.6098 

		# Coefficients:
		#               Estimate Std. Error t value Pr(>|t|)    
		# (Intercept)  3.3598944  0.0121603 276.299   <2e-16 ***
		# all$age      0.0053107  0.0003076  17.266   <2e-16 ***
		# all$genderM -0.0069035  0.0081345  -0.849    0.396    
		# ---
		# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

		# Residual standard error: 1.124 on 99997 degrees of freedom
		# Multiple R-squared:  0.002973,	Adjusted R-squared:  0.002953 
		# F-statistic: 149.1 on 2 and 99997 DF,  p-value: < 2.2e-16

	BetaAge = 0.0053107
	stdError = 0.0003076
	interval = c(BetaAge - 1.96*stdError, BetaAge + 1.96*stdError)
}
female28 = function()
{
	all = read.table("u.all",header = TRUE, sep ="|",quote="")

	#following section 12.11
	lmout = lm(formula = all$rating ~ all$age + all$gender)
	A = vcov(lmout)
	#levels(all$gender) 	#female = 1
	#[1] "F" "M"
	b = matrix(c(1,28,1), ncol = 3)
	varHat = b %*% A %*% t(b)

	stdError = sqrt(varHat)
	stdError = stdError[1,1]

	beta = c(3.3598944, 0.0053107, -0.0069035)
	meanRatWomen28 = sum(c(1, 28, 1)*beta)

	interval = c(meanRatWomen28 - 1.96*stdError, meanRatWomen28 + 1.96*stdError)
}

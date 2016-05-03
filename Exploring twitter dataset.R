set.seed(100)
trails <- 1000
min <- 1                                  
max <- 100
uni_dist<-runif(trails,min,max+1)
uni_dist<-data.frame(x=uni_dist)
head(uni_dist)

qplot(x=x,data=uni_dist,binwidth=10)
qplot(x=x,data=uni_dist,binwidth=10)+ scale_x_continuous(breaks=seq(0,100,10))


min <- 1                                  
max <- 6
die <- as.integer(runif(trails,min,max+1))
die<-data.frame(roll=die)
head(die)
str(die)
qplot(x=roll,data=die, color=I('#615445'), fill=I('#CC0000'))+ scale_x_discrete(breaks=1:6)

dice <- as.integer(runif(trails,min,max+0.5)+runif(trails,min,max+0.5))
dice<-data.frame(rolls=dice)
head(dice)

qplot(x=rolls,data=dice, color=I('#615445'), fill=I('#CC0000')) + scale_x_discrete(breaks=2:12) +  labs(title="Distribution Sum of Two Dice", y="Count", x="Sum of Two Uniform Random Variables (Dice) n=3333 Min=1 Max=6")


norm_dist<-rnorm(10000, mean=50, sd=20)
norm_denisty<-dnorm(norm_dist)
ggplot(data.frame(x=norm_dist,y=norm_denisty)) + aes(x=x,y=y) + geom_point() + labs(title="Standard Normal Distribution", y="Count", x="Normal Random Variable n=3333 mean=0, SD=1")


bi_dist<-rbinom(n=6,size=9,prob=0.435)
bi_dist
summary(bi_dist)
qplot(x=x,data=data.frame(x=bi_dist), color=I('#615445'), fill=I('#CC0000')) + scale_x_discrete(breaks=0:9) + labs(title="Binomial Distribution", y="Count", x="Binomial n=3333,size=9,prob=0.33")

lambda1=rpois(n=trails,lambda=1)



cauchy<- rcauchy(n=10, location = 0, scale = 1)
plot(cauchy, type="o", col=2)

summary(cars)
cars
qplot(speed,dist, data=cars)
head(diamonds)




data(mpg)
data(economics)
data(diamonds)
data_url <- 'http://54.88.34.236/YouTube/MachineLearning/M01/M01_quasi_twitter.csv'
twitter <- read.csv(url(data_url))
A=rcauchy(n=10, location = 0, scale = 0.5)
A
pcauchy(A,location=0,scale=0.5)
require(reshape2)
r_cauchy <-data.frame(A=rcauchy(n=10, location = 0, scale = 0.5),
                      B=rcauchy(n=10, location = 0, scale = 1),
                      C=rcauchy(n=10, location = 0, scale = 2),
                      D=rcauchy(n=10, location = -2, scale = 1)) 

rnd <- melt(data=r_cauchy)
ggplot(rnd, aes(x=(value))) + geom_density(aes(group=variable,color=variable)) + labs(title="Cauchy Distributions", y="P(X)", x=" x ")

data<- read.csv("M01_Lesson_02_Q1.csv")
str(data)
require(outliers)
qplot(x=X, data=data,binwidth=1) #UNIFORM
summary(data$X)

qplot(x=A, data=data,binwidth=0.5) #normal
summary(data$A)
sd(data$A)
shapiro.test(data$A)
grubbs.test(data$X)
qqnorm(data$A)
qqline(data$A)
qplot(x=B, data=data,binwidth=0.5)
shapiro.test(data$B)
grubbs.test(data$B)
qqnorm(data$B)
qqline(data$B)
summary(data$B)
qplot(x=C, data=data,binwidth=1)
shapiro.test(data$C)

qqnorm(data$C)
qqline(data$C)
summary(data$C)
sd(data$E)
dat<-rnorm(333,mean=6.3,sd=1.48)
dat_density<-dnorm(dat)
ggplot(data.frame(x=dat,y=dat_density)) + aes(x=x,y=y) + geom_point()
grubbs.test(data$C)

qplot(x=D, data=data,binwidth=1)
shapiro.test(data$D)
qqnorm(data$D)
qqline(data$D)
summary(data$D)
dat<-rnorm(333,mean=8.919,sd=3.13)
dat_density<-dnorm(dat)
ggplot(data.frame(x=dat,y=dat_density)) + aes(x=x,y=y) + geom_point()
grubbs.test(data$E)

qplot(x=E, data=data,binwidth=20)
summary(data$E)




#column1

test<-rep(1:333,1)
uni_dist<-data.frame(x=test,y=data$X)

qplot(x,y,data=uni_dist)+ geom_point(shape=1)


#Columne 2
norm_dist<-rnorm(333, mean=9.079, sd=1.785502)
norm_density<-dnorm(norm_dist)

scatter<- data.frame(generated=norm_density, original=data$A)
qplot(generated,original,data=scatter)

#column 3

norm_dist<-rnorm(333, mean=.03063, sd=0.9995666)
norm_density<-dnorm(norm_dist)

scatter<- data.frame(generated=norm_density, original=data$B)
qplot(generated,original,data=scatter)


#column 4

dat<-rnorm(333,mean=6.3,sd=1.48)
dat_density<-dnorm(dat)
scatter<- data.frame(generated=dat_density, original=data$C)
qplot(generated,original,data=scatter)

#column 5
dat<-rnorm(333,mean=8.919,sd=3.13)
dat_density<-dnorm(dat)
scatter<- data.frame(generated=dat_density, original=data$D)
qplot(generated,original,data=scatter)

#column 6
dat<-rnorm(333,mean=185.90,sd=87.42228)
dat_density<-dnorm(dat)
scatter<- data.frame(generated=dat_density, original=data$D)
qplot(generated,original,data=scatter)
set.seed(333)
trails <- 333
min <- 1                                  
max <- 333
uni_dist<-runif(trails,min,max+1)
scatter<-data.frame(x=uni_dist,y=data$E)
qplot(x,y,data=scatter)


summary(diamonds)
mean(diamonds$carat)
sd(diamonds$carat)
median(diamonds$carat)



#assignment 1b

data1<- read.csv("M01_quasi_twitter.csv")

#column 1
qplot(x=screen_name, data=data1,binwidth=1) #UNIFORM
summary(data1$screen_name)


qplot(x=created_at_month  , data=data1,binwidth=2)
summary(data1$created_at_month)

qqnorm(data1$created_at_month)
qqline(data1$created_at_month)
grubbs.test(data1$created_at_month)

qplot(x=created_at_day  , data=data1,binwidth=1)
summary(data1$created_at_day)
qqnorm(data1$created_at_day)
qqline(data1$created_at_day)

qplot(x=created_at_year   , data=data1,binwidth=1)
summary(data1$created_at_year  )

qplot(x=country   , data=data1,binwidth=10)
summary(data1$country  )


qplot(x=location  , data=data1,binwidth=10)
summary(data1$location  )

qplot(friends_count, data=data1, geom="density")
qplot(friends_count, data=data1, fill=country, geom="density")
qplot(friends_count,screen_name   , data=data1)+geom_violin()
summary(data1$friends_count  )


qplot(x=friends_count, data=data1,color=I('#17331F'), fill=I('#CC0000'))
qplot(x=log(friends_count+1,2), data=data1,color=I('#17331F'), fill=gender)
grubbs.test(data1$friends_count)
qplot(x=log(friends_count+1,2), data=data1,geom="density",color=I('#17331F'), fill=gender)


qplot(x=followers_count   , data=data1,binwidth=1)
summary(data1$followers_count  )

qplot(x=statuses_count   , data=data1,binwidth=1)
summary(data1$statuses_count  )

qplot(x=favourites_count   , data=data1,binwidth=1)
summary(data1$favourites_count )

qplot(x=favourited_count   , data=data1,binwidth=1)
summary(data1$favourited_count  )

qplot(x=dob_day, data=data1,binwidth=1)
summary(data1$dob_day  )


qplot(x=dob_year, data=data1,binwidth=1)
summary(data1$dob_year)


qplot(x=dob_month, data=data1,binwidth=1)
summary(data1$dob_month)


qplot(x=gender, data=data1,binwidth=1)
summary(data1$gender)


qplot(x=mobile_favourites_count, data=data1,binwidth=1)
summary(data1$mobile_favourites_count)

qplot(x=mobile_favourites_count, data=data1,color=I('#17331F'), fill=I('#CC0000'))
qplot(x=log(mobile_favourites_count+1,2), data=data1,color=I('#17331F'), fill=gender)
grubbs.test(data1$mobile_favourites_count)
qplot(x=mobile_favourites_count, data=data1,xlim=c(10,100),color=I('#17331F'), fill=I('#CC0000'))

qplot(x=mobile_favourited_count, data=data1,binwidth=1)
summary(data1$mobile_favourited_count)

qplot(x=education, data=data1,binwidth=1)
summary(data1$education)
qqnorm(data1$education)
qqline(data1$education)
grubbs.test(data1$education)


qplot(x=experience, data=data1,binwidth=1)
summary(data1$experience)
qqnorm(data1$experience)
qqline(data1$experience)
grubbs.test(data1$experience)

qplot(x=age, data=data1,binwidth=1)
summary(data1$age)
qqnorm(data1$age)
qqline(data1$age)
grubbs.test(data1$age)

qplot(x=race, data=data1,binwidth=1)
summary(data1$race)


qplot(x=wage, data=data1,binwidth=1)
summary(data1$wage)

qqnorm(data1$wage)
qqline(data1$wage)
grubbs.test(data1$wage)

qplot(x=retweeted_count  , data=data1,binwidth=1)
summary(data1$retweeted_count  )
qplot(x=retweeted_count, data=data1,color=I('#17331F'), fill=I('#CC0000'))
qplot(x=log(retweeted_count+1,2), data=data1,color=I('#17331F'), fill=gender)
grubbs.test(data1$retweeted_count)
qplot(x=retweeted_count, data=data1,xlim=c(10,100),color=I('#17331F'), fill=I('#CC0000'))


qplot(x=height, data=data1,binwidth=1)

summary(data1$height)
qplot(x=height, data=data1,binwidth=1,xlim=c(140,210))
qqnorm(data1$height)
qqline(data1$height)
grubbs.test(data1$height)












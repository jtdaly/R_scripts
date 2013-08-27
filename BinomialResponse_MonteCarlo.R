n=1000;   							# number of iterations
j=0;   								# count variable

x=cbind(array(0, c(1,15)), array(1, c(1,15)))  		#create x vector
x=as.vector(x)


#Check Type I error to ensure it is reasonable 
for(k in c(1:n)){
y=rbinom(length(x), 1, 0.8)				#generate data under the null hypothesis
logit.fit <- glm(y~x, family=binomial)		#fit logistic regression
pv = summary(logit.fit)$coef[2,4]			#pull out p-value for the one factor
#if (pv<1){print(pv)}
if (pv < 0.24){j=j+1}  					#count number of incorrect decisions (note I used 0.24 to get closer to 20% size)
}
TypeI=j/n								#compare actual type I error to confidence level (sanity check)
TypeI


p=as.vector(array(0,c(1,length(x))))

for(l in c(1:length(x))){					#create alternative hypothesis
  if (x[l]==0){p[l]=.8}					#p = 0.8 if x = 0
  else {p[l]=.9}						#p = 0.9 if x = 1
}


j=0;  								#reinitialize counting variable
for(k in c(1:n)) {
  y=as.vector(array(0,c(1,length(x))))		
  
  for(l in c(1:length(x))){
    y[l]=rbinom(1, 1, p[l])				#generate data under the alternative hypothesis
  }
  
  logit.fit <- glm(y~x, family=binomial)		#fit logistic regression
  pv = summary(logit.fit)$coef[2,4]			#p-value for factor
  
  if (pv < 0.24){j=j+1}					#count the number of correct decisions
  
}

Power=j/n								#calculate power
Power


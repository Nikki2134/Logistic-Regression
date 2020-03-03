data <- read.csv("C://Users//gsnik//Desktop//college//Sem4//Predictive//Logistic Regression//heart.csv",sep = ',', header = TRUE)
data = data.frame(data)

y = data$target
x1 = data$age
x2 = data$sex
x3 = data$cp
x4 = data$trestbps
x5 = data$chol
x6 = data$fbs
x7 = data$restecg
x8 = data$thalach

print(data$age)


logreg = glm(y ~ x1+x2+x3+x4+x5+x6+x7+x8, family = binomial(link = "logit"))
t = summary(logreg)
t
#t$coefficients
#t$coefficients[1]


#row8
#data can be taken as input also and then passed to predict
x = data.frame(x1=10, x2=115, x3=0, x4=0, x5=0, x6=35.3, x7=0.134, x8=29)
if(predict(logreg, x) > 0.5) {
  print("The person is diabetic")
}else{
  print("The person is not diabetic")
}



x1_val = seq(from=min(x1), to=max(x1), length.out=length(x1))
x2_val = seq(from=min(x2), to=max(x2), length.out=length(x2))
x3_val = seq(from=min(x3), to=max(x3), length.out=length(x3))
x4_val = seq(from=min(x4), to=max(x4), length.out=length(x4))
x5_val = seq(from=min(x5), to=max(x5), length.out=length(x5))
x6_val = seq(from=min(x6), to=max(x6), length.out=length(x6))
x7_val = seq(from=min(x7), to=max(x7), length.out=length(x7))
x8_val = seq(from=min(x8), to=max(x8), length.out=length(x8))

y_pred = 0.248 + (-0.022*x1_val) + (-2.024*x2_val) + (0.924*x3_val) + (-0.025*x4_val) + (-0.005*x5_val) + (-0.109*x6_val) + (0.382*x7_val) + (0.041*x8_val)
y_ans = exp(y_pred)/(1+exp(y_pred))
plot.new()
par(mfrow=c(2, 4))
plot(x1_val,y_ans, col = 'Blue', type='l', xlab = 'Age', ylab = 'Heart Disease')
plot(x2_val,y_ans, col = 'Red', type='l', xlab = 'Sex', ylab = 'Heart Disease')
plot(x3_val,y_ans, col = 'Green', type='l', xlab = 'Chest Pain Type', ylab = 'Heart Disease')
plot(x4_val,y_ans, col = 'coral', type='l', xlab = 'Resting BP', ylab = 'Heart Disease')
plot(x5_val,y_ans, col = 'orange', type='l', xlab = 'Cholestrol', ylab = 'Heart Disease')
plot(x6_val,y_ans, col = 'Violet', type='l', xlab = 'Fasting BS', ylab = 'Heart Disease')
plot(x7_val,y_ans, col = 'antiquewhite4', type='l', xlab = 'Resting ecg', ylab = 'Heart Disease')
plot(x8_val,y_ans, col = 'Turquoise', type='l', xlab = 'Maximum Heart Rate', ylab = 'Heart Disease')

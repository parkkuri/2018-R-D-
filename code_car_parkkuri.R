########################
# # 차량 도난 공모전 # #
########################
setwd("C:/Users/laep9/Desktop/bigdata/driving")
rm(list=ls())
getwd()

#데이터 불러오기
drive_train <- read.csv("KU-Driving-Dataset-pre-train.csv", stringsAsFactors = F,
                        header = T)

#데이터 구조 확인
str(drive_train)
head(drive_train)
tail(drive_train)
colSums(is.na(drive_train)) #결측치 없음

#마지막 행은 라벨. 펙터처리
unique(drive_train$Driver)
drive_train$Driver <- as.factor(drive_train$Driver)


#EDA 탐색적 데이터 분석 - 히스토그램
names(table(drive_train$Fuel_Pressure))!=0


for(i in 1:(length(drive_train)-1)){
  if(i%%9==0| i==1){ # i가 9로 나누어떨어지거나 1이면
    x11() #새창을 열어서
    par(mfrow=c(3,3)) # palet를 3x3으로 나눠라
  }
  if(as.numeric(drive_train[,i])){
    hist(drive_train[,i],drive_train[,30],
            main = colnames(drive_train)[i], breaks=seq(min(drive_train[,i]),max(drive_train[,i])+0.1,0.1))
  }
}



hist(drive_train[,i],drive_train[,30])

summary(drive_train[,i])

class(drive_train[,i])



#모든 열이 0인거 제거(4개)
drive_train2 <- subset(drive_train, select = -c(Filtered_Accelerator_Pedal_value,
                                                Inhibition_of_engine_fuel_cut_off,
                                                Fuel_Pressure,
                                                Glow_plug_control_request))
#nearzerovar 사용 - 분산 확인
library(caret)
nearZeroVar(drive_train2[,-51],saveMetrics=TRUE)

#nearzerovar결과를 drive3에 새로 할당
drive3 <- drive_train2[,-nearZeroVar(drive_train2)]
drive3

write.csv(drive_train2, "drive_train51.csv")
getwd()

#상관성 확인 - 상관성 높은 열(변수) 제거
findCorrelation(cor(subset(drive_train2, select=-c(Driver))))
findCorrelation(cor(drive_train2[,-51]))

cor(drive_train2[,-51])

table(is.na(cor(drive_train2[,-51])))
#FALSE  TRUE 
#2306   194 

#drive3로 correlation 확인
drive_train4 <- findCorrelation(cor(drive3[,-38]))

#결과값을 train_final로 새로운 객체 지정 - 총 변수 30개로 줄어듬
drive_final <- drive3[,-drive_train4]
write.csv(drive_final,"drive_final.csv")

#k-fold vaildation
library(randomForest)
drive_final$Driver <- as.factor(drive_final$Driver)

idx <- createFolds(drive_final[,30], k=10) #k를 10으로 지정
as.vector(idx[1])
idx[[1]] #idx꺼내기
accuracy_full<-c() #값을 넣어놓을 빈 벡터 생성

#Random Forest(rf) 실행
for (i in 1:10){
training_fold <- drive_final[-idx[[i]],] #각 fold를 training fold에 넣음
test_fold = drive_final[idx[[i]],]

classifier = randomForest( Driver~., #랜덤포레스트 사용
                           data = training_fold )
y_pred = predict ( classifier, newdata = test_fold ) 
cm = table ( test_fold[,30], y_pred ) #얼마나 정확한지 table로 비교
accuracy = sum(diag(cm))/sum(cm) #각 fold의 정확도 합

accuracy_full<-c(accuracy_full,accuracy) #앞에서 생성된 빈 벡터에 값을 할당
}

#########결과값으로 나온 accurarcy_full의 정확도의 평균값을 구함
mean(accuracy_full) # 0.9991182
write.csv(accuracy_full, "accuracy_full.csv") #중간 저장

########따로 rf돌림(중요한 변수가 무엇인지 알아내기 위해)- 변수 30개
rf_drive = randomForest( Driver~., data = training_fold )
write.csv(rf_drive, "rf_drive.csv")
importance(rf_drive)

#나온 결과 시각화(높을 수록 중요한 변수)
varImpPlot(rf_drive)


########우리가 뽑은 8개 변수로만 rf 돌림
drive_pick <- subset(drive_train2, select = c(Current_Gear,
                                              Clutch_operation_acknowledge,
                                              Vehicle_speed,
                                              Acceleration_speed_._Longitudinal,
                                              Indication_of_brake_switch_ON.OFF,
                                              Acceleration_speed_._Lateral,
                                              Steering_wheel_speed,
                                              Steering_wheel_angle,
                                              Driver
                                              
))

accuracy_7vari<-c()

for (i in 1:10){
  training_fold <- drive_pick[-idx[[i]],]
  test_fold = drive_pick[idx[[i]],]
  
  classifier = randomForest( Driver~.,
                             data = training_fold )
  y_pred = predict ( classifier, newdata = test_fold )
  cm = table ( test_fold[,9], y_pred )
  accuracy = sum(diag(cm))/sum(cm)
  
  accuracy_7vari<-c(accuracy_7vari,accuracy)
}

accuracy_7vari
mean(accuracy_7vari)
#결과: 0.3443913


#######데이터만으로 뽑은 30개 변수로 svm 돌림
#install.packages("e1071");
library(e1071)

accuracy_full_svm<-c()
ncol(drive_final)

drive_final2<-sapply(drive_final[,c(-28,-30)],scale)
str(drive_final[,c(-28,-30)])
head(drive_final2)

drive3<-cbind(drive_final2,drive_final[,c(28,30)])



for (i in 1:10){
  training_fold <- drive3[-idx[[i]],]
  test_fold = drive3[idx[[i]],]
  
  classifier = svm( Driver~.,
                             data = training_fold )
  y_pred = predict ( classifier, newdata = test_fold )
  cm = table ( test_fold[,30], y_pred )
  accuracy = sum(diag(cm))/sum(cm)
  
  accuracy_full_svm<-c(accuracy_full_svm,accuracy)
}


accuracy_full_svm
mean(accuracy_full_svm) #0.9429984


########우리가 뽑은은 8개 변수로 svm 돌림
accuracy_pick_svm<-c()


drive_pick<-sapply(drive_pick[,-9],scale)
str(drive_pick[,-9])
head(drive_pick)

drive_pick<-cbind(drive_pick,drive_pick[,-9])




for (i in 1:10){
  training_fold <- drive_pick[-idx[[i]],]
  test_fold = drive_pick[idx[[i]],]
  
  classifier = svm( Driver~.,
                    data = training_fold )
  y_pred = predict ( classifier, newdata = test_fold )
  cm = table ( test_fold[,9], y_pred )
  accuracy = sum(diag(cm))/sum(cm)
  
  accuracy_pick_svm<-c(accuracy_pick_svm,accuracy)
}
accuracy_pick_svm
mean(accuracy_pick_svm) #0.256067

########trip 변수을 더미 변수로 만들기
library(caret)
drive_dm = dummyVars('~Trip', drive_final)
drive_dm = data.frame(predict(drive_dm,drive_final))

drive_dummy = cbind(drive_final, drive_dm)

#원변수 인 trip 삭제
drive_dummy2 = subset(drive_dummy, select = -Trip)


########더미변수 별 rf
accuracy_dummy_rf<-c()
#library(randomForest)

for (i in 1:10){
  training_fold <- drive_dummy2[-idx[[i]],]
  test_fold = drive_dummy2[idx[[i]],]
  
  classifier = randomForest( Driver~.,
                             data = training_fold )
  y_pred = predict ( classifier, newdata = test_fold )
  cm = table ( test_fold[,29], y_pred )
  accuracy = sum(diag(cm))/sum(cm)
  
  accuracy_dummy_rf<-c(accuracy_dummy_rf,accuracy)
}

accuracy_dummy_rf
mean(accuracy_dummy_rf) #0.9991711 

########정말 trip 더미변수가 중요한 걸까?
rf_dummy_drive = randomForest( Driver~., data =  drive_dummy2 )
write.csv(rf_dummy_drive, "rf_dummy_drive.csv")
importance(rf_dummy_drive)
#위 결과 시각화
varImpPlot(rf_dummy_drive)


########각 trip level별 정확도 
##trip 1
trip1 <- subset(drive_final, Trip == 1)
trip1 <- subset(trip1, select = -Trip)

accuracy_trip1<-c()
#library(randomForest)

for (i in 1:10){
  training_fold <- trip1[-idx[[i]],]
  test_fold = trip1[idx[[i]],]
  
  classifier = randomForest( Driver~.,
                             data = training_fold )
  y_pred = predict ( classifier, newdata = test_fold )
  cm = table ( test_fold[,29], y_pred )
  accuracy = sum(diag(cm))/sum(cm)
  
  accuracy_trip1<-c(accuracy_trip1,accuracy)
}

accuracy_trip1
mean(accuracy_trip1) #0.9988668

##trip 2
trip2 <- subset(drive_final, Trip == 2)
trip2 <- subset(trip2, select = -Trip)

accuracy_trip2<-c()


for (i in 1:10){
  training_fold <- trip2[-idx[[i]],]
  test_fold = trip2[idx[[i]],]
  
  classifier = randomForest( Driver~.,
                             data = training_fold )
  y_pred = predict ( classifier, newdata = test_fold )
  cm = table ( test_fold[,29], y_pred )
  accuracy = sum(diag(cm))/sum(cm)
  
  accuracy_trip2<-c(accuracy_trip2,accuracy)
}

accuracy_trip2
mean(accuracy_trip2) #0.9997377



##trip 3
trip3 <- subset(drive_final, Trip == 3)
trip3 <- subset(trip3, select = -Trip)

accuracy_trip3<-c()


for (i in 1:10){
  training_fold <- trip3[-idx[[i]],]
  test_fold = trip3[idx[[i]],]
  
  classifier = randomForest( Driver~.,
                             data = training_fold )
  y_pred = predict ( classifier, newdata = test_fold )
  cm = table ( test_fold[,29], y_pred )
  accuracy = sum(diag(cm))/sum(cm)
  
  accuracy_trip3<-c(accuracy_trip3,accuracy)
}

accuracy_trip3
mean(accuracy_trip3) #0.9993183


########test랑 정확도 비교
test<-read.csv('KU-Driving-Dataset-pre-test.csv')
a<-intersect(colnames(drive_final),colnames(test))

b<-test[,a]

###최종모델, test에 trip이 1밖에 없으므로 1만있는 데이터 사용
library(randomForest)
rf_dummy_drive = randomForest( Driver~., data =  trip1 )
test$Trip<-as.factor(test$Trip)

pre_test <- predict(rf_dummy_drive,test)

final_test <- cbind(IndexNumber = test$Index.Number,Driver = pre_test)

write.csv(final_test,'final_test.csv')
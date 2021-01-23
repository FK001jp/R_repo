library(dplyr)
library(ggplot2)
library(rlang)
library(stringr)
library(tidyr)
library(functional)
library(RPostgreSQL)

setwd('/Users/kimishi/Desktop/quad_190110')
data1 <- read.csv('for_plot_190112_1.csv',header = T,stringsAsFactors = F,sep=',')

con <- dbConnect(PostgreSQL(), host="localhost", port=5432, user="kimishi", password="********", dbname="postgres")
heavy_query <- "SELECT t1.*,t2.artist_genres FROM tracks as t1 left join artists as t2 on t1.artist_id=t2.artist_id;"
tracks <- dbGetQuery(con, heavy_query)
tracks1 <- tracks%>%filter(grepl('pop', artist_genres))

data1_1 <- data1%>%left_join(tracks,by='uri')%>%select(uri,valence,energy.y,Saito,Kimino,Suzuki,Yoshimura,Kokubo,Ishiyama,Miyazaki)
colnames(data1_1)<-c('uri','Valence','energy','Saito','Kimino','Suzuki','Yoshimura','Kokubo','Ishiyama','Miyazaki')
data1_2 <- data1_1%>%mutate(Q1_1=if_else(Valence>=0.5 & energy>=0.5,'1',''))%>%mutate(Q1_2=if_else(Valence<0.5 & energy>=0.5,'2',''))%>%mutate(Q1_3=if_else(Valence<0.5 & energy<0.5,'3',''))%>%mutate(Q1_4=if_else(Valence>=0.5 & energy<0.5,'4',''))%>%mutate(QUAD=paste(!!!rlang::syms(c("Q1_1", "Q1_2", "Q1_3","Q1_4")), sep=""))

data2 <-data1_2
data3 <- data2%>%mutate(flg1_1=ifelse(Saito==1,1,0))%>%mutate(flg1_2=ifelse(Saito==2,1,0))%>%mutate(flg1_3=ifelse(Saito==3,1,0))%>%mutate(flg1_4=ifelse(Saito==4,1,0))%>%mutate(flg2_1=ifelse(Kimino==1,1,0))%>%mutate(flg2_2=ifelse(Kimino==2,1,0))%>%mutate(flg2_3=ifelse(Kimino==3,1,0))%>%mutate(flg2_4=ifelse(Kimino==4,1,0))%>%mutate(flg3_1=ifelse(Suzuki==1,1,0))%>%mutate(flg3_2=ifelse(Suzuki==2,1,0))%>%mutate(flg3_3=ifelse(Suzuki==3,1,0))%>%mutate(flg3_4=ifelse(Suzuki==4,1,0))%>%mutate(flg4_1=ifelse(Yoshimura==1,1,0))%>%mutate(flg4_2=ifelse(Yoshimura==2,1,0))%>%mutate(flg4_3=ifelse(Yoshimura==3,1,0))%>%mutate(flg4_4=ifelse(Yoshimura==4,1,0))%>%mutate(flg5_1=ifelse(Kokubo==1,1,0))%>%mutate(flg5_2=ifelse(Kokubo==2,1,0))%>%mutate(flg5_3=ifelse(Kokubo==3,1,0))%>%mutate(flg5_4=ifelse(Kokubo==4,1,0))%>%mutate(flg6_1=ifelse(Ishiyama==1,1,0))%>%mutate(flg6_2=ifelse(Ishiyama==2,1,0))%>%mutate(flg6_3=ifelse(Ishiyama==3,1,0))%>%mutate(flg6_4=ifelse(Ishiyama==4,1,0))%>%mutate(flg7_1=ifelse(Miyazaki==1,1,0))%>%mutate(flg7_2=ifelse(Miyazaki==2,1,0))%>%mutate(flg7_3=ifelse(Miyazaki==3,1,0))%>%mutate(flg7_4=ifelse(Miyazaki==4,1,0))
data4 <- data3%>%mutate(count1=flg1_1+flg2_1+flg3_1+flg4_1+flg5_1+flg6_1+flg7_1)%>%mutate(count2=flg1_2+flg2_2+flg3_2+flg4_2+flg5_2+flg6_2+flg7_2)%>%mutate(count3=flg1_3+flg2_3+flg3_3+flg4_3+flg5_3+flg6_3+flg7_3)%>%mutate(count4=flg1_4+flg2_4+flg3_4+flg4_4+flg5_4+flg6_4+flg7_4)
data4_1<- data4[,44:47]
data4_2 <- as.data.frame(apply(data4_1,1,which.max))
colnames(data4_2)<- c('ans')
data4_3 <- cbind(data4,data4_2)
data4_4 <- data4_3%>%mutate(hit=if_else(ans==QUAD,1,2))%>%mutate(hit_yn_quad=paste0(QUAD,hit))

ggp0 <- ggplot(data4_4,aes(x=Valence,y=energy, size = c(5, 3)[hit], colour = hit_yn_quad))+geom_point()+scale_size_identity()
ggp0

ggsave(file = "220_Majority_190112_0.5_1.png", plot = ggp0)

#######(x-up_y-up)
data5<-data4_4%>%mutate(Q1_1=if_else(Valence>=0.6 & energy>=0.6,'1',''))%>%mutate(Q1_2=if_else(Valence<0.6 & energy>=0.6,'2',''))%>%mutate(Q1_3=if_else(Valence<0.6 & energy<0.6,'3',''))%>%mutate(Q1_4=if_else(Valence>=0.6 & energy<0.6,'4',''))%>%mutate(Q1_f=paste(!!!rlang::syms(c("Q1_1", "Q1_2", "Q1_3","Q1_4")), sep=""))
#data6 <- data5%>%mutate(flg1_1=ifelse(Q1_f==Saito,1,0))%>%mutate(flg2_1=ifelse(Q1_f==Kimino,1,0))%>%mutate(flg3_1=ifelse(Q1_f==Suzuki,1,0))%>%mutate(flg4_1=ifelse(Q1_f==Yoshimura,1,0))%>%mutate(flg5_1=ifelse(Q1_f==Kokubo,1,0))%>%mutate(flg6_1=ifelse(Q1_f==Ishiyama,1,0))%>%mutate(flg7_1=ifelse(Q1_f==Miyazaki,1,0))
data6 <- data5%>%mutate(hit1=if_else(ans==Q1_f,1,2))%>%mutate(hit_yn_quad1=paste0(Q1_f,hit1))

ggp1 <- ggplot(data6,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad1))+geom_point()+scale_size_identity()
ggp1
#ggp <- ggplot(data4,aes(x=Valence,y=energy, size = c(5, 3)[hit], colour = hit_yn_quad))+geom_point()+scale_size_identity()

ggsave(file = "220_Majority_190112_0.6_x-up_y-up.png", plot = ggp1)

####0.7
data7<-data6%>%mutate(Q2_1=if_else(Valence>=0.7 & energy>=0.7,'1',''))%>%mutate(Q2_2=if_else(Valence<0.7 & energy>=0.7,'2',''))%>%mutate(Q2_3=if_else(Valence<0.7 & energy<0.7,'3',''))%>%mutate(Q2_4=if_else(Valence>=0.7 & energy<0.7,'4',''))%>%mutate(Q2_f=paste(!!!rlang::syms(c("Q2_1", "Q2_2", "Q2_3","Q2_4")), sep=""))
#data9 <- data8%>%mutate(flg1_2=ifelse(Q2_f==Saito,1,0))%>%mutate(flg2_2=ifelse(Q2_f==Kimino,1,0))%>%mutate(flg3_2=ifelse(Q2_f==Suzuki,1,0))%>%mutate(flg4_2=ifelse(Q2_f==Yoshimura,1,0))%>%mutate(flg5_2=ifelse(Q2_f==Kokubo,1,0))%>%mutate(flg6_2=ifelse(Q2_f==Ishiyama,1,0))%>%mutate(flg7_2=ifelse(Q2_f==Miyazaki,1,0))
data8 <- data7%>%mutate(hit1=if_else(ans==Q2_f,1,2))%>%mutate(hit_yn_quad2=paste0(Q2_f,hit1))

ggp2 <- ggplot(data8,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad2))+geom_point()+scale_size_identity()
ggp2
ggsave(file = "220_Majority_190112_0.7_x-up_y-up.png", plot = ggp2)

####0.8
data9<-data8%>%mutate(Q3_1=if_else(Valence>=0.8 & energy>=0.8,'1',''))%>%mutate(Q3_2=if_else(Valence<0.8 & energy>=0.8,'2',''))%>%mutate(Q3_3=if_else(Valence<0.8 & energy<0.8,'3',''))%>%mutate(Q3_4=if_else(Valence>=0.8 & energy<0.8,'4',''))%>%mutate(Q3_f=paste(!!!rlang::syms(c("Q3_1", "Q3_2", "Q3_3","Q3_4")), sep=""))
#data12 <- data11%>%mutate(flg1_3=ifelse(Q3_f==Saito,1,0))%>%mutate(flg2_3=ifelse(Q3_f==Kimino,1,0))%>%mutate(flg3_3=ifelse(Q3_f==Suzuki,1,0))%>%mutate(flg4_3=ifelse(Q3_f==Yoshimura,1,0))%>%mutate(flg5_3=ifelse(Q3_f==Kokubo,1,0))%>%mutate(flg6_3=ifelse(Q3_f==Ishiyama,1,0))%>%mutate(flg7_3=ifelse(Q3_f==Miyazaki,1,0))
data10 <- data9%>%mutate(hit1=if_else(ans==Q3_f,1,2))%>%mutate(hit_yn_quad3=paste0(Q3_f,hit1))

ggp3 <- ggplot(data10,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad3))+geom_point()+scale_size_identity()
ggp3
ggsave(file = "220_Majority_190112_0.8_x-up_y-up.png", plot = ggp3)

####0.85
data11<-data10%>%mutate(Q4_1=if_else(Valence>=0.85 & energy>=0.85,'1',''))%>%mutate(Q4_2=if_else(Valence<0.85 & energy>=0.85,'2',''))%>%mutate(Q4_3=if_else(Valence<0.85 & energy<0.85,'3',''))%>%mutate(Q4_4=if_else(Valence>=0.85 & energy<0.85,'4',''))%>%mutate(Q4_f=paste(!!!rlang::syms(c("Q4_1", "Q4_2", "Q4_3","Q4_4")), sep=""))
#data15 <- data14%>%mutate(flg1_4=ifelse(Q4_f==Saito,1,0))%>%mutate(flg2_4=ifelse(Q4_f==Kimino,1,0))%>%mutate(flg3_4=ifelse(Q4_f==Suzuki,1,0))%>%mutate(flg4_4=ifelse(Q4_f==Yoshimura,1,0))%>%mutate(flg5_4=ifelse(Q4_f==Kokubo,1,0))%>%mutate(flg6_4=ifelse(Q4_f==Ishiyama,1,0))%>%mutate(flg7_4=ifelse(Q4_f==Miyazaki,1,0))
data12 <- data11%>%mutate(hit1=if_else(ans==Q4_f,1,2))%>%mutate(hit_yn_quad4=paste0(Q4_f,hit1))

ggp4 <- ggplot(data12,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad4))+geom_point()+scale_size_identity()
ggp4
ggsave(file = "220_Majority_190112_0.85_x-up_y-up.png", plot = ggp4)

#####count#####
d1_1<-data12%>%group_by(hit_yn_quad)%>%mutate(cnt_base=n())
d1_2 <-data12%>%group_by(hit_yn_quad1)%>%mutate(cnt06=n())
d1_3 <-data12%>%group_by(hit_yn_quad2)%>%mutate(cnt07=n())
d1_4 <-data12%>%group_by(hit_yn_quad3)%>%mutate(cnt08=n())
d1_5 <-data12%>%group_by(hit_yn_quad4)%>%mutate(cnt085=n())

d2_1 <-d1_1%>%distinct(hit_yn_quad,cnt_base,.keep_all=FALSE)%>%select(hit_yn_quad,cnt_base)
d2_2 <-d1_2%>%distinct(hit_yn_quad1,cnt06,.keep_all=FALSE)%>%select(hit_yn_quad1,cnt06)
d2_3 <-d1_3%>%distinct(hit_yn_quad2,cnt07,.keep_all=FALSE)%>%select(hit_yn_quad2,cnt07)
d2_4 <-d1_4%>%distinct(hit_yn_quad3,cnt08,.keep_all=FALSE)%>%select(hit_yn_quad3,cnt08)
d2_5 <-d1_5%>%distinct(hit_yn_quad4,cnt085,.keep_all=FALSE)%>%select(hit_yn_quad4,cnt085)

d3_1 <- d2_1%>%left_join(d2_2,by=c('hit_yn_quad'='hit_yn_quad1'))%>%left_join(d2_3,by=c('hit_yn_quad'='hit_yn_quad2'))%>%left_join(d2_4,by=c('hit_yn_quad'='hit_yn_quad3'))%>%left_join(d2_5,by=c('hit_yn_quad'='hit_yn_quad4'))%>%arrange(hit_yn_quad)

d4_1 <- d3_1%>%filter(hit_yn_quad==11|hit_yn_quad==21|hit_yn_quad==31|hit_yn_quad==41)%>%mutate(key1=stringr::str_sub(hit_yn_quad,1, 1))%>%rename(cnt_base_hit=cnt_base)%>%rename(cnt06_hit=cnt06)%>%rename(cnt07_hit=cnt07)%>%rename(cnt08_hit=cnt08)%>%rename(cnt085_hit=cnt085)
d4_1[5,2:6] <- apply(d4_1[,2:6],2,sum)
d4_1[5,1] <-'99'
d4_1[5,7] <-'5'
d4_2 <- d3_1%>%filter(hit_yn_quad==12|hit_yn_quad==22|hit_yn_quad==32|hit_yn_quad==42)%>%mutate(key2=stringr::str_sub(hit_yn_quad,1, 1))%>%rename(cnt_base_out=cnt_base)%>%rename(cnt06_out=cnt06)%>%rename(cnt07_out=cnt07)%>%rename(cnt08_out=cnt08)%>%rename(cnt085_out=cnt085)
d4_2[1,6] <-0

d4_2[5,2:6] <- apply(d4_2[,2:6],2,sum)
d4_2[5,1] <-'99'
d4_2[5,7] <-'5'

d5_1<- d4_1%>%left_join(d4_2,by=c('key1'='key2'))%>%mutate(cnt_base_all=cnt_base_hit+cnt_base_out)%>%mutate(cnt06_all=cnt06_hit+cnt06_out)%>%mutate(cnt07_all=cnt07_hit+cnt07_out)%>%mutate(cnt08_all=cnt08_hit+cnt08_out)%>%mutate(cnt085_all=cnt085_hit+cnt085_out)
d5_2 <- d5_1%>%mutate(cnt_base_rate=cnt_base_hit/cnt_base_all*100)%>%mutate(cnt06_rate=cnt06_hit/cnt06_all*100)%>%mutate(cnt07_rate=cnt07_hit/cnt07_all*100)%>%mutate(cnt08_rate=cnt08_hit/cnt08_all*100)%>%mutate(cnt085_rate=cnt085_hit/cnt085_all*100)

d6_1 <- d5_2%>%select(hit_yn_quad.x,cnt_base_rate,cnt06_rate,cnt07_rate,cnt08_rate,cnt085_rate)

d7_1 <- d6_1%>%tidyr::gather(var, value,-hit_yn_quad.x)%>%mutate(x=row_number())%>%mutate(idx=stringr::str_sub(hit_yn_quad.x,1, 1))%>%mutate(QUAD=paste0(idx,'QUAD'))
d7_1$x <- as.numeric(d7_1$x)
for (i in 1:nrow(d7_1)){
  if(d7_1[i,'x']==1){d7_1[i,'x1']=0.5}else if(d7_1[i,'x']==2){d7_1[i,'x1']=0.6}else if(d7_1[i,'x']==3){d7_1[i,'x1']=0.7}else if(d7_1[i,'x']==4){d7_1[i,'x1']=0.8}else if(d7_1[i,'x']==5){d7_1[i,'x1']=0.85}
  if(d7_1[i,'QUAD']=='9QUAD'){d7_1[i,'QUAD']='ALL'}
}

ggp5 <- ggplot(d7_1, aes(x = x1, y = value, color = QUAD)) +
  geom_line() + ylim(0,100) + ggtitle("軸の移動による正解率推移(Valence-up_Energy-up)") + xlab("Threshold") + ylab("正解率") + theme_bw(base_family = "HiraKakuProN-W3")
ggp5
ggsave(file = "220_Majority_190112_error_x-up_y-up.png", plot = ggp5)

###########2(x-down_y-up)
data5<-data4_4%>%mutate(Q1_1=if_else(Valence>=0.4 & energy>=0.6,'1',''))%>%mutate(Q1_2=if_else(Valence<0.4 & energy>=0.6,'2',''))%>%mutate(Q1_3=if_else(Valence<0.4 & energy<0.6,'3',''))%>%mutate(Q1_4=if_else(Valence>=0.4 & energy<0.6,'4',''))%>%mutate(Q1_f=paste(!!!rlang::syms(c("Q1_1", "Q1_2", "Q1_3","Q1_4")), sep=""))
#data6 <- data5%>%mutate(flg1_1=ifelse(Q1_f==Saito,1,0))%>%mutate(flg2_1=ifelse(Q1_f==Kimino,1,0))%>%mutate(flg3_1=ifelse(Q1_f==Suzuki,1,0))%>%mutate(flg4_1=ifelse(Q1_f==Yoshimura,1,0))%>%mutate(flg5_1=ifelse(Q1_f==Kokubo,1,0))%>%mutate(flg6_1=ifelse(Q1_f==Ishiyama,1,0))%>%mutate(flg7_1=ifelse(Q1_f==Miyazaki,1,0))
data6 <- data5%>%mutate(hit1=if_else(ans==Q1_f,1,2))%>%mutate(hit_yn_quad1=paste0(Q1_f,hit1))

ggp1 <- ggplot(data6,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad1))+geom_point()+scale_size_identity()
ggp1
#ggp <- ggplot(data4,aes(x=Valence,y=energy, size = c(5, 3)[hit], colour = hit_yn_quad))+geom_point()+scale_size_identity()

ggsave(file = "220_Majority_190112_0.6_x-down_y-up.png", plot = ggp1)

####0.7
data7<-data6%>%mutate(Q2_1=if_else(Valence>=0.3 & energy>=0.7,'1',''))%>%mutate(Q2_2=if_else(Valence<0.3 & energy>=0.7,'2',''))%>%mutate(Q2_3=if_else(Valence<0.3 & energy<0.7,'3',''))%>%mutate(Q2_4=if_else(Valence>=0.3 & energy<0.7,'4',''))%>%mutate(Q2_f=paste(!!!rlang::syms(c("Q2_1", "Q2_2", "Q2_3","Q2_4")), sep=""))
#data9 <- data8%>%mutate(flg1_2=ifelse(Q2_f==Saito,1,0))%>%mutate(flg2_2=ifelse(Q2_f==Kimino,1,0))%>%mutate(flg3_2=ifelse(Q2_f==Suzuki,1,0))%>%mutate(flg4_2=ifelse(Q2_f==Yoshimura,1,0))%>%mutate(flg5_2=ifelse(Q2_f==Kokubo,1,0))%>%mutate(flg6_2=ifelse(Q2_f==Ishiyama,1,0))%>%mutate(flg7_2=ifelse(Q2_f==Miyazaki,1,0))
data8 <- data7%>%mutate(hit1=if_else(ans==Q2_f,1,2))%>%mutate(hit_yn_quad2=paste0(Q2_f,hit1))

ggp2 <- ggplot(data8,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad2))+geom_point()+scale_size_identity()
ggp2
ggsave(file = "220_Majority_190112_0.7_x-down_y-up.png", plot = ggp2)

####0.8
data9<-data8%>%mutate(Q3_1=if_else(Valence>=0.2 & energy>=0.8,'1',''))%>%mutate(Q3_2=if_else(Valence<0.2 & energy>=0.8,'2',''))%>%mutate(Q3_3=if_else(Valence<0.2 & energy<0.8,'3',''))%>%mutate(Q3_4=if_else(Valence>=0.2 & energy<0.8,'4',''))%>%mutate(Q3_f=paste(!!!rlang::syms(c("Q3_1", "Q3_2", "Q3_3","Q3_4")), sep=""))
#data12 <- data11%>%mutate(flg1_3=ifelse(Q3_f==Saito,1,0))%>%mutate(flg2_3=ifelse(Q3_f==Kimino,1,0))%>%mutate(flg3_3=ifelse(Q3_f==Suzuki,1,0))%>%mutate(flg4_3=ifelse(Q3_f==Yoshimura,1,0))%>%mutate(flg5_3=ifelse(Q3_f==Kokubo,1,0))%>%mutate(flg6_3=ifelse(Q3_f==Ishiyama,1,0))%>%mutate(flg7_3=ifelse(Q3_f==Miyazaki,1,0))
data10 <- data9%>%mutate(hit1=if_else(ans==Q3_f,1,2))%>%mutate(hit_yn_quad3=paste0(Q3_f,hit1))

ggp3 <- ggplot(data10,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad3))+geom_point()+scale_size_identity()
ggp3
ggsave(file = "220_Majority_190112_0.8_x-down_y-up.png", plot = ggp3)

####0.85
data11<-data10%>%mutate(Q4_1=if_else(Valence>=0.15 & energy>=0.85,'1',''))%>%mutate(Q4_2=if_else(Valence<0.15 & energy>=0.85,'2',''))%>%mutate(Q4_3=if_else(Valence<0.15 & energy<0.85,'3',''))%>%mutate(Q4_4=if_else(Valence>=0.15 & energy<0.85,'4',''))%>%mutate(Q4_f=paste(!!!rlang::syms(c("Q4_1", "Q4_2", "Q4_3","Q4_4")), sep=""))
#data15 <- data14%>%mutate(flg1_4=ifelse(Q4_f==Saito,1,0))%>%mutate(flg2_4=ifelse(Q4_f==Kimino,1,0))%>%mutate(flg3_4=ifelse(Q4_f==Suzuki,1,0))%>%mutate(flg4_4=ifelse(Q4_f==Yoshimura,1,0))%>%mutate(flg5_4=ifelse(Q4_f==Kokubo,1,0))%>%mutate(flg6_4=ifelse(Q4_f==Ishiyama,1,0))%>%mutate(flg7_4=ifelse(Q4_f==Miyazaki,1,0))
data12 <- data11%>%mutate(hit1=if_else(ans==Q4_f,1,2))%>%mutate(hit_yn_quad4=paste0(Q4_f,hit1))

ggp4 <- ggplot(data12,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad4))+geom_point()+scale_size_identity()
ggp4
ggsave(file = "220_Majority_190112_0.85_x-down_y-up.png", plot = ggp4)

#####count#####
d1_1<-data12%>%group_by(hit_yn_quad)%>%mutate(cnt_base=n())
d1_2 <-data12%>%group_by(hit_yn_quad1)%>%mutate(cnt06=n())
d1_3 <-data12%>%group_by(hit_yn_quad2)%>%mutate(cnt07=n())
d1_4 <-data12%>%group_by(hit_yn_quad3)%>%mutate(cnt08=n())
d1_5 <-data12%>%group_by(hit_yn_quad4)%>%mutate(cnt085=n())

d2_1 <-d1_1%>%distinct(hit_yn_quad,cnt_base,.keep_all=FALSE)%>%select(hit_yn_quad,cnt_base)
d2_2 <-d1_2%>%distinct(hit_yn_quad1,cnt06,.keep_all=FALSE)%>%select(hit_yn_quad1,cnt06)
d2_3 <-d1_3%>%distinct(hit_yn_quad2,cnt07,.keep_all=FALSE)%>%select(hit_yn_quad2,cnt07)
d2_4 <-d1_4%>%distinct(hit_yn_quad3,cnt08,.keep_all=FALSE)%>%select(hit_yn_quad3,cnt08)
d2_5 <-d1_5%>%distinct(hit_yn_quad4,cnt085,.keep_all=FALSE)%>%select(hit_yn_quad4,cnt085)

d3_1 <- d2_1%>%left_join(d2_2,by=c('hit_yn_quad'='hit_yn_quad1'))%>%left_join(d2_3,by=c('hit_yn_quad'='hit_yn_quad2'))%>%left_join(d2_4,by=c('hit_yn_quad'='hit_yn_quad3'))%>%left_join(d2_5,by=c('hit_yn_quad'='hit_yn_quad4'))%>%arrange(hit_yn_quad)

d4_1 <- d3_1%>%filter(hit_yn_quad==11|hit_yn_quad==21|hit_yn_quad==31|hit_yn_quad==41)%>%mutate(key1=stringr::str_sub(hit_yn_quad,1, 1))%>%rename(cnt_base_hit=cnt_base)%>%rename(cnt06_hit=cnt06)%>%rename(cnt07_hit=cnt07)%>%rename(cnt08_hit=cnt08)%>%rename(cnt085_hit=cnt085)
d4_1[5,2:6] <- apply(d4_1[,2:6],2,sum)
d4_1[5,1] <-'99'
d4_1[5,7] <-'5'
d4_2 <- d3_1%>%filter(hit_yn_quad==12|hit_yn_quad==22|hit_yn_quad==32|hit_yn_quad==42)%>%mutate(key2=stringr::str_sub(hit_yn_quad,1, 1))%>%rename(cnt_base_out=cnt_base)%>%rename(cnt06_out=cnt06)%>%rename(cnt07_out=cnt07)%>%rename(cnt08_out=cnt08)%>%rename(cnt085_out=cnt085)
d4_2[2,5] <-0
d4_2[2,6] <-0

d4_2[5,2:6] <- apply(d4_2[,2:6],2,sum)
d4_2[5,1] <-'99'
d4_2[5,7] <-'5'

d5_1<- d4_1%>%left_join(d4_2,by=c('key1'='key2'))%>%mutate(cnt_base_all=cnt_base_hit+cnt_base_out)%>%mutate(cnt06_all=cnt06_hit+cnt06_out)%>%mutate(cnt07_all=cnt07_hit+cnt07_out)%>%mutate(cnt08_all=cnt08_hit+cnt08_out)%>%mutate(cnt085_all=cnt085_hit+cnt085_out)
d5_2 <- d5_1%>%mutate(cnt_base_rate=cnt_base_hit/cnt_base_all*100)%>%mutate(cnt06_rate=cnt06_hit/cnt06_all*100)%>%mutate(cnt07_rate=cnt07_hit/cnt07_all*100)%>%mutate(cnt08_rate=cnt08_hit/cnt08_all*100)%>%mutate(cnt085_rate=cnt085_hit/cnt085_all*100)

d6_1 <- d5_2%>%select(hit_yn_quad.x,cnt_base_rate,cnt06_rate,cnt07_rate,cnt08_rate,cnt085_rate)

d7_1 <- d6_1%>%tidyr::gather(var, value,-hit_yn_quad.x)%>%mutate(x=row_number())%>%mutate(idx=stringr::str_sub(hit_yn_quad.x,1, 1))%>%mutate(QUAD=paste0(idx,'QUAD'))
d7_1$x <- as.numeric(d7_1$x)
for (i in 1:nrow(d7_1)){
  if(d7_1[i,'x']==1){d7_1[i,'x1']=0.5}else if(d7_1[i,'x']==2){d7_1[i,'x1']=0.6}else if(d7_1[i,'x']==3){d7_1[i,'x1']=0.7}else if(d7_1[i,'x']==4){d7_1[i,'x1']=0.8}else if(d7_1[i,'x']==5){d7_1[i,'x1']=0.85}
  if(d7_1[i,'QUAD']=='9QUAD'){d7_1[i,'QUAD']='ALL'}
}

ggp5 <- ggplot(d7_1, aes(x = x1, y = value, color = QUAD)) +
  geom_line() + ylim(0,100) + ggtitle("軸の移動による正解率推移(Valence-down_Energy-up)") + xlab("Threshold") + ylab("正解率") + theme_bw(base_family = "HiraKakuProN-W3")
ggp5
ggsave(file = "220_Majority_190112_error_x-down_y-up.png", plot = ggp5)

###########3(x-down_y-down)
data5<-data4_4%>%mutate(Q1_1=if_else(Valence>=0.4 & energy>=0.4,'1',''))%>%mutate(Q1_2=if_else(Valence<0.4 & energy>=0.4,'2',''))%>%mutate(Q1_3=if_else(Valence<0.4 & energy<0.4,'3',''))%>%mutate(Q1_4=if_else(Valence>=0.4 & energy<0.4,'4',''))%>%mutate(Q1_f=paste(!!!rlang::syms(c("Q1_1", "Q1_2", "Q1_3","Q1_4")), sep=""))
#data6 <- data5%>%mutate(flg1_1=ifelse(Q1_f==Saito,1,0))%>%mutate(flg2_1=ifelse(Q1_f==Kimino,1,0))%>%mutate(flg3_1=ifelse(Q1_f==Suzuki,1,0))%>%mutate(flg4_1=ifelse(Q1_f==Yoshimura,1,0))%>%mutate(flg5_1=ifelse(Q1_f==Kokubo,1,0))%>%mutate(flg6_1=ifelse(Q1_f==Ishiyama,1,0))%>%mutate(flg7_1=ifelse(Q1_f==Miyazaki,1,0))
data6 <- data5%>%mutate(hit1=if_else(ans==Q1_f,1,2))%>%mutate(hit_yn_quad1=paste0(Q1_f,hit1))

ggp1 <- ggplot(data6,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad1))+geom_point()+scale_size_identity()
ggp1
#ggp <- ggplot(data4,aes(x=Valence,y=energy, size = c(5, 3)[hit], colour = hit_yn_quad))+geom_point()+scale_size_identity()

ggsave(file = "220_Majority_190112_0.6_x-down_y-down.png", plot = ggp1)

####0.7
data7<-data6%>%mutate(Q2_1=if_else(Valence>=0.3 & energy>=0.3,'1',''))%>%mutate(Q2_2=if_else(Valence<0.3 & energy>=0.3,'2',''))%>%mutate(Q2_3=if_else(Valence<0.3 & energy<0.3,'3',''))%>%mutate(Q2_4=if_else(Valence>=0.3 & energy<0.3,'4',''))%>%mutate(Q2_f=paste(!!!rlang::syms(c("Q2_1", "Q2_2", "Q2_3","Q2_4")), sep=""))
#data9 <- data8%>%mutate(flg1_2=ifelse(Q2_f==Saito,1,0))%>%mutate(flg2_2=ifelse(Q2_f==Kimino,1,0))%>%mutate(flg3_2=ifelse(Q2_f==Suzuki,1,0))%>%mutate(flg4_2=ifelse(Q2_f==Yoshimura,1,0))%>%mutate(flg5_2=ifelse(Q2_f==Kokubo,1,0))%>%mutate(flg6_2=ifelse(Q2_f==Ishiyama,1,0))%>%mutate(flg7_2=ifelse(Q2_f==Miyazaki,1,0))
data8 <- data7%>%mutate(hit1=if_else(ans==Q2_f,1,2))%>%mutate(hit_yn_quad2=paste0(Q2_f,hit1))

ggp2 <- ggplot(data8,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad2))+geom_point()+scale_size_identity()
ggp2
ggsave(file = "220_Majority_190112_0.7_x-down_y-down.png", plot = ggp2)

####0.8
data9<-data8%>%mutate(Q3_1=if_else(Valence>=0.2 & energy>=0.2,'1',''))%>%mutate(Q3_2=if_else(Valence<0.2 & energy>=0.2,'2',''))%>%mutate(Q3_3=if_else(Valence<0.2 & energy<0.2,'3',''))%>%mutate(Q3_4=if_else(Valence>=0.2 & energy<0.2,'4',''))%>%mutate(Q3_f=paste(!!!rlang::syms(c("Q3_1", "Q3_2", "Q3_3","Q3_4")), sep=""))
#data12 <- data11%>%mutate(flg1_3=ifelse(Q3_f==Saito,1,0))%>%mutate(flg2_3=ifelse(Q3_f==Kimino,1,0))%>%mutate(flg3_3=ifelse(Q3_f==Suzuki,1,0))%>%mutate(flg4_3=ifelse(Q3_f==Yoshimura,1,0))%>%mutate(flg5_3=ifelse(Q3_f==Kokubo,1,0))%>%mutate(flg6_3=ifelse(Q3_f==Ishiyama,1,0))%>%mutate(flg7_3=ifelse(Q3_f==Miyazaki,1,0))
data10 <- data9%>%mutate(hit1=if_else(ans==Q3_f,1,2))%>%mutate(hit_yn_quad3=paste0(Q3_f,hit1))

ggp3 <- ggplot(data10,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad3))+geom_point()+scale_size_identity()
ggp3
ggsave(file = "220_Majority_190112_0.8_x-down_y-down.png", plot = ggp3)

####0.85
data11<-data10%>%mutate(Q4_1=if_else(Valence>=0.15 & energy>=0.15,'1',''))%>%mutate(Q4_2=if_else(Valence<0.15 & energy>=0.15,'2',''))%>%mutate(Q4_3=if_else(Valence<0.15 & energy<0.15,'3',''))%>%mutate(Q4_4=if_else(Valence>=0.15 & energy<0.15,'4',''))%>%mutate(Q4_f=paste(!!!rlang::syms(c("Q4_1", "Q4_2", "Q4_3","Q4_4")), sep=""))
#data15 <- data14%>%mutate(flg1_4=ifelse(Q4_f==Saito,1,0))%>%mutate(flg2_4=ifelse(Q4_f==Kimino,1,0))%>%mutate(flg3_4=ifelse(Q4_f==Suzuki,1,0))%>%mutate(flg4_4=ifelse(Q4_f==Yoshimura,1,0))%>%mutate(flg5_4=ifelse(Q4_f==Kokubo,1,0))%>%mutate(flg6_4=ifelse(Q4_f==Ishiyama,1,0))%>%mutate(flg7_4=ifelse(Q4_f==Miyazaki,1,0))
data12 <- data11%>%mutate(hit1=if_else(ans==Q4_f,1,2))%>%mutate(hit_yn_quad4=paste0(Q4_f,hit1))

ggp4 <- ggplot(data12,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad4))+geom_point()+scale_size_identity()
ggp4
ggsave(file = "220_Majority_190112_0.85_x-down_y-down.png", plot = ggp4)

#####count#####
d1_1<-data12%>%group_by(hit_yn_quad)%>%mutate(cnt_base=n())
d1_2 <-data12%>%group_by(hit_yn_quad1)%>%mutate(cnt06=n())
d1_3 <-data12%>%group_by(hit_yn_quad2)%>%mutate(cnt07=n())
d1_4 <-data12%>%group_by(hit_yn_quad3)%>%mutate(cnt08=n())
d1_5 <-data12%>%group_by(hit_yn_quad4)%>%mutate(cnt085=n())

d2_1 <-d1_1%>%distinct(hit_yn_quad,cnt_base,.keep_all=FALSE)%>%select(hit_yn_quad,cnt_base)
d2_2 <-d1_2%>%distinct(hit_yn_quad1,cnt06,.keep_all=FALSE)%>%select(hit_yn_quad1,cnt06)
d2_3 <-d1_3%>%distinct(hit_yn_quad2,cnt07,.keep_all=FALSE)%>%select(hit_yn_quad2,cnt07)
d2_4 <-d1_4%>%distinct(hit_yn_quad3,cnt08,.keep_all=FALSE)%>%select(hit_yn_quad3,cnt08)
d2_5 <-d1_5%>%distinct(hit_yn_quad4,cnt085,.keep_all=FALSE)%>%select(hit_yn_quad4,cnt085)

d3_1 <- d2_1%>%left_join(d2_2,by=c('hit_yn_quad'='hit_yn_quad1'))%>%left_join(d2_3,by=c('hit_yn_quad'='hit_yn_quad2'))%>%left_join(d2_4,by=c('hit_yn_quad'='hit_yn_quad3'))%>%left_join(d2_5,by=c('hit_yn_quad'='hit_yn_quad4'))%>%arrange(hit_yn_quad)
d3_1[6,5] <-0
d3_1[6,6] <-0


d4_1 <- d3_1%>%filter(hit_yn_quad==11|hit_yn_quad==21|hit_yn_quad==31|hit_yn_quad==41)%>%mutate(key1=stringr::str_sub(hit_yn_quad,1, 1))%>%rename(cnt_base_hit=cnt_base)%>%rename(cnt06_hit=cnt06)%>%rename(cnt07_hit=cnt07)%>%rename(cnt08_hit=cnt08)%>%rename(cnt085_hit=cnt085)
d4_1[5,2:6] <- apply(d4_1[,2:6],2,sum)
d4_1[5,1] <-'99'
d4_1[5,7] <-'5'
d4_2 <- d3_1%>%filter(hit_yn_quad==12|hit_yn_quad==22|hit_yn_quad==32|hit_yn_quad==42)%>%mutate(key2=stringr::str_sub(hit_yn_quad,1, 1))%>%rename(cnt_base_out=cnt_base)%>%rename(cnt06_out=cnt06)%>%rename(cnt07_out=cnt07)%>%rename(cnt08_out=cnt08)%>%rename(cnt085_out=cnt085)
#d4_2[2,5] <-0
#d4_2[2,6] <-0
d4_2[5,2:6] <- apply(d4_2[,2:6],2,sum)
d4_2[5,1] <-'99'
d4_2[5,7] <-'5'

d5_1<- d4_1%>%left_join(d4_2,by=c('key1'='key2'))%>%mutate(cnt_base_all=cnt_base_hit+cnt_base_out)%>%mutate(cnt06_all=cnt06_hit+cnt06_out)%>%mutate(cnt07_all=cnt07_hit+cnt07_out)%>%mutate(cnt08_all=cnt08_hit+cnt08_out)%>%mutate(cnt085_all=cnt085_hit+cnt085_out)
d5_2 <- d5_1%>%mutate(cnt_base_rate=cnt_base_hit/cnt_base_all*100)%>%mutate(cnt06_rate=cnt06_hit/cnt06_all*100)%>%mutate(cnt07_rate=cnt07_hit/cnt07_all*100)%>%mutate(cnt08_rate=cnt08_hit/cnt08_all*100)%>%mutate(cnt085_rate=cnt085_hit/cnt085_all*100)

d6_1 <- d5_2%>%select(hit_yn_quad.x,cnt_base_rate,cnt06_rate,cnt07_rate,cnt08_rate,cnt085_rate)

d7_1 <- d6_1%>%tidyr::gather(var, value,-hit_yn_quad.x)%>%mutate(x=row_number())%>%mutate(idx=stringr::str_sub(hit_yn_quad.x,1, 1))%>%mutate(QUAD=paste0(idx,'QUAD'))
d7_1$x <- as.numeric(d7_1$x)
for (i in 1:nrow(d7_1)){
  if(d7_1[i,'x']==1){d7_1[i,'x1']=0.5}else if(d7_1[i,'x']==2){d7_1[i,'x1']=0.6}else if(d7_1[i,'x']==3){d7_1[i,'x1']=0.7}else if(d7_1[i,'x']==4){d7_1[i,'x1']=0.8}else if(d7_1[i,'x']==5){d7_1[i,'x1']=0.85}
  if(d7_1[i,'QUAD']=='9QUAD'){d7_1[i,'QUAD']='ALL'}
}

ggp5 <- ggplot(d7_1, aes(x = x1, y = value, color = QUAD)) +
  geom_line() + ylim(0,100) + ggtitle("軸の移動による正解率推移(Valence-down_Energy-down)") + xlab("Threshold") + ylab("正解率") + theme_bw(base_family = "HiraKakuProN-W3")
ggp5
ggsave(file = "220_Majority_190112_error_x-down_y-down.png", plot = ggp5)

###########4(x-up_y-down)
data5<-data4_4%>%mutate(Q1_1=if_else(Valence>=0.6 & energy>=0.4,'1',''))%>%mutate(Q1_2=if_else(Valence<0.6 & energy>=0.4,'2',''))%>%mutate(Q1_3=if_else(Valence<0.6 & energy<0.4,'3',''))%>%mutate(Q1_4=if_else(Valence>=0.6 & energy<0.4,'4',''))%>%mutate(Q1_f=paste(!!!rlang::syms(c("Q1_1", "Q1_2", "Q1_3","Q1_4")), sep=""))
#data6 <- data5%>%mutate(flg1_1=ifelse(Q1_f==Saito,1,0))%>%mutate(flg2_1=ifelse(Q1_f==Kimino,1,0))%>%mutate(flg3_1=ifelse(Q1_f==Suzuki,1,0))%>%mutate(flg4_1=ifelse(Q1_f==Yoshimura,1,0))%>%mutate(flg5_1=ifelse(Q1_f==Kokubo,1,0))%>%mutate(flg6_1=ifelse(Q1_f==Ishiyama,1,0))%>%mutate(flg7_1=ifelse(Q1_f==Miyazaki,1,0))
data6 <- data5%>%mutate(hit1=if_else(ans==Q1_f,1,2))%>%mutate(hit_yn_quad1=paste0(Q1_f,hit1))

ggp1 <- ggplot(data6,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad1))+geom_point()+scale_size_identity()
ggp1
#ggp <- ggplot(data4,aes(x=Valence,y=energy, size = c(5, 3)[hit], colour = hit_yn_quad))+geom_point()+scale_size_identity()

ggsave(file = "220_Majority_190112_0.6_x-up_y-down.png", plot = ggp1)

####0.7
data7<-data6%>%mutate(Q2_1=if_else(Valence>=0.7 & energy>=0.3,'1',''))%>%mutate(Q2_2=if_else(Valence<0.7 & energy>=0.3,'2',''))%>%mutate(Q2_3=if_else(Valence<0.7 & energy<0.3,'3',''))%>%mutate(Q2_4=if_else(Valence>=0.7 & energy<0.3,'4',''))%>%mutate(Q2_f=paste(!!!rlang::syms(c("Q2_1", "Q2_2", "Q2_3","Q2_4")), sep=""))
#data9 <- data8%>%mutate(flg1_2=ifelse(Q2_f==Saito,1,0))%>%mutate(flg2_2=ifelse(Q2_f==Kimino,1,0))%>%mutate(flg3_2=ifelse(Q2_f==Suzuki,1,0))%>%mutate(flg4_2=ifelse(Q2_f==Yoshimura,1,0))%>%mutate(flg5_2=ifelse(Q2_f==Kokubo,1,0))%>%mutate(flg6_2=ifelse(Q2_f==Ishiyama,1,0))%>%mutate(flg7_2=ifelse(Q2_f==Miyazaki,1,0))
data8 <- data7%>%mutate(hit1=if_else(ans==Q2_f,1,2))%>%mutate(hit_yn_quad2=paste0(Q2_f,hit1))

ggp2 <- ggplot(data8,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad2))+geom_point()+scale_size_identity()
ggp2
ggsave(file = "220_Majority_190112_0.7_x-up_y_down.png", plot = ggp2)

####0.8
data9<-data8%>%mutate(Q3_1=if_else(Valence>=0.8 & energy>=0.2,'1',''))%>%mutate(Q3_2=if_else(Valence<0.8 & energy>=0.2,'2',''))%>%mutate(Q3_3=if_else(Valence<0.8 & energy<0.2,'3',''))%>%mutate(Q3_4=if_else(Valence>=0.8 & energy<0.2,'4',''))%>%mutate(Q3_f=paste(!!!rlang::syms(c("Q3_1", "Q3_2", "Q3_3","Q3_4")), sep=""))
#data12 <- data11%>%mutate(flg1_3=ifelse(Q3_f==Saito,1,0))%>%mutate(flg2_3=ifelse(Q3_f==Kimino,1,0))%>%mutate(flg3_3=ifelse(Q3_f==Suzuki,1,0))%>%mutate(flg4_3=ifelse(Q3_f==Yoshimura,1,0))%>%mutate(flg5_3=ifelse(Q3_f==Kokubo,1,0))%>%mutate(flg6_3=ifelse(Q3_f==Ishiyama,1,0))%>%mutate(flg7_3=ifelse(Q3_f==Miyazaki,1,0))
data10 <- data9%>%mutate(hit1=if_else(ans==Q3_f,1,2))%>%mutate(hit_yn_quad3=paste0(Q3_f,hit1))

ggp3 <- ggplot(data10,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad3))+geom_point()+scale_size_identity()
ggp3
ggsave(file = "220_Majority_190112_0.8_x-up_y-down.png", plot = ggp3)

####0.85
data11<-data10%>%mutate(Q4_1=if_else(Valence>=0.85 & energy>=0.15,'1',''))%>%mutate(Q4_2=if_else(Valence<0.85 & energy>=0.15,'2',''))%>%mutate(Q4_3=if_else(Valence<0.85 & energy<0.15,'3',''))%>%mutate(Q4_4=if_else(Valence>=0.85 & energy<0.15,'4',''))%>%mutate(Q4_f=paste(!!!rlang::syms(c("Q4_1", "Q4_2", "Q4_3","Q4_4")), sep=""))
#data15 <- data14%>%mutate(flg1_4=ifelse(Q4_f==Saito,1,0))%>%mutate(flg2_4=ifelse(Q4_f==Kimino,1,0))%>%mutate(flg3_4=ifelse(Q4_f==Suzuki,1,0))%>%mutate(flg4_4=ifelse(Q4_f==Yoshimura,1,0))%>%mutate(flg5_4=ifelse(Q4_f==Kokubo,1,0))%>%mutate(flg6_4=ifelse(Q4_f==Ishiyama,1,0))%>%mutate(flg7_4=ifelse(Q4_f==Miyazaki,1,0))
data12 <- data11%>%mutate(hit1=if_else(ans==Q4_f,1,2))%>%mutate(hit_yn_quad4=paste0(Q4_f,hit1))

ggp4 <- ggplot(data12,aes(x=Valence,y=energy, size = c(5, 3)[hit1], colour = hit_yn_quad4))+geom_point()+scale_size_identity()
ggp4
ggsave(file = "220_Majority_190112_0.85_x-up_y-down.png", plot = ggp4)

#####count#####
d1_1<-data12%>%group_by(hit_yn_quad)%>%mutate(cnt_base=n())
d1_2 <-data12%>%group_by(hit_yn_quad1)%>%mutate(cnt06=n())
d1_3 <-data12%>%group_by(hit_yn_quad2)%>%mutate(cnt07=n())
d1_4 <-data12%>%group_by(hit_yn_quad3)%>%mutate(cnt08=n())
d1_5 <-data12%>%group_by(hit_yn_quad4)%>%mutate(cnt085=n())

d2_1 <-d1_1%>%distinct(hit_yn_quad,cnt_base,.keep_all=FALSE)%>%select(hit_yn_quad,cnt_base)
d2_2 <-d1_2%>%distinct(hit_yn_quad1,cnt06,.keep_all=FALSE)%>%select(hit_yn_quad1,cnt06)
d2_3 <-d1_3%>%distinct(hit_yn_quad2,cnt07,.keep_all=FALSE)%>%select(hit_yn_quad2,cnt07)
d2_4 <-d1_4%>%distinct(hit_yn_quad3,cnt08,.keep_all=FALSE)%>%select(hit_yn_quad3,cnt08)
d2_5 <-d1_5%>%distinct(hit_yn_quad4,cnt085,.keep_all=FALSE)%>%select(hit_yn_quad4,cnt085)

d3_1 <- d2_1%>%left_join(d2_2,by=c('hit_yn_quad'='hit_yn_quad1'))%>%left_join(d2_3,by=c('hit_yn_quad'='hit_yn_quad2'))%>%left_join(d2_4,by=c('hit_yn_quad'='hit_yn_quad3'))%>%left_join(d2_5,by=c('hit_yn_quad'='hit_yn_quad4'))%>%arrange(hit_yn_quad)
d3_1[7,6] <-0
d3_1[8,6] <-0


d4_1 <- d3_1%>%filter(hit_yn_quad==11|hit_yn_quad==21|hit_yn_quad==31|hit_yn_quad==41)%>%mutate(key1=stringr::str_sub(hit_yn_quad,1, 1))%>%rename(cnt_base_hit=cnt_base)%>%rename(cnt06_hit=cnt06)%>%rename(cnt07_hit=cnt07)%>%rename(cnt08_hit=cnt08)%>%rename(cnt085_hit=cnt085)
d4_1[5,2:6] <- apply(d4_1[,2:6],2,sum)
d4_1[5,1] <-'99'
d4_1[5,7] <-'5'
d4_2 <- d3_1%>%filter(hit_yn_quad==12|hit_yn_quad==22|hit_yn_quad==32|hit_yn_quad==42)%>%mutate(key2=stringr::str_sub(hit_yn_quad,1, 1))%>%rename(cnt_base_out=cnt_base)%>%rename(cnt06_out=cnt06)%>%rename(cnt07_out=cnt07)%>%rename(cnt08_out=cnt08)%>%rename(cnt085_out=cnt085)
#d4_2[7,5] <-0
#d4_2[8,5] <-0
d4_2[5,2:6] <- apply(d4_2[,2:6],2,sum)
d4_2[5,1] <-'99'
d4_2[5,7] <-'5'

d5_1<- d4_1%>%left_join(d4_2,by=c('key1'='key2'))%>%mutate(cnt_base_all=cnt_base_hit+cnt_base_out)%>%mutate(cnt06_all=cnt06_hit+cnt06_out)%>%mutate(cnt07_all=cnt07_hit+cnt07_out)%>%mutate(cnt08_all=cnt08_hit+cnt08_out)%>%mutate(cnt085_all=cnt085_hit+cnt085_out)
d5_2 <- d5_1%>%mutate(cnt_base_rate=cnt_base_hit/cnt_base_all*100)%>%mutate(cnt06_rate=cnt06_hit/cnt06_all*100)%>%mutate(cnt07_rate=cnt07_hit/cnt07_all*100)%>%mutate(cnt08_rate=cnt08_hit/cnt08_all*100)%>%mutate(cnt085_rate=cnt085_hit/cnt085_all*100)

d6_1 <- d5_2%>%select(hit_yn_quad.x,cnt_base_rate,cnt06_rate,cnt07_rate,cnt08_rate,cnt085_rate)

d7_1 <- d6_1%>%tidyr::gather(var, value,-hit_yn_quad.x)%>%mutate(x=row_number())%>%mutate(idx=stringr::str_sub(hit_yn_quad.x,1, 1))%>%mutate(QUAD=paste0(idx,'QUAD'))
d7_1$x <- as.numeric(d7_1$x)
for (i in 1:nrow(d7_1)){
  if(d7_1[i,'x']==1){d7_1[i,'x1']=0.5}else if(d7_1[i,'x']==2){d7_1[i,'x1']=0.6}else if(d7_1[i,'x']==3){d7_1[i,'x1']=0.7}else if(d7_1[i,'x']==4){d7_1[i,'x1']=0.8}else if(d7_1[i,'x']==5){d7_1[i,'x1']=0.85}
  if(d7_1[i,'QUAD']=='9QUAD'){d7_1[i,'QUAD']='ALL'}
}

ggp5 <- ggplot(d7_1, aes(x = x1, y = value, color = QUAD)) +
  geom_line() + ylim(0,100) + ggtitle("軸の移動による正解率推移(Valence-up_Energy-down)") + xlab("Threshold") + ylab("正解率") + theme_bw(base_family = "HiraKakuProN-W3")
ggp5
ggsave(file = "220_Majority_190112_error_x-up_y-down.png", plot = ggp5)


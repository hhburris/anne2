DT<-mm_heatherb_may18_2018
hist(DT$re_AsM2T)
DT$lgAST2<-log(DT$re_AsM2T)
hist(DT$lgAST2)
plot(DT$lgAST2,DT$Fenton_Z_score00)
m1<-lm(DT$Fenton_Z_score00~DT$lgAST2)
m1
summary(m1)
hist(DT$re_ZnM2T)
plot(DT$re_ZnM2T, DT$Fenton_Z_score00)
m2<-lm(DT$Fenton_Z_score00~DT$re_ZnM2T)
m2
summary(m2)
resid(m2)
m2a<-gam(DT$Fenton_Z_score00~s(DT$re_ZnM2T))
m2a         
summary(m2a)
plot(m2a)



sex1<-(DT[DT$sexo_h00=='1',])
sex2<-(DT[DT$sexo_h00=='2',])

#

table (DT$sexo_h00)

table (sex1$sexo_h00)
table (sex2$sexo_h00)

m1a2<-gam(sex1$Fenton_Z_score00~s(sex1$lgAST2))
summary (m1a2)
plot (m1a2)

m1a3<-gam(sex2$Fenton_Z_score00~s(sex2$lgAST2))
summary (m1a3)
plot (m1a3)

m1a<-gam(DT$Fenton_Z_score00~s(DT$lgAST2))
m1a
summary(m1a)
plot(m1a)

#?write.csv
#write.csv(DT, file ="DT.CSV")

m4<-gam(DT$lgAST2~s(DT$mother_pre_bmi))
summary (m4)
plot(m4)

m4<-gam(DT$re_AsC00~s(DT$re_PbC00))
summary (m4)
plot(m4)

m4<-gam(DT$re_AsM2T~s(DT$re_PbM2T))
summary (m4)
plot(m4)

#zinc and gestational age
m5<-gam(DT$gestage_comb00~s(DT$re_ZnM2T))
summary (m5)

m5a<-gam(sex1$gestage_comb00~s(sex1$re_ZnM2T))
summary (m5a)
plot (m5a)

m5b<-gam(sex2$gestage_comb00~s(sex2$re_ZnM2T))
summary (m5b)
plot (m5b)

#Fenton models
m5b<-gam(DT$Fenton_Z_score00~s(sex2$re_ZnM2T))
summary (m5b)
plot (m5b)


## correlation coeeficients
names(anne5)

tp3<-anne5[,c("re_AsM2T", "re_AsM00", "re_AsC00")]

x<-tp3[1:3]

Ccor1<-cor(x, y = NULL, use = "pairwise.complete.obs",
            method = c("pearson")) #"spearman"))
Ccor1

Ccor2<-round(Ccor1, digits = 2)
Ccor2
write.csv(Ccor2, "asCor.csv")
getwd()


Ccor3<-cor(x, y = NULL, use = "pairwise.complete.obs",
           method = c("spearman"))
Ccor3

Ccor4<-round(Ccor3, digits = 2)
Ccor4
write.csv(Ccor4, "asCor1.csv")
getwd()
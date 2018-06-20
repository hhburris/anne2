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

?write.csv
write.csv(DT, file ="DT.CSV")

# DID THIS GO? NO STILL NO?mm  now?
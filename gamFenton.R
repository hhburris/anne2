anne3 <- read_sas("U:/PROGRESS/anne3.sas7bdat", 
                  +     NULL)
# Deleted value for the person with re_AS00 >10
lm1<-lm(Fenton_Z_score00~logASM00, data = anne3)
summary(lm1)
#load MGCV package
gam1<-gam(Fenton_Z_score00~s(logASM00)+s(mother_age2T)+s(mother_pre_bmi)+midses 
          +highses +v682T +sexo_h00+smoke_inside2T , data = anne3)
summary (gam1)
plot (gam1)


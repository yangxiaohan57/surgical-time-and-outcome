##### Libraries #####
library(ggplot2)
library(rms) # vif
library(broom) #confusion matrix

##### Read in the dataset #####
load("~/Desktop/702/Final/surgery_timing.Rdata")
surgery <- stata_data

##### factorizing #####
surgery$ahrq_ccs <- factor(surgery$ahrq_ccs)
surgery$gender <- factor(surgery$gender, levels=c('M','F'))
surgery$race <- factor(surgery$race, levels=c('Caucasian','African American','Other'))
surgery$asa_status <- factor(surgery$asa_status, levels=c('I-II','III','IV-VI'), labels=c(1,2,3))
surgery$baseline_cancer <- factor(surgery$baseline_cancer)
surgery$baseline_cvd <- factor(surgery$baseline_cvd)
surgery$baseline_dementia <- factor(surgery$baseline_dementia)
surgery$baseline_diabetes <- factor(surgery$baseline_diabetes)
surgery$baseline_digestive <- factor(surgery$baseline_digestive)
surgery$baseline_osteoart <- factor(surgery$baseline_osteoart)
surgery$baseline_psych <- factor(surgery$baseline_psych)
surgery$baseline_pulmonary <- factor(surgery$baseline_pulmonary)
surgery$baseline_charlson <- factor(surgery$baseline_charlson)
surgery$dow <- factor(surgery$dow, levels=c('Mon','Tue','Wed','Thu','Fri'), labels=c(1,2,3,4,5))
surgery$month <- factor(surgery$month, levels=c('Jan','Feb','Mar','Apr','May',
                                                'Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                        labels=c(1,2,3,4,5,6,7,8,9,10,11,12))
surgery$moonphase <- factor(surgery$moonphase, 
                            levels=c('New Moon','First Quarter','Full Moon','Last Quarter'),
                            labels=c(1,2,3,4))
surgery$mort30 <- factor(surgery$mort30)
surgery$complication <- factor(surgery$complication)



##### Imputation #####

# Columns with missing values: gender(categorical 3), race(categorical 480), 
                            #  asa_status(categorical 8), bmi(continuous 3290)

# run entire thing in mice, drop asa_status, drop missing in gender, 
# run mice on race bmi(exclude not useful column for BMI, rerun on subset)

surgery_impu <- mice(surgery,m=10,defaultMethod=c("pmm","logreg","polyreg","polr"),print=F)
simpu <- complete(surgery_impu,7)
    # BMI density plot looks good, categorical var distribution is generally the same


##### EDA #####

# Consider dropping ccsMort30Rate and ccsComplicationRate, because they are highly correlated with ahrq_ccs

# For numerical var 
ggplot(surgery,aes(x=age, y=mort30, fill=mort30)) + geom_boxplot() + coord_flip() + 
labs(title="Age vs 30 days mortality",x="age",y="mortality") + theme_classic()  +
scale_y_discrete() + scale_fill_brewer(palette="Blues")
        # distribution of age do vary between dead and not dead

ggplot(surgery,aes(x=bmi, y=mort30, fill=mort30)) + geom_boxplot() + coord_flip() + 
  labs(title="Bmi vs 30 days mortality",x="bmi",y="mortality") + theme_classic()  +
  scale_y_discrete() + scale_fill_brewer(palette="Blues")
        # not much different in BMI for dead and non-dead

ggplot(surgery,aes(x=mortality_rsi, y=mort30, fill=mort30)) + geom_boxplot() + coord_flip() + 
  labs(title="mortality_rsi vs 30 days mortality",x="mortality_rsi",y="mortality") + theme_classic()  +
  scale_y_discrete() + scale_fill_brewer(palette="Blues")
        # dead people have higher risk value

ggplot(surgery,aes(x=complication_rsi, y=mort30, fill=mort30)) + geom_boxplot() + coord_flip() + 
  labs(title="complication_rsi vs 30 days mortality",x="complication_rsi",y="mortality") + theme_classic()+
  scale_y_discrete() + scale_fill_brewer(palette="Blues")
        # dead people have higher complication risk value

ggplot(surgery,aes(x=hour, y=mort30, fill=mort30)) + geom_boxplot() + coord_flip() + 
  labs(title="hour vs 30 days mortality",x="hour",y="mortality") + theme_classic()+
  scale_y_discrete() + scale_fill_brewer(palette="Blues")
        # dead people are operated at later hours


# For Categorical var

apply(table(surgery[,c("mort30","gender")])/sum(table(surgery[,c("mort30","gender")])),
      2,function(x) x/sum(x)) 
chisq.test(table(simpu[,c("mort30","gender")]), simulate.p.value=TRUE) # *


apply(table(surgery[,c("mort30","race")])/sum(table(surgery[,c("mort30","race")])),
      2,function(x) x/sum(x)) 
chisq.test(table(simpu[,c("mort30","race")]), simulate.p.value=TRUE)


apply(table(surgery[,c("mort30","asa_status")])/sum(table(surgery[,c("mort30","asa_status")])),
      2,function(x) x/sum(x)) 
chisq.test(table(simpu[,c("mort30","asa_status")]), simulate.p.value=TRUE) # *


apply(table(surgery[,c("mort30","baseline_cancer")])/sum(table(surgery[,c("mort30","baseline_cancer")])),
      2,function(x) x/sum(x)) 
chisq.test(table(simpu[,c("mort30","baseline_cancer")]), simulate.p.value=TRUE) # *


apply(table(surgery[,c("mort30","baseline_cvd")])/sum(table(surgery[,c("mort30","baseline_cvd")])),
      2,function(x) x/sum(x)) 
chisq.test(table(simpu[,c("mort30","baseline_cvd")]), simulate.p.value=TRUE) # *


apply(table(surgery[,c("mort30","baseline_dementia")])/sum(table(surgery[,c("mort30","baseline_dementia")])),
      2,function(x) x/sum(x)) # seems highly correlated but might because of age
chisq.test(table(simpu[,c("mort30","baseline_dementia")]), simulate.p.value=TRUE)


apply(table(surgery[,c("mort30","baseline_diabetes")])/sum(table(surgery[,c("mort30","baseline_diabetes")])),
      2,function(x) x/sum(x)) 
chisq.test(table(simpu[,c("mort30","baseline_diabetes")]), simulate.p.value=TRUE)


apply(table(surgery[,c("mort30","baseline_digestive")])/sum(table(surgery[,c("mort30","baseline_digestive")])),
      2,function(x) x/sum(x)) 
chisq.test(table(simpu[,c("mort30","baseline_digestive")]), simulate.p.value=TRUE)


apply(table(surgery[,c("mort30","baseline_osteoart")])/sum(table(surgery[,c("mort30","baseline_osteoart")])),
      2,function(x) x/sum(x)) # seems highly correlated but might because of age
chisq.test(table(simpu[,c("mort30","baseline_osteoart")]), simulate.p.value=TRUE)


apply(table(surgery[,c("mort30","baseline_psych")])/sum(table(surgery[,c("mort30","baseline_psych")])),
      2,function(x) x/sum(x)) # associated
chisq.test(table(simpu[,c("mort30","baseline_psych")]), simulate.p.value=TRUE) # *


apply(table(surgery[,c("mort30","baseline_pulmonary")])/sum(table(surgery[,c("mort30","baseline_pulmonary")])),
      2,function(x) x/sum(x)) #associated
chisq.test(table(simpu[,c("mort30","baseline_pulmonary")]), simulate.p.value=TRUE) #*


apply(table(surgery[,c("mort30","baseline_charlson")])/sum(table(surgery[,c("mort30","baseline_charlson")])),
      2,function(x) x/sum(x)) # associated
chisq.test(table(simpu[,c("mort30","baseline_charlson")]), simulate.p.value=TRUE) #*


apply(table(surgery[,c("mort30","baseline_charlson")])/sum(table(surgery[,c("mort30","baseline_charlson")])),
      2,function(x) x/sum(x))
chisq.test(table(simpu[,c("mort30","baseline_dementia")]), simulate.p.value=TRUE)


apply(table(surgery[,c("mort30","dow")])/sum(table(surgery[,c("mort30","dow")])),2,function(x) x/sum(x))
      # wed, fri较高 其他差不多
chisq.test(table(simpu[,c("mort30","dow")]), simulate.p.value=TRUE)


apply(table(surgery[,c("mort30","month")])/sum(table(surgery[,c("mort30","month")])),2,function(x) x/sum(x))
      # seasonal 一二七月最高
chisq.test(table(simpu[,c("mort30","month")]), simulate.p.value=TRUE)


apply(table(surgery[,c("mort30","moonphase")])/sum(table(surgery[,c("mort30","moonphase")])),2,function(x) x/sum(x))
chisq.test(table(simpu[,c("mort30","moonphase")]), simulate.p.value=TRUE)



# check correlation

#### With Age

  #continuous
ggplot(simpu, aes(x=age,y=bmi)) + geom_point(alpha=0.4,color='blue') +
  labs(title="BMI vs age",x="age",y="BMI") + theme_classic()

ggplot(simpu, aes(x=age,y=mortality_rsi)) + geom_point(alpha=0.4,color='blue') +
  labs(title="mortality_rsi vs age",x="age",y="mortality_rsi") + theme_classic()

ggplot(simpu, aes(x=age,y=complication_rsi)) + geom_point(alpha=0.4,color='blue') +
  labs(title="complication_rsi vs age",x="age",y="complication_rsi") + theme_classic()


  # categorical 
    # asa_status is correlated with age
ggplot(simpu,aes(x=asa_status, y=age, fill=asa_status)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="asa_status vs age",x="asa_status",y="age") + theme_classic()+
  scale_y_discrete()

ggplot(simpu,aes(x=baseline_cancer, y=age, fill=baseline_cancer)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_cancer vs age",x="baseline_cancer",y="age") + theme_classic()+
  scale_y_discrete() # slightly correlated

ggplot(simpu,aes(x=baseline_cvd, y=age, fill=baseline_cvd)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_cvd vs age",x="baseline_cvd",y="age") + theme_classic()+
  scale_y_discrete() # correlated

ggplot(simpu,aes(x=baseline_dementia, y=age, fill=baseline_dementia)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_dementia vs age",x="baseline_dementia",y="age") + theme_classic()+
  scale_y_discrete() # very correlated

ggplot(simpu,aes(x=baseline_diabetes, y=age, fill=baseline_diabetes)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_diabetes vs age",x="baseline_diabetes",y="age") + theme_classic()+
  scale_y_discrete() # slightly correlated


ggplot(simpu,aes(x=baseline_digestive, y=age, fill=baseline_digestive)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_digestive vs age",x="baseline_digestive",y="age") + theme_classic()+
  scale_y_discrete() # not correlated


ggplot(simpu,aes(x=baseline_osteoart, y=age, fill=baseline_osteoart)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_osteoart vs age",x="baseline_osteoart",y="age") + theme_classic()+
  scale_y_discrete() # slightly correlated


ggplot(simpu,aes(x=baseline_psych, y=age, fill=baseline_psych)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_psych vs age",x="baseline_psych",y="age") + theme_classic()+
  scale_y_discrete() # not correlated

ggplot(simpu,aes(x=baseline_pulmonary, y=age, fill=baseline_pulmonary)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_pulmonary vs age",x="baseline_pulmonary",y="age") + theme_classic()+
  scale_y_discrete() # not correletd
 
ggplot(simpu,aes(x=baseline_charlson, y=age, fill=baseline_charlson)) + geom_boxplot() +
  labs(title="baseline_charlson vs age",x="baseline_charlson",y="age") + theme_classic()+
  scale_y_discrete()

  
  
# Potential Interaction
ggplot(simpu,aes(x=asa_status, y=age, fill=asa_status)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="asa_status vs age",x="asa_status",y="age") + theme_classic()+
  scale_y_discrete() + facet_wrap(~mort30)

ggplot(simpu,aes(x=baseline_psych, y=age, fill=baseline_cvd)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_cvd vs age",x="baseline_cvd",y="age") + theme_classic()+
  scale_y_discrete() + facet_wrap(~mort30) # 死人里没有psych的年纪要大一些？？

ggplot(simpu,aes(x=baseline_charlson, y=age, fill=baseline_charlson)) + geom_boxplot() +
  labs(title="baseline_charlson vs age",x="baseline_charlson",y="age") + theme_classic()+
  scale_y_discrete() + facet_wrap(~mort30) #distribution不太一样 可能是数据量的原因



ggplot(simpu,aes(x=baseline_cancer, y=mortality_rsi, fill=baseline_cancer)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_cancer vs mortality_rsi",x="baseline_cancer",y="mortality_rsi") +
  theme_classic() +scale_y_discrete() + facet_wrap(~mort30)

ggplot(simpu,aes(x=baseline_diabetes, y=mortality_rsi, fill=baseline_diabetes)) + geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  labs(title="baseline_diabetes vs mortality_rsi",x="baseline_diabetes",y="mortality_rsi") +
  theme_classic() +scale_y_discrete() + facet_wrap(~mort30)



baseline <- simpu[,7:14]
simpu$baseline_pulmonary <- factor(simpu$baseline_pulmonary)
baseline$baseline_cancer <- as.integer(baseline$baseline_cancer) - 1
baseline$baseline_cvd <- as.integer(baseline$baseline_cvd) - 1
baseline$baseline_dementia <- as.integer(baseline$baseline_dementia) - 1
baseline$baseline_diabetes <- as.integer(baseline$baseline_diabetes) -1
baseline$baseline_digestive <- as.integer(baseline$baseline_digestive) -1
baseline$baseline_osteoart <- as.integer(baseline$baseline_osteoart) -1
baseline$baseline_psych <- as.integer(baseline$baseline_psych) -1
baseline$baseline_pulmonary <- as.integer(baseline$baseline_pulmonary) -1
baseline_sum <- rowSums(baseline)
table(baseline_sum)

write.csv(simpu,
          "/Users/xiaohan/Desktop/702/Final/cleaned_surgery.csv", 
          row.names = TRUE)




##### Modeling #####

mod_full <- glm(mort30~ahrq_ccs+age+gender+race+asa_status+bmi+baseline_cancer+baseline_cvd
                +baseline_dementia+baseline_diabetes+baseline_digestive+baseline_osteoart+
                  baseline_psych+baseline_pulmonary+baseline_charlson+mortality_rsi+
                  complication_rsi+hour+dow+month+moonphase+age:baseline_cvd,
                family=binomial(link=logit),data=simpu)
summary(mod_full)


# AIC
mod_null <- glm(mort30~1,family=binomial(link=logit),data=simpu)
Model_stepwise <- step(mod_null, scope = formula(mod_full),direction="both",trace=0)
summary(Model_stepwise)

# backward
mod_backward <- step(mod_full, scope=formula(mod_full),direction="backward",trace=0)
summary(mod_backward)

# EDA SUGGESTED MODEL
mod_eda <- glm(mort30~age+asa_status+baseline_dementia+baseline_osteoart+
                 baseline_psych+baseline_pulmonary+baseline_charlson+mortality_rsi+
                 complication_rsi+hour+dow+month,family=binomial(link=logit),data=simpu)
summary(mod_eda)

# gender,asa_status,charlson,baseline_osteoart,complication_rsi,hour,dow,month

mod_null_time <- glm(mort30~hour+dow+month,family=binomial(link=logit),data=simpu)
Model_stepwise2 <- step(mod_null_time, scope = formula(mod_full),direction="both",trace=0)
summary(Model_stepwise2)

# self selected model 
mod_selfselect <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                        complication_rsi+month,
                      family=binomial(link=logit),data=simpu)
summary(mod_selfselect)



# anova

###test if AIC model is better than full
anova(Model_stepwise, mod_full, test= "Chisq")
      # full model is no better at 0.05 level

###test if self select if better than full
anova(mod_selfselect, mod_full, test= "Chisq")
      # full model is no better at 0.05 level

### test if adding hour/dow/age improves the model
mod_selfselect_hour <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                             complication_rsi+month+hour,
                           family=binomial(link=logit),data=simpu)
anova(mod_selfselect,mod_selfselect_hour,test= "Chisq")
        # adding hour makes no improvement

mod_selfselect_dow <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                             complication_rsi+month+dow,
                           family=binomial(link=logit),data=simpu)
anova(mod_selfselect,mod_selfselect_dow,test= "Chisq")
        # adding dow makes no improvement

mod_selfselect_age <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                            complication_rsi+month+age,
                          family=binomial(link=logit),data=simpu)
anova(mod_selfselect,mod_selfselect_age,test= "Chisq")
        # adding age makes no improvement

mod_selfselect_cancer <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                            complication_rsi+month+baseline_cancer,
                          family=binomial(link=logit),data=simpu)
anova(mod_selfselect,mod_selfselect_cancer,test= "Chisq")
        # adding cancer makes no improvement

mod_selfselect_cvd <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                               complication_rsi+month+baseline_cvd,
                             family=binomial(link=logit),data=simpu)
anova(mod_selfselect,mod_selfselect_cvd,test= "Chisq")
        # adding cancer makes no improvement

mod_selfselect_pulmonary <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                            complication_rsi+month+baseline_pulmonary,
                          family=binomial(link=logit),data=simpu)
anova(mod_selfselect,mod_selfselect_pulmonary,test= "Chisq")
        # adding cancer makes no improvement

#age:baseline_cvd
mod_selfselect_inter <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                                  complication_rsi+month+age*baseline_cvd,
                                family=binomial(link=logit),data=simpu)
anova(mod_selfselect,mod_selfselect_inter,test= "Chisq")
        # adding interaction makes no improvement






### test if dropping month improves the model
model_selfselect_dropmonth <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                                    complication_rsi,
                                  family=binomial(link=logit),data=simpu)
anova(mod_selfselect,model_selfselect_dropmonth,test= "Chisq")

### test if dropping osteo improves the model
model_selfselect_droposteo <- glm(mort30~gender+asa_status+baseline_charlson+
                                    complication_rsi+month,
                                  family=binomial(link=logit),data=simpu)
anova(mod_selfselect,model_selfselect_droposteo,test= "Chisq") ## Improves !!

# multicolinearity

### baseline_osteoart vs age
mod_ageosteoart <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                              complication_rsi+month+age+baseline_osteoart:age,
                             family=binomial(link=logit),data=simpu)
summary(mod_ageosteoart)
anova(mod_selfselect_age,mod_ageosteoart,test= "Chisq")
# no improvement


### asa_status vs age
mod_ageasa <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                         complication_rsi+month+age+asa_status:age,
                       family=binomial(link=logit),data=simpu)
anova(mod_selfselect_age,mod_ageasa,test= "Chisq")


mod_selfselect <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+
                        complication_rsi+month+hour,
                      family=binomial(link=logit),data=simpu)
summary(mod_selfselect)


vif(mod_selfselect)


######## Address issue ########

simpu$hour_group[simpu$hour<12] = "AM"
simpu$hour_group[simpu$hour>=12] = "PM"
simpu$hour_group <- factor(simpu$hour_group)

simpu$comprsi_group[simpu$complication_rsi<0] = 0
simpu$comprsi_group[simpu$complication_rsi>=0] = 1
simpu$comprsi_group <- factor(simpu$comprsi_group)


final <- glm(mort30~gender+asa_status+baseline_osteoart+baseline_charlson+comprsi_group+
               month+hour_group,family=binomial(link=logit),data=simpu)
summary(final)

resid <- residuals(final,"resp")
binnedplot(x=fitted(final),y=resid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(final) >= mean(as.numeric(simpu$mort30)-1), "Yes","No")),
                            as.factor(simpu$mort30),positive = "Yes")
Conf_mat$table
Conf_mat$overall["Accuracy"]
Conf_mat$byClass[c("Sensitivity","Specificity")] 


df2 <- data.frame("Accuracy"=0.61, "Sensitivity"=0.58, "Specificity"=0.62)
kable(df2, digits=2, caption = "Confusion Matrix Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))








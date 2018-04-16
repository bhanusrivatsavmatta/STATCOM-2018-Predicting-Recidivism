crime_data <- read.csv('Compas_Test.csv')

summary(crime_data)

hist(crime_data$age)


library(dplyr)
library(ggplot2)



ggplot(data=crime_data,aes(x=age_cat,fill=age_cat)) + geom_bar(stat='count') + facet_grid(.~race)


ggplot(data=crime_data[crime_data$race=='Asian',],aes(x=age)) + geom_histogram(bins=5)
ggplot(data=crime_data[crime_data$race=='Native American',],aes(x=age)) + geom_histogram(bins=5)


#distribution across races

ggplot(data=crime_data,aes(x=race,y=juv_misd_count,fill=race)) + geom_bar(stat = "identity")
ggplot(data=crime_data,aes(x=race,y=juv_fel_count,fill=race)) + geom_bar(stat = "identity")
ggplot(data=crime_data,aes(x=race,y=juv_other_count,fill=race)) + geom_bar(stat = "identity")


# across age groups


ggplot(data=crime_data,aes(x=age_cat,y=juv_misd_count,fill=age_cat)) + geom_bar(stat = "identity")
ggplot(data=crime_data,aes(x=age_cat,y=juv_fel_count,fill=age_cat)) + geom_bar(stat = "identity")
ggplot(data=crime_data,aes(x=age_cat,y=juv_other_count,fill=age_cat)) + geom_bar(stat = "identity")


#days spent in prison

prison_days <- crime_data %>% group_by(race) %>% summarise(prison_days=mean(priors_count))


ggplot(data=prison_days,aes(x=race,y=prison_days,fill=race)) + geom_bar(stat = "identity")

# check stats for selected columns only

select_columns <- c("sex","age","age_cat","race","juv_fel_count","decile_score","juv_misd_count",
                    "juv_other_count","is_recid","score_text","start","end")


#high level summary before model building

crime_select <- crime_data[,select_columns]

crime_select$gap <- crime_select$end - crime_select$start

crime_select_summary <-crime_select %>% group_by(sex,age_cat,race,score_text) %>% summarize(avg_fel=mean(juv_fel_count),avg_decile=mean(decile_score),avg_misdemeanor=mean(juv_misd_count),avg_other=mean(juv_other_count),avg_gap=mean(gap),count=sum(!(is.na(gap))))


#columns required for model alon

model_columns <- c("sex","age","race","juv_fel_count","decile_score","juv_misd_count","juv_other_count","is_recid","score_text")


model_select <- crime_select[,model_columns]

summary(model_select)

#train test split

set.seed(7)
sample <- sample.int(n=nrow(model_select),size=floor(0.8*nrow(model_select)),replace = F)
train = model_select[sample,]
test = model_select[-sample,]

# whatever model we want to fit. 

model_select <- model_select[!is.na(model_select$is_recid),]

logistic <- glm(is_recid~sex+age+race+juv_fel_count+juv_misd_count+juv_other_count,data=model_select,family="binomial")

summary(logistic)

model_select$decile_score <-  0.1 * model_select$decile_score

logistic_score <- glm(decile_score~sex+age+race+juv_fel_count+juv_misd_count+juv_other_count,data=model_select,family="binomial")

summary(logistic_score)


###
logistic_score_no_race <- glm(decile_score~age+juv_fel_count+juv_misd_count+juv_other_count,data=model_select,family="binomial")

summary(logistic_score_no_race)


#glm(fancy~cov1+cov2+cov3+cov4+cov4_sq+cov5cov1+cov5cov2+cov5+cov6, data=securityData, family = "binomial")

set.seed(42)

train = model_select[sample,]
test = model_select[-sample,]

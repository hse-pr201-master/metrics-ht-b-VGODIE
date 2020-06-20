setwd("~/Documents/HW_Metrics/")
library(tidyverse)
library(stringr)
library(dplyr)
library(zoo)
library(lmtest)
library(car)
df <- read.csv(file="datasets_14446_19502_country_profile_variables.csv")
View(df)

to_save <- c("Region",
             "Sex.ratio..m.per.100.f..2017.",
             "GDP.per.capita..current.US..",
             "Economy..Services.and.other.activity....of.GVA.",
             "Employment..Services....of.employed.",
             "Unemployment....of.labour.force.",
             "Labour.force.participation..female.male.pop....",
             "Urban.population....of.total.population.",
             "Fertility.rate..total..live.births.per.woman.",
             "Life.expectancy.at.birth..females.males..years.",
             "Health..Total.expenditure....of.GDP.",
             "Education..Government.expenditure....of.GDP.",
             "Education..Tertiary.gross.enrol..ratio..f.m.per.100.pop..",
             "Mobile.cellular.subscriptions..per.100.inhabitants.",
             "Individuals.using.the.Internet..per.100.inhabitants.",
             "CO2.emission.estimates..million.tons.tons.per.capita.",
             "Seats.held.by.women.in.national.parliaments.."
             )
data = df[, names(df) %in% to_save]
head(data)
# Короче я вот что подумал. Было два стула: на первом пики точеные, на втором - делать скучную задачу предсказания ввп
# от каких-то регрессоров. Поэтому я решил сделать задачу необычной: в датасете я нашел такой параметр, как количество женщин
# в парламенте и подумал, что в целом это является одним из показателей "загнивающего", но в котором так хотелось бы жить,
# европейского/американского общества. Увеличение количества женщин в парламенте это сдвиг в ценностях общества, я бы даже сказал
# это изменение духовных потребностей общества. Но необходимость в удовлетворении духовных потребностей возникает только после
# решения экзистенциальных и социальных проблем - поэтому первое, что влияет на этот сдвиг - это рост доходов населения и 
# сокращения безработицы. Кроме этого, важными факторами перехода в новый формат является доля сферы услуг в экономике и 
# соответственно занятости в сфере услуг. Расходы на образование и здоровье - тоже являются индикаторами перехода к новому
# ценностному мышлению, которые позволяют женщинам участвовать в общественной жизни. Продолжительность жизни и доля 
# урбанизированного населения отражают уровень развития общества,а значит изменения ценностей. Еще одной особенностью
# развитого общества является технологический прорыв и проникновение технологий в современную жизнь - именно благодаря 
# этому новые взгляды распостраняются быстрее и проникают в массы. 

unique(data$Region)

is_eur = as.numeric(data$Region==c("SouthernEurope", "NorthernAmerica", "WesternEurope", "EasternEurope", "NorthernEurope"))

# это бинарная переменная отвечающая за то, является ли страна из Северной Америки или Европы (оплот европейского мышления)


dataset = data.frame(is_eur)

#создадим итоговый датасет, для чего обработаем регрессоры - переведем их в численный вид, заменим пропуски на нули

labour_part = str_split_fixed(data$Labour.force.participation..female.male.pop...., "/", 2)
labour_part = as.numeric(as.character(labour_part))
labour_part[is.na(labour_part)] <- 0
labour_part[labour_part == -99] = 0
labour_part = matrix(labour_part, nrow=length(labour_part)/2, ncol=2)
dataset$labour_part = (labour_part[,1] + labour_part[,2])/2

employ_serv = as.numeric(as.character(data$Employment..Services....of.employed.))
employ_serv[is.na(employ_serv)] = 0 
employ_serv[employ_serv == -99] = 0
dataset$employed_in_services = employ_serv

unemployment = as.numeric(as.character(data$Unemployment....of.labour.force.))
unemployment[is.na(unemployment)] = 0
unemployment[unemployment == -99] = 0
dataset$unemployment = unemployment

fertility = as.numeric(as.character(data$Fertility.rate..total..live.births.per.woman.))
fertility[is.na(fertility)] = 0
fertility[fertility == -99] = 0
dataset$fertility = fertility

life_exp = str_split_fixed(data$Life.expectancy.at.birth..females.males..years., "/", 2)
life_exp = as.numeric(as.character(life_exp))
life_exp[is.na(life_exp)] = 0
life_exp[life_exp == -99] = 0
life_exp = matrix(life_exp, nrow=length(life_exp)/2, ncol=2)
dataset$life_exp = (life_exp[, 1] + life_exp[,2])/2

edu_expenses = as.numeric(as.character(data$Education..Government.expenditure....of.GDP.))
edu_expenses[is.na(edu_expenses)] = 0
edu_expenses[edu_expenses == -99] = 0
dataset$edu_expenses = edu_expenses

educ_women = str_split_fixed(data$Education..Tertiary.gross.enrol..ratio..f.m.per.100.pop.., "/", 2)
educ_women = as.numeric(as.character(educ_women))
educ_women[is.na(educ_women)] = 0
educ_women[educ_women == -99] = 0
educ_women = matrix(educ_women, nrow=length(educ_women)/2, ncol=2)[,1]
dataset$educ_women = educ_women

mobile_usage = as.numeric(as.character(data$Mobile.cellular.subscriptions..per.100.inhabitants.))
mobile_usage[is.na(mobile_usage)] = 0
mobile_usage[mobile_usage == -99] = 0
dataset$mobile_usage = mobile_usage

dataset$sex_ratio = data$Sex.ratio..m.per.100.f..2017.
dataset$gdp = data$GDP.per.capita..current.US..
dataset$econ_serv = data$Economy..Services.and.other.activity....of.GVA.
dataset$urban_pop  = data$Urban.population....of.total.population.
dataset$health_expenses = data$Health..Total.expenditure....of.GDP.
dataset$internet_usage = data$Individuals.using.the.Internet..per.100.inhabitants.
dataset$emissions = data$CO2.emission.estimates..million.tons.tons.per.capita.
dataset$target = data$Seats.held.by.women.in.national.parliaments..

dataset <- dataset[!(dataset$target == -99),]

# Таким образом, я хочу исследовать зависимость количества женщин в парламенте от индикаторов развитого общества:
# региона ( европа + америка или нет - бинарная), экономических факторов: ввп на душу населения и безработицы, доли сферы услуг
# в экономике и занятости в сфере услуг, ценностных: расходы на здоровье, образование, количество девушек с высшим образованием
# факторов технологического развития: мобильная связь и интернет, от заботы об экологии - по выбросам СО2, и от средней продолжительности
# жизни, рождаемости.


scatter.smooth(dataset$educ_women, dataset$target)
scatter.smooth(dataset$educ_women**2, dataset$target)
scatter.smooth(dataset$labour_part, dataset$target)


cor(dataset)
summary(dataset)
View(educ_women)

cor(dataset$health_expenses, dataset$target)
cor(dataset)


# Оценим модель

model = lm(data=dataset, target~is_eur+labour_part+emissions+employed_in_services+ unemployment+fertility+life_exp+edu_expenses+educ_women+mobile_usage+sex_ratio+gdp+econ_serv+urban_pop+health_expenses+internet_usage)
model
fitted(model)
report = summary(model)
report$r.squared

bptest(model)
# говорит об отсутствии гетероскедастичности
gqtest(model, order.by = ~target, data=dataset, fraction=0.2)

# Однако тест Гольдфента-квандта говорит об отсутствии условной гомоскедастичности, поэтому будем использовать робастные оценки
# при проверке гипотез

# Проверим на мультиколлинеарность
vif(model)

#мультиколлинеарности не обнаружено


# Временные ряды
library(fabletools)
library(fable)
library(forecast)

ts_1 = arima.sim(model = list(order=c(1,0,0), ar=0.8), n=120) # AR(1)
ts_2 = arima.sim(model=list(order=c(3,0,0), ar=c(0.1, 0.2, 0.3)), n=120) #AR(3)
ts_3 = arima.sim(model=list(order=c(0,0,2), ma=c(1.2, 2)), n=120) #MA(2)

plot(ts_1)

# 1 ряд является стационарным, так как единственный корень характеристического многочлена меньше 1

plot(ts_2)

# 2 ряд 

plot(ts_3)

# 3 ряд является тоже стационарным следуя из графика, и при предположении, что ошибки являются белым шумом , то мат ожидания
# и дисперсия ряда являются константами.

#теперь создадим парочку арим

arima_1 = arima.sim(model=list(order=c(0,1,2), ma=c(0.7, 0.3)), n=120)
plot(arima_1)
arima_2 = arima.sim(model=list(order=c(0,0,0)), n=120)
tsdisplay(arima_2)
arima_3 = arima.sim(model=list(order=c(3,0,0), ar=c(0.25, 0.1, 0.5)), n=120)
tsdisplay(arima_3)


#создадим случайное блуждание

rnd = arima.sim(model=list(order=c(0,1,0)), n=120)
tsdisplay(rnd)

# это уравнение не имеет стационарных решений в силу равенства единственного корня характеристического многочлена единице
# здесь ACF постепенно убывает , при этом все значения выборочной функции значимо отоличаются от нуля - что является признаком
# нестационарности, при этом выборочная частная функция значима только в t-1 периоде 

# Создадим AR(1) процесс

ar_1 = arima.sim(model=list(order=c(1,0,0), ar=0.7), n=120)
tsdisplay(ar_1)

# В отличии от графика ACF случайного блуждания тем, что отличимыми от нуля являются только первые несколько значений - что
# является признаком стационарности, а частная автокорреляция имеет лишь одно отличимое от нуля значение соотвествующее t-1 периоду.

# Сгенерим ARIMA(2,0,3) и поработаем в ней

arima = arima.sim(model=list(order=c(2,0,3), ar=c(0.1, 0.65), ma=c(0.7, 0.4, 0.2)), n=120)
tsdisplay(arima)
ts_model = Arima(arima, order=c(2,0,3))

train = arima[1:100]
test = arima[101:120]

model = Arima(train, order=c(2,0,3))
summary(model)

preds = forecast(model, h=20)

plot(arima,type="l",col="red")
plot(preds,col="green")
# хреновенько модель предсказала - истинные значения идут сильно вверх, в то время как модель предсказывает их относительно стабильными
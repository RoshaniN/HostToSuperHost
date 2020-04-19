library(dplyr) ;
library(ragtop);
library(tidyr);
library(anchors);
library(stringr);
library(glmnet);
library(caTools);

all_data <- read.csv('listings.csv', stringsAsFactors= F, header = T, colClasses=c("id"="integer")) #, na.strings=c("",'N/A')

sel <- dplyr::select(all_data,id, host_is_superhost, host_response_rate, host_total_listings_count,property_type, review_scores_rating,cancellation_policy,host_identity_verified,neighbourhood_group_cleansed,instant_bookable,guests_included, room_type, bathrooms, bedrooms, beds, price, extra_people)
#sel <- dplyr::select(all_data,id, host_is_superhost, host_response_rate, host_total_listings_count,property_type, review_scores_rating,cancellation_policy,host_identity_verified,neighbourhood_group_cleansed,instant_bookable,guests_included, 
# reviews_per_month -- to main data frame

sel <- subset(sel, sel$host_is_superhost == 't' | sel$host_is_superhost == 'f')

unique(sel$cancellation_policy)

sel$cancellation_policy[sel$cancellation_policy == 'flexible'] <- 0
sel$cancellation_policy[sel$cancellation_policy == 'moderate'] <- 0
sel$cancellation_policy[sel$cancellation_policy == 'strict_14_with_grace_period'] <- 1
sel$cancellation_policy[sel$cancellation_policy == 'super_strict_30'] <- 1
sel$cancellation_policy[sel$cancellation_policy == 'strict'] <- 1
sel$cancellation_policy[sel$cancellation_policy == 'super_strict_60'] <- 1

sel$neighbourhood_group_cleansed[sel$neighbourhood_group_cleansed == 'Brooklyn'] <- 0
sel$neighbourhood_group_cleansed[sel$neighbourhood_group_cleansed == 'Manhattan'] <- 1
sel$neighbourhood_group_cleansed[sel$neighbourhood_group_cleansed == 'Queens'] <- 2
sel$neighbourhood_group_cleansed[sel$neighbourhood_group_cleansed == 'Staten Island'] <- 3
sel$neighbourhood_group_cleansed[sel$neighbourhood_group_cleansed == 'Bronx'] <- 4

#abc<-data.frame(category=c("Apartment","Barn","Bed and breakfast","Boat","Boutique hotel","Bungalow","Bus","Cabin","Camper/RV","Casa particular(Cuba)","Castle","Cave","Condominium","Cottage","Dome house","Earth house","Guest suite")

abc<-unique(sel$property_type)

for(i in 1:length(abc)){
  sel$property_type[sel$property_type == abc[i]] <- i - 1
}

sel[sel$host_is_superhost=='t',] <- replace.value(sel[sel$host_is_superhost=='t',], c("host_response_rate"), from=c("","N/A"), to="89%", verbose = TRUE)
sel[sel$host_is_superhost=='f',] <- replace.value(sel[sel$host_is_superhost=='f',], c("host_response_rate"), from=c("","N/A"), to="79%", verbose = TRUE)


sel <- replace.value(sel, c("host_is_superhost"), from="t", to="1", verbose = TRUE)
sel <- replace.value(sel, c("host_is_superhost"), from="f", to="0", verbose = TRUE)

sel <- replace.value(sel, c("instant_bookable"), from="t", to="1", verbose = TRUE)
sel <- replace.value(sel, c("instant_bookable"), from="f", to="0", verbose = TRUE)

sel <- replace.value(sel, c("host_identity_verified"), from="t", to="1", verbose = TRUE)
sel <- replace.value(sel, c("host_identity_verified"), from="f", to="0", verbose = TRUE)

sel$host_response_rate <- str_replace_all(sel$host_response_rate, "%", "")

sel <- transform(sel, host_response_rate = as.numeric(host_response_rate))
#print(head(sel))
sel <- transform(sel, cancellation_policy = as.numeric(cancellation_policy))
#print(head(sel))
sel <- transform(sel, host_is_superhost = as.numeric(host_is_superhost))
sel <- transform(sel, neighbourhood_group_cleansed = as.numeric(neighbourhood_group_cleansed))
sel <- transform(sel, host_identity_verified = as.numeric(host_identity_verified))
sel <- transform(sel, instant_bookable = as.numeric(instant_bookable))
sel <- transform(sel, guests_included = as.numeric(guests_included))
sel <- transform(sel, property_type = as.numeric(property_type))



x1 <- subset(sel , !is.na(sel$review_scores_rating) & sel$host_is_superhost==1)
x2 <- subset(sel , !is.na(sel$review_scores_rating) & sel$host_is_superhost==0)

sel[sel$host_is_superhost==1,] <- replace.value(sel[sel$host_is_superhost==1,], c("review_scores_rating"), from=NA, to=round(mean(x1$review_scores_rating)))
sel[sel$host_is_superhost==0,] <- replace.value(sel[sel$host_is_superhost==0,], c("review_scores_rating"), from=NA, to=round(mean(x2$review_scores_rating)))

x3 <- subset(sel , !is.na(sel$host_total_listings_count) & sel$host_is_superhost==1)
x4 <- subset(sel , !is.na(sel$host_total_listings_count) & sel$host_is_superhost==0)

sel[sel$host_is_superhost==1,] <- replace.value(sel[sel$host_is_superhost==1,], c("host_total_listings_count"), from=NA, to=round(mean(x3$host_total_listings_count)))
sel[sel$host_is_superhost==0,] <- replace.value(sel[sel$host_is_superhost==0,], c("host_total_listings_count"), from=NA, to=round(mean(x4$host_total_listings_count)))

#---------------------------------
mod_sel <- dplyr::select(sel,id, host_is_superhost, host_response_rate, host_total_listings_count,property_type, review_scores_rating,cancellation_policy,host_identity_verified,neighbourhood_group_cleansed,instant_bookable,guests_included)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(mod_sel,SplitRatio = 0.60) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_data =subset(mod_sel,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test_data =subset(mod_sel, sample==FALSE)
#------------------------
logitMod <- glm(host_is_superhost ~ property_type+guests_included+ instant_bookable + host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy+host_identity_verified+neighbourhood_group_cleansed, data=training_data, family=binomial(link="logit"))
#predicted <- plogis(predict(logitMod, test_data))
predicted_logit <- predict(logitMod, test_data, type="response")
summary(logitMod)
#misClassError(test_data$host_is_superhost, predicted)
coefficients<-coef(logitMod)
coefficients<-sort(coefficients)
print(coefficients)
#-------------------------
probitMod <- glm(host_is_superhost ~ host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy, data=training_data, family=binomial(link="probit"))
#predicted <- plogis(predict(probitMod, test_data))
predicted_probit <- predict(probitMod, test_data, type="response")




#room_type, bathrooms, bedrooms, beds, price, extra_people

sel <- replace.value(sel, c("room_type"), from="Entire home/apt", to="0", verbose = TRUE)
sel <- replace.value(sel, c("room_type"), from="Private room", to="1", verbose = TRUE)
sel <- replace.value(sel, c("room_type"), from="Shared room", to="2", verbose = TRUE)
sel <- replace.value(sel, c("room_type"), from="Hotel room", to="3", verbose = TRUE)
sel <- transform(sel, room_type = as.numeric(room_type))

nrow(sel)
sel <- subset(sel , !is.na(sel$bathrooms))#, is.na(sel$bedrooms), is.na(sel$beds))
sel <- subset(sel, !is.na(sel$bedrooms))
sel <- subset(sel, !is.na(sel$beds))

sel$price <- str_replace_all(sel$price, "\\$", "")
sel$price <- str_replace_all(sel$price, ",", "")
sel <- transform(sel, price = as.numeric(price))

sel$extra_people <- str_replace_all(sel$extra_people, "\\$", "")
sel$extra_people <- str_replace_all(sel$extra_people, ",", "")
sel <- transform(sel, extra_people = as.numeric(extra_people))


Brooklyn <- subset(sel , sel$neighbourhood_group_cleansed == 0)
Manhattan <- subset(sel , sel$neighbourhood_group_cleansed == 1)
Queens <- subset(sel , sel$neighbourhood_group_cleansed == 2)
Staten_Island <- subset(sel , sel$neighbourhood_group_cleansed == 3)
Bronx <- subset(sel , sel$neighbourhood_group_cleansed== 4)

#  TO DO FOR NEIGHBORHOOD GROUP ANALYSIS: Normalize price and stuff...


#------------- Brooklyn -----------------
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(Brooklyn,SplitRatio = 0.60) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_data =subset(Brooklyn,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test_data =subset(Brooklyn, sample==FALSE)
#------------------------
logitMod <- glm(host_is_superhost ~ property_type+guests_included+ instant_bookable + host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy+host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="logit"))
#predicted <- plogis(predict(logitMod, test_data))
predicted_logit <- predict(logitMod, test_data, type="response")
summary(logitMod)
#misClassError(test_data$host_is_superhost, predicted)
coefficients<-coef(logitMod)
coefficients<-sort(coefficients)
print(coefficients)
#-------------------------
probitMod <- glm(host_is_superhost ~ host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy +host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="probit"))
#predicted <- plogis(predict(probitMod, test_data))
predicted_probit <- predict(probitMod, test_data, type="response")


#------------- Manhattan -----------------
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(Manhattan,SplitRatio = 0.60) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_data =subset(Manhattan,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test_data =subset(Manhattan, sample==FALSE)
#------------------------
logitMod <- glm(host_is_superhost ~ property_type+guests_included+ instant_bookable + host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy+host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="logit"))
#predicted <- plogis(predict(logitMod, test_data))
predicted_logit <- predict(logitMod, test_data, type="response")
summary(logitMod)
#misClassError(test_data$host_is_superhost, predicted)
coefficients<-coef(logitMod)
coefficients<-sort(coefficients)
print(coefficients)
#-------------------------
probitMod <- glm(host_is_superhost ~ host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy +host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="probit"))
#predicted <- plogis(predict(probitMod, test_data))
predicted_probit <- predict(probitMod, test_data, type="response")



#------------- Queens -----------------
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(Queens,SplitRatio = 0.60) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_data =subset(Queens,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test_data =subset(Queens, sample==FALSE)
#------------------------
logitMod <- glm(host_is_superhost ~ property_type+guests_included+ instant_bookable + host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy+host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="logit"))
#predicted <- plogis(predict(logitMod, test_data))
predicted_logit <- predict(logitMod, test_data, type="response")
summary(logitMod)
#misClassError(test_data$host_is_superhost, predicted)
coefficients<-coef(logitMod)
coefficients<-sort(coefficients)
print(coefficients)
#-------------------------
probitMod <- glm(host_is_superhost ~ host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy +host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="probit"))
#predicted <- plogis(predict(probitMod, test_data))
predicted_probit <- predict(probitMod, test_data, type="response")



#------------- Staten_Island -----------------
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(Staten_Island,SplitRatio = 0.60) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_data =subset(Staten_Island,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test_data =subset(Staten_Island, sample==FALSE)
#------------------------
logitMod <- glm(host_is_superhost ~ property_type+guests_included+ instant_bookable + host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy+host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="logit"))
#predicted <- plogis(predict(logitMod, test_data))
predicted_logit <- predict(logitMod, test_data, type="response")
summary(logitMod)
#misClassError(test_data$host_is_superhost, predicted)
coefficients<-coef(logitMod)
coefficients<-sort(coefficients)
print(coefficients)
#-------------------------
probitMod <- glm(host_is_superhost ~ host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy +host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="probit"))
#predicted <- plogis(predict(probitMod, test_data))
predicted_probit <- predict(probitMod, test_data, type="response")





#------------- Bronx -----------------
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(Bronx,SplitRatio = 0.60) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
training_data =subset(Bronx,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test_data =subset(Bronx, sample==FALSE)
#------------------------
logitMod <- glm(host_is_superhost ~ property_type+guests_included+ instant_bookable + host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy+host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="logit"))
#predicted <- plogis(predict(logitMod, test_data))
predicted_logit <- predict(logitMod, test_data, type="response")
summary(logitMod)
#misClassError(test_data$host_is_superhost, predicted)
coefficients<-coef(logitMod)
coefficients<-sort(coefficients)
print(coefficients)
#-------------------------
probitMod <- glm(host_is_superhost ~ host_response_rate + host_total_listings_count + review_scores_rating + cancellation_policy +host_identity_verified+room_type+ bathrooms+ bedrooms+ beds+ price+ extra_people, data=training_data, family=binomial(link="probit"))
#predicted <- plogis(predict(probitMod, test_data))
predicted_probit <- predict(probitMod, test_data, type="response")

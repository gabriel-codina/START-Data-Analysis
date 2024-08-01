#Gabriel Codina
#necessary libraries
library(tidyverse)
library(modelr)
library(ggthemes)



#reading in data file
StartResponse <- read_csv("C:/Users/gabco/Downloads/Copy of START Form (Responses) - Form Responses 1 (1).csv")

StartResponseAvg <- StartResponse %>%group_by(`How often do you play video games? (daily, few times a week, few times a month,       rarely, never)`) %>%summarise(avgScore = mean(`I would like to use more immersive simulations in the future.`))



#Part 1 Visualizing variables
ggplot(data = StartResponseAvg) +geom_bar(mapping = aes(x = `How often do you play video games? (daily, few times a week, few times a month,       rarely, never)`, y = avgScore),   stat = "identity") + labs(x = "Frequency of Playing Video Games",y = "Average Score for Immersive Simulations",title = "Average Score for Immersive Simulations by Video Game Frequency")
ggplot(data=StartResponse) + geom_boxplot(mapping=aes(x=`How often do you play video games? (daily, few times a week, few times a month,       rarely, never)`))+facet_wrap(~`Have you used VR before?`)



#Testing predictors of if they would like to use more simulations in the future

#This shows that both Video game usage and previous vr use are poor predictors of The users experience
lm1 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`How often do you play video games? (daily, few times a week, few times a month,       rarely, never)`))
summary(lm1) #Adjusted R-squared:  -0.03364 

lm2 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`Have you used VR before?`))
summary(lm2) #Adjusted R-squared:  -0.02891

lm3 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`I thought the system was easy to use.`))
summary(lm3) #Adjusted R-squared:  0.3242 

lm4 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`I found the simulation to be engaging.`))
summary(lm4) #Adjusted R-squared:   0.6429 

lm5 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`The simulation increased my knowledge of the semiconductor fabrication process.`))
summary(lm5) #Adjusted R-squared:  0.3526 

lm6 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`The simulation increased my interest level in the semiconductor industry.`))
summary(lm6) #Adjusted R-squared:  0.3922 

#Best predictor was Engagement(Graph)
StartResponse <- StartResponse %>% add_predictions(lm4)
ggplot(data=StartResponse) + geom_point(mapping = aes(y=`I would like to use more immersive simulations in the future.`, x= `I found the simulation to be engaging.`)) +geom_line(mapping = aes(x = `I found the simulation to be engaging.`, y = pred)) + theme_calc()+labs(title = "Linear Regression With Combinations of Predictors", x = "Engagement Level", y = "Future Interest in VR Learning")


#What variable best predicts level of engagement
lm1 <- lm(data=StartResponse, formula = `I found the simulation to be engaging.`~(`How often do you play video games? (daily, few times a week, few times a month,       rarely, never)`))
summary(lm1) #Adjusted R-squared:  -0.02547  

lm2 <- lm(data=StartResponse, formula = `I found the simulation to be engaging.`~(`Have you used VR before?`))
summary(lm2) #Adjusted R-squared:  -0.03846 

lm3 <- lm(data=StartResponse, formula = `I found the simulation to be engaging.`~(`I thought the system was easy to use.`))
summary(lm3) #Adjusted R-squared:  0.4461 

lm4 <- lm(data=StartResponse, formula = `I found the simulation to be engaging.`~(`I would like to use more immersive simulations in the future.`))
summary(lm4) #Adjusted R-squared:  0.6429 

lm5 <- lm(data=StartResponse, formula = `I found the simulation to be engaging.`~(`The simulation increased my knowledge of the semiconductor fabrication process.`))
summary(lm5) #Adjusted R-squared:  0.4712 

lm6 <- lm(data=StartResponse, formula = `I found the simulation to be engaging.`~(`The simulation increased my interest level in the semiconductor industry.`))
summary(lm6) #Adjusted R-squared:  0.1917 
#best predictor was Future Interest

#What variable best predicts increased knowledge
lm1 <- lm(data=StartResponse, formula = `The simulation increased my knowledge of the semiconductor fabrication process.`~(`How often do you play video games? (daily, few times a week, few times a month,       rarely, never)`))
summary(lm1) #Adjusted R-squared:  -0.01728    

lm2 <- lm(data=StartResponse, formula = `The simulation increased my knowledge of the semiconductor fabrication process.`~(`Have you used VR before?`))
summary(lm2) #Adjusted R-squared:  0.08327  

lm3 <- lm(data=StartResponse, formula = `The simulation increased my knowledge of the semiconductor fabrication process.`~(`I thought the system was easy to use.`))
summary(lm3) #Adjusted R-squared:  0.1389 

lm4 <- lm(data=StartResponse, formula = `The simulation increased my knowledge of the semiconductor fabrication process.`~(`I would like to use more immersive simulations in the future.`))
summary(lm4) #Adjusted R-squared:  0.3526 

lm5 <- lm(data=StartResponse, formula = `The simulation increased my knowledge of the semiconductor fabrication process.`~(`I found the simulation to be engaging.`))
summary(lm5) #Adjusted R-squared:  0.4712 

lm6 <- lm(data=StartResponse, formula = `The simulation increased my knowledge of the semiconductor fabrication process.`~(`The simulation increased my interest level in the semiconductor industry.`))
summary(lm6) #Adjusted R-squared:  0.4854  
#Best predictor was Increased Interest(Graph)
StartResponse <- StartResponse %>% add_predictions(lm6)
ggplot(data=StartResponse) + geom_point(mapping = aes(y=`The simulation increased my knowledge of the semiconductor fabrication process.`, x= `The simulation increased my interest level in the semiconductor industry.`)) +geom_line(mapping = aes(x = `The simulation increased my interest level in the semiconductor industry.`, y = pred)) + theme_calc()+labs(title = "Linear Regression With Combinations of Predictors", x = "Increased Interest in Semicondoctor Industry", y = "Increased Semicondoctor Knowledge")


#What variable best predicts level of interest
lm1 <- lm(data=StartResponse, formula = `The simulation increased my interest level in the semiconductor industry.`~(`How often do you play video games? (daily, few times a week, few times a month,       rarely, never)`))
summary(lm1) #Adjusted R-squared:  0.001619   

lm2 <- lm(data=StartResponse, formula = `The simulation increased my interest level in the semiconductor industry.`~(`Have you used VR before?`))
summary(lm2) #Adjusted R-squared:  0.02496    

lm3 <- lm(data=StartResponse, formula = `The simulation increased my interest level in the semiconductor industry.`~(`I thought the system was easy to use.`))
summary(lm3) #Adjusted R-squared:  0.2146  

lm4 <- lm(data=StartResponse, formula = `The simulation increased my interest level in the semiconductor industry.`~(`I would like to use more immersive simulations in the future.`))
summary(lm4) #Adjusted R-squared:  0.3922  

lm5 <- lm(data=StartResponse, formula = `The simulation increased my interest level in the semiconductor industry.`~(`The simulation increased my knowledge of the semiconductor fabrication process.`))
summary(lm5) #Adjusted R-squared:  0.4854  

lm6 <- lm(data=StartResponse, formula = `The simulation increased my interest level in the semiconductor industry.`~(`I found the simulation to be engaging.`))
summary(lm6) #Adjusted R-squared:  0.1917  
#best predictor was Increased Kowledge



#Run linear regression with different combinations of predictor variables

#interest*engagement
lm7 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`The simulation increased my interest level in the semiconductor industry.`*`I found the simulation to be engaging.`))
summary(lm7) #Adjusted R-squared:  0.7582  
#engagement*knowledge
lm8 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`The simulation increased my knowledge of the semiconductor fabrication process.`*`I found the simulation to be engaging.`))
summary(lm8) #Adjusted R-squared:  0.6256   
#interest*knowledge
lm9 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`The simulation increased my interest level in the semiconductor industry.`*`The simulation increased my knowledge of the semiconductor fabrication process.`))
summary(lm9) #Adjusted R-squared:  0.405   

#interest*engagement*knowledge
lm10 <- lm(data=StartResponse, formula = `I would like to use more immersive simulations in the future.`~(`The simulation increased my interest level in the semiconductor industry.`*`The simulation increased my knowledge of the semiconductor fabrication process.`*`I found the simulation to be engaging.`))
summary(lm10) #Adjusted R-squared:  0.7946 



#Interest*Engagement Plot
StartResponse <- StartResponse %>% add_predictions(lm7)
ggplot(data=StartResponse) + geom_point(mapping = aes(y=`I would like to use more immersive simulations in the future.`, x=`The simulation increased my interest level in the semiconductor industry.`*`I found the simulation to be engaging.` )) +geom_line(mapping = aes(x =`The simulation increased my interest level in the semiconductor industry.`*`I found the simulation to be engaging.` , y = pred)) + theme_calc()+labs(title = "Linear Regression With Combinations of Predictors", x = "Increased Interest * Engagement Level", y = "Future Interest in VR Learning")
sqrt(mean((StartResponse$`I would like to use more immersive simulations in the future.` - StartResponse$pred)^2))
#RMSE 0.5122579

#Interest*Knowledge Plot
StartResponse <- StartResponse %>% add_predictions(lm8)
ggplot(data=StartResponse) + geom_point(mapping = aes(y=`I would like to use more immersive simulations in the future.`, x= `The simulation increased my knowledge of the semiconductor fabrication process.`*`I found the simulation to be engaging.`)) +geom_line(mapping = aes(x =`The simulation increased my knowledge of the semiconductor fabrication process.`*`I found the simulation to be engaging.` , y = pred)) + theme_calc()+labs(title = "Linear Regression With Combinations of Predictors", x = "Increased Interest * Increased Knowledge", y = "Future Interest in VR Learning")
sqrt(mean((StartResponse$`I would like to use more immersive simulations in the future.` - StartResponse$pred)^2))
#RMSE 0.4327094

#Engagement*Knowledge Plot
StartResponse <- StartResponse %>% add_predictions(lm9)
ggplot(data=StartResponse) + geom_point(mapping = aes(y=`I would like to use more immersive simulations in the future.`, x= `The simulation increased my interest level in the semiconductor industry.`*`The simulation increased my knowledge of the semiconductor fabrication process.`)) +geom_line(mapping = aes(x = `The simulation increased my interest level in the semiconductor industry.`*`The simulation increased my knowledge of the semiconductor fabrication process.`, y = pred)) + theme_calc()+labs(title = "Linear Regression With Combinations of Predictors", x = "Increased Knowledge * Engagement Level", y = "Future Interest in VR Learning")
sqrt(mean((StartResponse$`I would like to use more immersive simulations in the future.` - StartResponse$pred)^2))
#RMSE 0.6373796

#plot the highest combination
StartResponse <- StartResponse %>% add_predictions(lm10)
ggplot(data=StartResponse) + geom_point(mapping = aes(y=`I would like to use more immersive simulations in the future.`, x= `The simulation increased my interest level in the semiconductor industry.`*`The simulation increased my knowledge of the semiconductor fabrication process.`*`I found the simulation to be engaging.`)) +geom_line(mapping = aes(x = `The simulation increased my interest level in the semiconductor industry.`*`The simulation increased my knowledge of the semiconductor fabrication process.`*`I found the simulation to be engaging.`, y = pred)) + theme_calc()+labs(title = "Linear Regression With Combinations of Predictors", x = "Increased Interest * Increased Knowledge * Engagement Level", y = "Future Interest in VR Learning")
sqrt(mean((StartResponse$`I would like to use more immersive simulations in the future.` - StartResponse$pred)^2))
#RMSE 0.4327094


#{Increased Interest * Increased Knowledge * Engagement Level} is on average within 0.4327094 units of the Future Interest in VR Learning Score
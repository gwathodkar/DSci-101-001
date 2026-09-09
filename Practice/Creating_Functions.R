

##############################
##### Creating Functions #####
##############################


#################
### Functions ###
#################

################################################################################ 
### Example 1: Computing the predicted number of calories required by women ###
################################################################################

calories_women <- function(weight, height, age){
  calories <- (10 * weight) + (6.25 * height) - (5 * age) - 161
  return(calories)
}

calories_women(weight = 65, height = 170, age = 35)
calories_women(65, 170, 35)


# Using default values
calories_women_fixed <- function(weight, height, age = 30){
  calories <- (10 * weight) + (6.25 * height) - (5 * age) - 161
  return(calories)
}  

calories_women_fixed(weight = 65, height = 170)
calories_women_fixed(weight = 65, height = 170, age = 35)


###################################################
### Example 2: Find the max between two numbers ###
###################################################

## Takes in two number inputs

find_max <- function(a,b){
  if(a>b){
    return(a)
  }else if(b>a){
    return(b)
  }else if(b==a){
    return("the two values are equal")
  }
}


find_max(1,2)
find_max(5,3)
find_max(6,6)

##################################################
### Example 3: Computing the Mean from Scratch ###
##################################################

mean_abdalla <- function(x){
  my_mean <- sum(x)/length(x)
  return(my_mean)
}

v <- 1:4
mean_abdalla(v)


########################################################
#################### Do it yourself ####################
########################################################


# 1. Create a function that computes the predicted number of calories 
#    required by men. Your arguments should be weight (in kg), height (in cm) 
#    and age (in years).

#    The formula for men is: 
#    10*weight(in kg) + 6.25*height(in cm) - 5*age(in years) + 5

calories_men <- function(weight, height, age){
  calories <- (10 * weight) + (6.25 * height) - (5 * age) +5
  return(calories)
}

calories_men(70, 160, 32)



# 2. Create a function that computes the circumference of a circle. Your arguments should be 
#    the radius of the circle. 

circumference<- function(radius){
  crc<-2*pi*radius
  return(crc)
}
circumference(4)




# 3. Create a function that changes the temperature from Fahrenheit to Celsius.  
temperature_converter<-function(tempinfahrenheit){
  tempincelsius<-(5*(tempinfahrenheit-32))/9
  return(tempincelsius)
}

temperature_converter(70)



# 4. Create a function that changes the temperature from Celsius to Fahrenheit.  

temperature_converter_2<-function(tempincel){
  tempinfah<-32+9*tempincel/5
  return(tempinfah)
}

temperature_converter_2(25)


# 5. Create a function such that, the arguments are the temperature. The function 
#    must tell us that: "It's freezing", if the temperature is at or below 0, 
#                       "It's cold", if the temperature is higher than 0, but lower or equal to 10, 
#                       "It's cool", if the temperature is higher than 10, but lower or equal to 20, 
#                       "It's warm", if the temperature is higher than 20, but lower or equal to 30, 
#                       "It's hot", if the temperature is higher than 30. 

weather<-function(temperature){
  t<-temperature
  if (t<=0){
    print("It's freezing")
  }
  else if (t>0 &t<=10){
    print("It's cool")
  }
  else if (t>10 &t<=20){
    print("It's warm")
  }
  else if (t>20 & t<=30){
    print("It's hot")
  }
}

weather(25)
weather(3)
weather(-9)
weather(15)


# 6. Create a function that returns the median and the mean of a vector. The output should 
#    be a vector such that the first element of the vector is the mean and the second 
#    element is the median. The arguments should be a numeric vector. 

mean_median<-function(v){
  y<-NA
  y[1]<-mean(v)
  y[2]<-median(v)
  return(y)
}
  
v<-1:100
mean_median(v)


# 7. Create a function that returns the standard deviation of a numeric vector. Do not use
#    the function sd or var in your computations. The output should be a number that represents 
#    the standard deviation. The arguments should be a numeric vector. 

# Steps to compute the standard deviation: 
# 1. average all the values in the dataset. 
# 2. subtract the mean from every value in the dataset. 
# 3. square every value that you got in part 2. 
# 4. sum all the values you got in part 3.
# 5. divide what you got in part 4 by the number of elements in the data minus 1. 
# 6. Take the square root of what you got in part 5. This is the standard deviation. 

standard_deviation_g<-function(v){
  m<-mean(v)
  y<-NA
  y<-v-m
  y2<-NA
  y2<-y*y
  sumy2<-sum(y2)
  n5<-sumy2/(length(v)-1)
  sd_g=sqrt(n5)
  return(sd_g)
}

v<-1:10
standard_deviation_g(v)
sd(v)
# Once you build your function, test it with any vector. Then use sd() function to compare 
# the results. 



# 8. Homework Problem
#    Zodiac signs can be categorized into water signs (cancer, scorpio, pisces), 
#    air signs (gemini, libra, aquarius), earth signs (taurus, virgo, capricorn), 
#    and fire signs (aries, leo, sagittarius). Create a function such that it's 
#    argument is a zodiac sign, and it will return what zodiac category they 
#    belong to. Make sure to include in your output the following message "cannot 
#    read your input, try again!" in the case that someone types an argument that 
#    is not a zodiac sign


#Dont use print functions use return instead

water_signs<- c("cancer", "scorpio", "pisces")
air_signs<-c("gemini", "libra", "aquarius")
earth_signs<-c("taurus", "virgo", "capricorn") 
fire_signs<-c("aries", "leo", "sagittarius")

zodiac_category<- function(zodiac_sign){
  if (zodiac_sign %in% water_signs){
    print("It's a water sign")
  }
  else if (zodiac_sign %in% air_signs){
    print("It's an air sign")
  }
  else if (zodiac_sign %in% fire_signs){
    print("It's a fire sign")
  }
  else if (zodiac_sign %in% earth_signs){
    print("It's an earth sign")
  }
  else{
    print("cannot read your input, try again!")
  }
  
}

zodiac_category("libra")
zodiac_category("earth")




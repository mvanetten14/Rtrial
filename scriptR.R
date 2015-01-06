
###data with 1 patient per row, columns different days
##read in file
infl2 <-read.csv("inflammation-02.csv", header=FALSE)

##looking at data files
head(infl2)
class(infl2)
str(infl2)
dim(infl2)

#look at patient 30, day 2
infl2[30,2]

mean (infl2[,7])
min(infl2[,7])
median(infl2[,7])
sd(infl2[,7])

##margins=2 is for columns
aveday <- apply(infl2, MARGIN=2, mean)
minday <- apply(infl2, MARGIN=2, min)
maxday <- apply(infl2, MARGIN=2, max)
stdday <- apply(infl2, MARGIN=2, sd)


#plot
plot(aveday)
plot(stdday)

#simple functions
fahr_to_kelvin <- function(temp) {
  kelvin <- (temp-32)*(5/9)+273.15
  return(kelvin)
}

fahr_to_kelvin(9)


kelvin_to_cel <- function(temp) {
  cel <- (temp-273.15)
  return(cel)
}

#convert farenheit to celcius
kelvin_to_cel(fahr_to_kelvin(10))

fahr_to_cel <- function (temp) {
  cel <-kelvin_to_cel(fahr_to_kelvin(temp))
  return(cel)
}

fahr_to_cel(10)

##another function
best_practice <-c("write", "programs", "for", "people", "not", "computers")
ast <-"***"

#add asterisk to beginning and end of original
fence <- function (original, wrapper) {
  new<-c(wrapper, original, wrapper)
  return(new)
}

fence(best_practice, ast)

##basic function for analysis

analyze <- function (file) {
  data_set <- read.csv(file, header=FALSE)
  res_mean <- apply(data_set, MARGIN=2, mean)
  res_min <- apply(data_set, MARGIN=2, min)
  res_max <- apply(data_set, MARGIN=2, max)
  #return(c(res_mean, res_min, res_max))
  plot(res_max, main=file)
  plot(res_mean, main=file)
}

analyze("inflammation-03.csv")

length(best_practice)

#use a for loop to do the same thing
len <-0
for (word in best_practice) {
  len=len+1
}

len

#now for sums
values <- c(1, 2, 3)

sum_func <- function (vector) {
  total <-0
  for (thing in vector){
    total<-total+thing
  }
  return(total)
}

sum_func(values)


#batch analyze inflamation files
file_list<-list.files(pattern="inflammation")

for (files in file_list) {
  print (files)
  analyze(files)
}

#a function that uses the pattern as input
analyze_all <- function (pattern_char) {
  file_list <-list.files(pattern=pattern_char)
  for (files in file_list) {
    print (files)
    analyze(files)
  }
}

analyze_all("inflammation")

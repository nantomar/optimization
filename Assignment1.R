#define a variable to read the file to be used in this program
sal.csv <- read.csv("//Users//ntomar//Desktop//books and docs//Rfiles//Salary_Data.csv")
sal.csv

#command to get statistical summary 
summary(sal.csv)

#to understand relationship between years of experience and salary -plot is used 
#define x and y axis of the plot ,define type fo plot , name if req and color if req
plot(sal.csv,type = "b" )
plot(sal.csv,type = "b", col="violet")

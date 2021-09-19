library("ggplot2")

# getting desired constants from user
a = as.integer(readline(prompt="Enter 1st coefficient:"))
b = as.integer(readline(prompt="Enter 2nd coefficient:"))
c = as.integer(readline(prompt="Enter 3rd coefficient:"))

# getting endpoints
start = as.integer(readline(prompt="Enter left bound (x = ?):"))
end = as.integer(readline(prompt="Enter right bound (x = ?):"))

# getting subintervals
n = as.integer(readline(prompt="How many subintervals:"))

# getting type of riemann sum
# type = readline(prompt="What type of Riemann Sum (l, m, r): ")

# riemann sums..
# need:
# left hand or right hand
# how many subintervals (n)
#   ^from that we can calculate delta x, b - a / n
#   x_i = a + i * delta x
#   make rectangles with f(x_i) for left hand or f(x_i+1) for right hand
# once we can do f(x_i) by plugging in all x_i's into f4(x) function above
# have to figure out how to draw the rectangles... i could draw lines but i wouldnt
# be able to fill them

# creating functions
f <- function(x){
  return (a*x^2 + b*x + c)
}

# testing output to make sure function is running correct
f(0)
f(1)
f(2)

# creating the approximating rectangles
delta_x <- (end - start) / n

xmins <- xmaxs <- ymins <- ymaxs <- custom <- c()
temp <- start + delta_x
xmins <- append(xmins, start, after = length(xmins))
custom <- append(custom, start, after = length(custom))
while (temp < end) {
  xmins <- append(xmins, temp, after = length(xmins))
  xmaxs <- append(xmaxs, temp, after = length(xmaxs))
  custom <- append(custom, temp, after = length(custom))
  temp <- temp + delta_x
}
custom <- head(custom, -1)
xmaxs <- append(xmaxs, temp)
custom <- append(custom, temp)

temp <- start

while (temp < end) {
  ymins <- append(ymins, min(0, f(temp)))
  ymaxs <- append(ymaxs, max(0, f(temp)))
  temp <- temp + delta_x
}

# graph
ggplot(data.frame(x=custom), aes(x = x)) +
  geom_hline(yintercept= 0) + # x-axis
  geom_vline(xintercept = 0) + # y-axis
  stat_function(fun = f) + # user function
  geom_rect(xmin = xmins, xmax = xmaxs, ymin = ymins, ymax = ymaxs, linetype="dotted", alpha=0.8, color="yellow")
  

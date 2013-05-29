* local installation directory of gnuplotxyz
$setlocal gnuplotxyz_location 'D:\util\gnuplotxyz\gnuplotxyz.gms'

set line "lines on the plot, number of slope terms" /l1*l5/;

* number of observations
$eval N 30

set x "observation points" /x1*x%N%/;

parameters
         x_val(x) "x values"
         y_val(line, x) "y values, depending on slope parameter"
;

scalar interv_low /20/;
scalar interv_up  /100/;


scalar quota "quota, i.e. the turning point of the sigmoid function"     /80/;

parameter sigmoid_slope(line) "gradually increasing slope terms";
sigmoid_slope(line) = ord(line)*10;

* (x,y) coordinates of the graph
x_val(x) = interv_low + (interv_up - interv_low) * ord(x) / %N%;
y_val(line, x) = sigmoid(sigmoid_slope(line) * (x_val(x) - quota) / quota);

parameter todraw(line, x, *);

todraw(line, x, "x") = x_val(x);
todraw(line, x, "y") = y_val(line, x);

display todraw;

*$exit

$libinclude %gnuplotxyz_location% todraw x y
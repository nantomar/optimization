/* Objective function */
min: c1q1 + c2q2 + c3q3 + c4q4 + 8 s1 + 8 s2 + 8 s3 + 8 s4 + 700 ts1 + 700 ts2 + 700 ts3 + 700 ts4 + 1400 tl1 + 1400 tl2 + 1400 tl3 + 1400 tl4;

/* Constraints */

c1 = 20;
c2 = 25;
c3 = 30;
c4 = 40;

so = 100;
s4 = 100;

so + q1 - s1 = d1;
s1 + q2 - s2 = d2;
s2 + q3 - s3 = d3;
s3 + q4 - s4 = d4;


1000 <= 500 ts1 + 1200 tl1;
1200 <= 500 ts2 + 1200 tl2;
1500 <= 500 ts3 + 1200 tl3;
1800 <= 500 ts4 + 1200 tl4;

d1 = 1000;
d2 = 1200;
d3 = 1500;
d4 = 1800;

 
int tsi , ts2 , ts3 , ts4 , ts4 , tl1 , tl2 , tl3 , tl4;



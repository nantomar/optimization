/* Objective function */
  min: c1q1 + c2q2 + c3q3 + c4q4 + 8 s1 + 8 s2 + 8 s3 + 8 s4 + 700 Ts1 + 700 Ts2 + 700 Ts3 + 700 Ts4 + 1400 Tl1 + 1400 Tl2 + 1400 Tl3 + 1400 Tl4;

/* Constraints */
c1 = 20;
c2 = 25;
c3 = 30;
c4 = 40;

S0 = 100;
S4 = 100;

S0 + q1 - S1 = d1;
S1 + q2 - S2 = d2;
S2 + q3 - S3 = d3;
S3 + q4 - S4 = d4;

1000 <= 500 Ts1 + 1200 Tl1;
1200 <= 500 Ts2 + 1200 Tl2;
1500 <= 500 Ts3 + 1200 Tl3;
1800 <= 500 Ts4 + 1200 Tl4;
int Tsi , Ts2 , Ts3 , Ts4 , Ts4 , Tl1 , Tl2 , Tl3 , Tl4;



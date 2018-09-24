/* Objective function */
min: 12 q1 + 14 q2 + 16 q3 + 18 q4 + 2 s1 + 2 s2 + 2 s3 + 2 s4 + 10 r1 + 12 r2 + 14 r3 + 16 r4;

/* Constraints */
q1 + r1 - s1 = 100;
s1 + q2 + r2 - s2 = 200;
s2 + q3 + r3 - s3 = 150;
s3 + q4 + r4 - s4 = 400;

q1 <= 150;
r1 <= M b1;
q1 >= 150 b1;
q2 <= 150;
r2 <= M b2;
q2 >= 150 b2;
q3 <= 150;
r3 <= M b3;
q3 >= 150 b3;
q4 <= 150;
r4 <= M b4;
q4 >= 150 b4;

M = 1000;

b1 <= 1;
b2 <= 1;
b3 <= 1;
b4 <= 1;
  
int b1;
int b2;
int b3;
int b4;

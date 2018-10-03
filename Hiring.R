/* Objective function */
  8 s1 + 8 s2 + 8 s3 + 8 s4 + 8 s5 + 8 s6 + 10 f1 + 10 f2 + 10 f3 + 10 f4 + 10 f5 + 10 f6 + 5 h1 + 5 h2 + 5 h3 + 5 h4 + 5 h5 + 5 h6;

/* Constraints */
s1 = 20;
s2 >= d1;
s3 >= d2;
s4 >= d5;
s5 >= d6;
  
  
s1 + h1 - f1 = s2;
s2 + h2 - f2 = s3;
s3 + h3 - f3 = s4;
s4 + h4 - f4 = s5;
s5 + h5 - f5 = s6;
  
d1 = 30;
d2 = 60;
d3 = 55;
d4 = 40;
d5 = 45;
d6 = 50;
  
  
  
  
  
  
  
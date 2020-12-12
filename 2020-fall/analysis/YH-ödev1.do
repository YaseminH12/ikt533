/*Angrist and Krueger (1991)*/

use "C:\Users\Bilgisayar\Documents\YH-ödev1\2020-fall\data\raw\NEW7080.dta" /* Transfer the data */

rename (v1 v2 v4 v5 v6 v9 v10 v11 v12 v13 v16 v18 v19 v20 v21 v24 v25 v27) ///
(AGE AGEQ EDUC ENOCENT ESOCENT LWKLYWGE MARRIED MIDATL MT NEWENG CENSUS QOB RACE SMSA SOATL WNOCENT WSOCENT YOB)

/*a*/
gen dum1 = (CENSUS==80 & YOB==30 & QOB<=2) /* Generating dummy for born in the first and the second quarter of birth in 1930 */
bys dum1 : reg EDUC b4.QOB if CENSUS==80 & YOB >= 30 & YOB <= 39 /* Generating dummy variable for quarters of birth in regression model, estimating the effect of quarters of birth on education, and assign 4.quarter of birth as a base [1930-1939] "Not including trend effect"*/

gen dum = (QOB==1) /* Generating dummy variable for born in first quarter of year */

/* 1920-1929 [CENSUS 1970] */

ttest  LWKLYWGE, by(dum), if CENSUS==70 & YOB >= 1920 & YOB <= 1929 /* Checking statistical reliability of differantiate in means */ 

ttest  EDUC, by(dum), if CENSUS==70 & YOB >= 1920 & YOB <= 1929/* Checking statistical reliability of differantiate in means */ 

/* Wald Estimate */ 

reg LWKLYWGE i.dum if CENSUS==70 & YOB >= 1920 & YOB <= 1929
mat list e(b)
scalar b_w1 = e(b)[1,2]
di "Log_W_Wage: ", b_w
reg EDUC i.dum if CENSUS==70 & YOB >= 1920 & YOB <= 1929 
mat list e(b)
scalar b_e1 = e(b)[1,2]
di "Education: ", b_e
generate p1 = b_w/b_e
display p1 /* Show the wald estimate (*) (Not including standard errors) */

reg LWKLYWGE EDUC if CENSUS==70 & YOB >= 1920 & YOB <= 1929 /* OLS Regression for the effect of education on weekly wage */


/* 1930-1939 [CENSUS 1980] */

ttest  LWKLYWGE, by(dum), if CENSUS==80 & YOB >= 30 & YOB <= 39 /* Checking statistical reliability of differantiate in means */ 

ttest  EDUC, by(dum), if CENSUS==80 & YOB >= 30 & YOB <= 39/* Checking statistical reliability of differantiate in means */ 

/* Wald Estimate */

reg LWKLYWGE i.dum if CENSUS==80 & YOB >= 30 & YOB <= 39
mat list e(b)
scalar b_w2 = e(b)[1,2]
di "Log_W_Wage: ", b_w
reg EDUC i.dum if CENSUS==80 & YOB >= 30 & YOB <= 39 
mat list e(b)
scalar b_e2 = e(b)[1,2]
di "Education: ", b_e
generate p2 = b_w/b_e
display p2 /* Show the wald estimate (*) (Not including standard errors) */

reg LWKLYWGE EDUC if CENSUS==80 & YOB >= 30 & YOB <= 39 /* OLS Regression for the effect of education on weekly wage */


/* ******************************** */


/*b*/

/* 1920-1929 [CENSUS 1970] */

/* TSL Estimate With Coding */

ivregress 2sls LWKLYWGE (EDUC= i.dum) if CENSUS==70 & YOB >= 1920 & YOB <= 1929


/* OLS Regression */

reg LWKLYWGE EDUC if CENSUS==70 & YOB >= 1920 & YOB <= 1929 






/*Angrist and Krueger (1991)*/

use "C:\Users\Bilgisayar\Documents\YH-ödev1\2020-fall\data\raw\NEW7080.dta" /* Transfer the data */

rename (v1 v2 v4 v5 v6 v9 v10 v11 v12 v13 v16 v18 v19 v20 v21 v24 v25 v27) ///
(AGE AGEQ EDUC ENOCENT ESOCENT LWKLYWGE MARRIED MIDATL MT NEWENG CENSUS QOB RACE SMSA SOATL WNOCENT WSOCENT YOB)

/*a*/
/* Creating quarter of birth dummy variables in OLS Model by depending on dummy variable [n-1] rule  */
reg EDUC b4.QOB if CENSUS==80 & YOB >= 30 & YOB <= 39 /* Estimating the effect of quarter of birth on education and create n-1 dummy variable */
eststo model1 
esttab model1, se

/* Creating quarter of birth dummy variables manuel by depending on dummy variable [n-1] rule  */
gen dum = (QOB==1)  /* Generating binary dummy variable for born in the first quarter of year */
gen dum1 = (QOB==2) /* Generating binary dummy variable for born in the second quarter of year */
gen dum2 = (QOB==3) /* Generating binary dummy variable for born in the third quarter of year */

/* 1920-1929 [CENSUS 1970] */

estpost ttest  LWKLYWGE, by(dum), if CENSUS==70 & YOB >= 1920 & YOB <= 1929 /* Checking statistical reliability of differantiate in means */
esttab ., se 

estpost ttest  EDUC, by(dum), if CENSUS==70 & YOB >= 1920 & YOB <= 1929/* Checking statistical reliability of differantiate in means */ 
esttab .,se

/* Wald Estimate With 2LSL Model */ 
ivregress 2sls LWKLYWGE (EDUC= i.dum) if CENSUS==70 & YOB >= 1920 & YOB <= 1929
eststo model2
esttab model2, se

/* Wald Estimate Manuel (Not including standard errors) */ 
reg LWKLYWGE i.dum if CENSUS==70 & YOB >= 1920 & YOB <= 1929
mat list e(b)
scalar b_w1 = e(b)[1,2]
di "Log_W_Wage: ", b_w
reg EDUC i.dum if CENSUS==70 & YOB >= 1920 & YOB <= 1929 
mat list e(b)
scalar b_e1 = e(b)[1,2]
di "Education: ", b_e
generate p1 = b_w/b_e
display p1 /* Show the wald estimate */

reg LWKLYWGE EDUC if CENSUS==70 & YOB >= 1920 & YOB <= 1929 /* OLS Regression for the effect of education on weekly wage */
eststo model3
esttab model3, se


/* 1930-1939 [CENSUS 1980] */

estpost ttest  LWKLYWGE, by(dum), if CENSUS==80 & YOB >= 30 & YOB <= 39 /* Checking statistical reliability of differantiate in means */
esttab ., se  

estpost ttest  EDUC, by(dum), if CENSUS==80 & YOB >= 30 & YOB <= 39/* Checking statistical reliability of differantiate in means */ 
esttab ., se 

/* Wald Estimate With 2LSL Model */ 
ivregress 2sls LWKLYWGE (EDUC= dum) if CENSUS==80 & YOB >= 30 & YOB <= 39
eststo model4
esttab model4, se

/* Wald Estimate Manuel (Not including standard errors) */

reg LWKLYWGE dum if CENSUS==80 & YOB >= 30 & YOB <= 39
mat list e(b)
scalar b_w2 = e(b)[1,2]
di "Log_W_Wage: ", b_w
reg EDUC i.dum if CENSUS==80 & YOB >= 30 & YOB <= 39 
mat list e(b)
scalar b_e2 = e(b)[1,2]
di "Education: ", b_e
generate p2 = b_w/b_e
display p2 /* Show the wald estimate */

reg LWKLYWGE EDUC if CENSUS==80 & YOB >= 30 & YOB <= 39 /* OLS Regression for the effect of education on weekly wage */
eststo model5
esttab model5, se

/* ******************************** */


/*b*/


/* 1920-1929 [CENSUS 1970] */

/* TSL Estimate With Coding (No Control Variables) */

ivregress 2sls LWKLYWGE (EDUC= dum) if CENSUS==70 & YOB >= 1920 & YOB <= 1929
eststo model6
esttab model6, se

/* OLS Regression (No Control Variables)*/

reg LWKLYWGE EDUC if CENSUS==70 & YOB >= 1920 & YOB <= 1929
eststo model7,
esttab model7, se

/* TSL Estimate With Coding (Including Control Variables) */

/* All Control Variables Except YOB*/

ivregress 2sls LWKLYWGE RACE ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT AGE /// 
 (EDUC= dum) if CENSUS==70 & YOB >= 1920 & YOB <= 1929
eststo model8
esttab model8, se 

/* With 9 YOB Dummies and Regions */

ivregress 2sls LWKLYWGE i.YOB ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT ///
(EDUC= dum) 
 if CENSUS==70 & YOB >= 1920 & YOB <= 1929
eststo model9
esttab model9, se 


/* OLS Regression */

/*All Control Variables*/
reg LWKLYWGE EDUC RACE i.YOB ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT AGE ///
 if CENSUS==70 & YOB >= 1920 & YOB <= 1929
eststo model10
esttab model10, se

/* With 9 YOB Dummies and Regions */

reg LWKLYWGE EDUC i.YOB ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT ///
 if CENSUS==70 & YOB >= 1920 & YOB <= 1929
eststo model11a
esttab model11a, se

/* *************************************************************************************************************** */

/* 1930-1939 [CENSUS 1980] */

/* TSL Estimate With Coding (No Control Variables) */

ivregress 2sls LWKLYWGE (EDUC= dum) if CENSUS==80 & YOB >= 30 & YOB <= 39
eststo model12
esttab model12, se

/* OLS Regression (No Control Variables)*/

reg LWKLYWGE EDUC if CENSUS==80 & YOB >= 30 & YOB <= 39 
eststo model13,
esttab model14, se 

/* TSL Estimate With Coding (Including Control Variables) */

/* All Control Variables Except YOB*/

ivregress 2sls LWKLYWGE RACE ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT AGE /// 
 (EDUC= dum) if CENSUS==80 & YOB >= 30 & YOB <= 39
eststo model15
esttab model15, se 

/* With 9 YOB Dummies and Regions */

ivregress 2sls LWKLYWGE i.YOB ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT (EDUC= dum) ///
 if CENSUS==80 & YOB >= 30 & YOB <= 39
eststo model16
esttab model16, se 

/* OLS Regression */

/*All Control Variables*/
reg LWKLYWGE EDUC RACE i.YOB ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT AGE ///
 if CENSUS==80 & YOB >= 30 & YOB <= 39
eststo model17
esttab model17, se
    
/* With 9 YOB Dummies and Regions */

reg LWKLYWGE EDUC i.YOB ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT ///
 if CENSUS==80 & YOB >= 30 & YOB <= 39
eststo model18a
esttab model18a, se


/* *************************************************************************************************************** */

/* Weakly Instrumental Problem Control */

reg EDUC dum if CENSUS==70 & YOB >= 1920 & YOB <= 1929
eststo model19
esttab model19, se

reg EDUC dum RACE i.YOB ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT AGE if CENSUS==70 & YOB >= 1920 & YOB <= 1929
eststo model20
esttab model20, se

reg EDUC dum if CENSUS==80 & YOB >= 30 & YOB <= 39
eststo model21
esttab model21, se

reg EDUC dum RACE i.YOB ENOCENT ESOCENT MIDATL MT NEWENG SOATL WNOCENT WSOCENT AGE if CENSUS==80 & YOB >= 30 & YOB <= 39
eststo model22
esttab model22, se

/* Graph (Not Completed)"Not Drawn Line"*/

gen q = "q"
gen m = 19

egen concatenate QOB2 = concat(q QOB)
egen concatenate YEAR2 = concat(m YOB)
egen concatenate DATE2 = concat(YEAR2 QOB2)

gen YEARQ = quarterly(DATE2, "YQ")
format YEARQ %tq

egen EDUCMEAN1 = mean(EDUC), by(QOB YEARQ), if CENSUS==80 & YOB >= 30 & YOB <= 39

separate EDUCMEAN, by(QOB) veryshortlabel
twoway (scatter EDUCMEAN? YEARQ,  mlabel(QOB)) if YOB >= 30 & YOB <= 39








































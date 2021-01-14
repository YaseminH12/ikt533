/* Ceritoglu vd. (2017) "The Impact of Syrian refugees on natives' labor market outcomes in Turkey" */

use "C:\Users\Bilgisayar\Documents\YH-ödev1\2020-fall\data\clean\n_pop.dta" /* Transfer the data */

/*Creating the interaction term*/
gen int_term = (tre_st==1 & are_st==1)
gen gender = (S3==1)


/*Informal employment-to-population ratio*/
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2, robust 
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log, robust
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if gender==1, robust
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if gender == 0, robust
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if low_educ == 1, robust
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if high_educ == 1,robust

esttab , se r2 ar2 keep(int_term _cons )
eststo clear

/*Labor force participation*/
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2, robust  
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log, robust 
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if gender==1, robust  
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if gender == 0, robust 
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if low_educ == 1, robust 
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if high_educ == 1, robust 

esttab , se  r2 ar2 keep(int_term _cons )
eststo clear

/*Unemployment-to-population ratio*/
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2, robust  
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log, robust 
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if gender==1, robust 
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if gender==0, robust 
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if low_educ == 1, robust 
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if high_educ == 1, robust 

esttab , se  r2 ar2 keep(int_term _cons )
eststo clear

/*Formal employment-to-population ratio*/
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2, robust  
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log, robust  
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if gender == 1, robust 
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if gender == 0, robust 
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if low_educ == 1, robust 
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if high_educ == 1, robust 

esttab , se  r2 ar2 keep(int_term _cons )
eststo clear

/*Job separation probobility*/
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 if S97<3, robust 
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3, robust  
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3 & gender==1, robust 
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3 & gender==0, robust 
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3 & low_educ == 1, robust 
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3 & high_educ == 1, robust 

esttab , se  r2 ar2 keep(int_term _cons )
eststo clear

/*Job finding probobility*/
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 if S97==4, robust  
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4, robust  
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4 & gender==1, robust 
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4 & gender==0, robust 
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4 & low_educ == 1, robust 
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4 & high_educ == 1, robust 

esttab , se  r2 ar2 keep(int_term _cons )
eststo clear

/*Formal real monthly earnings*/
eststo : reg reel_wage int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st full_time S37A i.S33KOD i.year i.NUTS2 if for_emp==1 & S39==1, robust  
eststo : reg reel_wage int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st full_time S37A i.year i.NUTS2 trade_log if for_emp==1 & S39==1, robust  
eststo : reg reel_wage int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st full_time S37A i.year i.NUTS2 trade_log if for_emp==1 & S39==1 & gender==1, robust 
eststo : reg reel_wage int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st full_time S37A i.year i.NUTS2 trade_log if for_emp==1 & S39==1 & gender==0, robust 
eststo : reg reel_wage int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st full_time S37A i.year i.NUTS2 trade_log if for_emp==1 & S39==1 & low_educ==1, robust 
eststo : reg reel_wage int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st full_time S37A i.year i.NUTS2 trade_log if for_emp==1 & S39==1 & high_educ==1, robust 

esttab , se  r2 ar2 keep(int_term _cons )
eststo clear


/***************************************************************************************************************************************************/
/*Refugee effects by age groups*/
/*Labor force participation*/
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>14 & S6<25, robust
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>24 & S6<35, robust
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>34 & S6<45, robust
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>44 & S6<55, robust
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>55 & S6<64, robust
eststo : reg lab_force int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log, robust 
esttab , se  keep(int_term )
eststo clear
/*Unemployment-to-population*/
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>14 & S6<25, robust
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>24 & S6<35, robust
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>34 & S6<45, robust
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>44 & S6<55, robust
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>55 & S6<64, robust
eststo : reg un_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log, robust 
esttab , se  keep(int_term )
eststo clear
/*Informal employment-to-population*/
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>14 & S6<25, robust
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>24 & S6<35, robust
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>34 & S6<45, robust
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>44 & S6<55, robust
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>55 & S6<64, robust
eststo : reg inf_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log, robust 
esttab , se  keep(int_term )
eststo clear
/*Formal employment-to-population*/
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>14 & S6<25, robust
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>24 & S6<35, robust
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>34 & S6<45, robust
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>44 & S6<55, robust
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S6>55 & S6<64, robust
eststo : reg for_emp int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log, robust 
esttab , se  keep(int_term )
eststo clear
/*Job Separation Probability*/
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3 & S6>14 & S6<25, robust
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3 & S6>24 & S6<35, robust
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3 & S6>34 & S6<45, robust
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3 & S6>44 & S6<55, robust
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3 & S6>55 & S6<64, robust
eststo : reg job_sep int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97<3, robust 
esttab , se  keep(int_term )
eststo clear
/*Job Finding Probability*/
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4 & S6>14 & S6<25, robust
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4 & S6>24 & S6<35, robust
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4 & S6>34 & S6<45, robust
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4 & S6>44 & S6<55, robust
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4 & S6>55 & S6<64, robust
eststo : reg job_find int_term gender i.S6_GRUP marital high_educ i.S6_GRUP#high_educ urb_st i.year i.NUTS2 trade_log if S97==4, robust 
esttab , se  keep(int_term )
eststo clear





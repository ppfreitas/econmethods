set more off
************************************************************
******************     Cheating paper    *******************
gl datcheating "C:\cheatingpaper\rodimiro_analysis\final_analysis_AEJ"
gl logs "C:\cheatingpaper\rodimiro_analysis\final_analysis_AEJ"
gl dos "C:\Users\Rodimiro\Documents\Educación\Cheating\base_cheating\do"
************************************************************

log using "$logs\cheatfinal", text replace

************************************************************
*******        Cheating tables - panel data         ********
************************************************************

use "$datcheating\base_final_AEJ_publication.dta"

*** DEFINE WHO IS IN THE SAMPLE THE 3 YEARS AND HAS ALI SCORES IN ALL THREE YEARS ***
gen N=1 if ali_score~=.
egen n=total(N), by (folio)
gen attrit=0 if anio==2008 
replace attrit=1 if anio==2008 & n~=3

egen mean_sc=mean(mat_score), by(semester_2 attrit) 
egen sd_sc=sd(mat_score), by(semester_2 attrit)
gen std_mat_score=(mat_score-mean_sc)/sd_sc
bysort semester_2: sum std_mat_score

egen mean_ali=mean(ali_score), by(semester_2 attrit) 
egen sd_ali=sd(ali_score), by(semester_2 attrit)
gen std_ali_score=(ali_score-mean_ali)/sd_ali
bysort semester_2: sum std_ali_score

***CLEANING AND REDUCE CATEGORIES OF CATEGORICAL VARIABLES FROM STUDENT CONTEXT QUESTIONNAIRES***

global x "rooms fam_income age studies_mother studies_father fam_members pc_attit pc_fail pc_cap num_books scholarship fre_friend floor_mat internet"
foreach x in $x  {
replace `x'=. if `x'==-9
}
replace fam_income=3 if fam_income==4 | fam_income==5
gen age2=age^2
gen female=1 if student_gender==1
replace female=0 if student_gender==0
replace studies_mother=0 if studies_mother==1
replace studies_mother=1 if studies_mother==2
replace studies_mother=2 if studies_mother==3 | studies_mother==4
replace studies_mother=3 if studies_mother==5 | studies_mother==6
replace studies_father=0 if studies_father==1
replace studies_father=1 if studies_father==2
replace studies_father=2 if studies_father==3 | studies_father==4
replace studies_father=3 if studies_father==5 | studies_father==6

replace fam_members=. if fam_members==99 | fam_members>30

global x "pc_attit pc_fail pc_cap fre_friend"
foreach x in $x  {
replace `x'=0 if `x'==1 | `x'==2
replace `x'=1 if `x'==3 | `x'==4
}
replace num_books=0 if num_books==1 | num_books==2
replace num_books=1 if num_books==3 | num_books==4
replace num_books=2 if num_books==5 | num_books==6 | num_books==7
replace floor_mat=0 if floor_mat==1
replace floor_mat=1 if floor_mat==2
replace floor_mat=2 if floor_mat==3
gen mat_score2=mat_score^2
replace age= age + 12

bysort folio: egen everch=max(W00001) 

gen inter_t1_score= treat1 * std_mat_score
gen inter_t2_score= treat2 * std_mat_score
gen inter_t3_score= treat3 * std_mat_score

gen alltreat=0
replace alltreat=1 if treat1==1 | treat2==1 | treat3==1
gen t1=0 if treatment=="C"
replace t1=1 if treat1==1
gen t2=0 if treatment=="C"
replace t2=1 if treat2==1
gen t3=0 if treatment=="C"
replace t3=1 if treat3==1 

**********     Table2 descriptive analysis     ***********

xi: sum  i.fam_income i.studies_m i.studies_f i.num_books if semester_2==2 & attrit==0

gen fam_income1=_Ifam_incom_1
gen fam_income2=_Ifam_incom_2
gen fam_income3=_Ifam_incom_3
gen num_books1=_Inum_books_1
gen num_books2=_Inum_books_2
gen mom_stud1=_Istudies_m_1 
gen mom_stud2=_Istudies_m_2 
gen mom_stud3=_Istudies_m_3 
gen fa_stud1=_Istudies_f_1 
gen fa_stud2=_Istudies_f_2 
gen fa_stud3=_Istudies_f_3 

xi: sum  $child_background std_mat_score std_ali_score scholarship fam_members fam_income1 fam_income2 fam_income3 internet num_books1 num_books2 mom_stud1 mom_stud2 mom_stud3 fa_stud1 fa_stud2 fa_stud3 if semester_2==2 & attrit==0
xi: sum  $child_background std_mat_score std_ali_score scholarship fam_members fam_income1 fam_income2 fam_income3 internet num_books1 num_books2 mom_stud1 mom_stud2 mom_stud3 fa_stud1 fa_stud2 fa_stud3 if semester_2==2 & everch==1 & attrit==0
xi: sum  $child_background  std_mat_score std_ali_score scholarship fam_members fam_income1 fam_income2 fam_income3 internet num_books1 num_books2 mom_stud1 mom_stud2 mom_stud3 fa_stud1 fa_stud2 fa_stud3 if semester_2==2 & everch==0 & attrit==0


global x "$child_background ali_stdscore scholarship fam_members fam_income1 fam_income2 fam_income3 internet num_books1 num_books2 mom_stud1 mom_stud2 mom_stud3 fa_stud1 fa_stud2 fa_stud3"
foreach x in $x  {
gen ch_`x'= `x' if semester_2==2 & everch==1
gen nch_`x'= `x' if semester_2==2 & everch==0
ttest ch_`x' == nch_`x' if attrit==0, unp
}


***********     missing corrections     ************
global x "age age2 female fam_income internet fam_members num_books studies_mother studies_father pc_attit pc_fail pc_cap fre_friend scholarship std_mat_score std_ali_score inter_t1_score inter_t2_score inter_t3_score"
foreach x in $x  {
gen miss_`x'=0
replace miss_`x'=1 if `x'==.
replace `x'=0 if `x'==.
}

global x "MatriculaTotal TotalpersonalDocente LaboratoriosTotal reprobaron1a5Total "
foreach x in $x  {
gen missing_`x'=0
replace missing_`x'=1 if `x'==.
replace `x'=0 if `x'==.
}

global treat "treat1 treat2 treat3"
global child_background "age age2 female pc_attit pc_fail pc_cap fre_friend std_mat_score "
global hh_char "fam_members i.fam_income internet i.num_books i.studies_mother i.studies_father scholarship" 
global school "MatriculaTotal TotalpersonalDocente LaboratoriosTotal reprobaron1a5Total"
global miss_child "miss_std_mat_score miss_age miss_female miss_pc_attit miss_pc_fail miss_pc_cap miss_fre_friend"
global miss_hh "miss_fam_income miss_internet miss_num_books miss_studies_mother miss_studies_father  miss_scholarship"
global inter "inter_t1_score inter_t2_score inter_t3_score"

 
********  Appendix table 4 - attrition ********
xi: logit attrit $treat , cluster(clavecct)
margins, dydx(*) post
outreg2  using "$datcheating/ATable4.out", se bdec(3) coefastr replace keep($treat)

xi: logit attrit $treat $child_background $miss_child, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable4.out", se bdec(3) coefastr append keep($treat)

xi: logit attrit $treat $child_background $miss_child $hh_char $miss_hh , cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable4.out", se bdec(3) coefastr append keep($treat)

xi: logit attrit $treat $child_background $miss_child $hh_char $miss_hh $school, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable4.out", se bdec(3) coefastr append keep($treat)

 
********now delete attritors*****
keep if n==3
sort folio clavecct anio
tab anio treatment
tab semester_2 treatment


************       Table1 (panel group)     **************
bysort semester_2 treatment: sum W00001

******* Differences between treatment groups *******
global x "2 4 6"
global y "C T1 T2 T3"
foreach x in $x  {
foreach y in $y  {
egen shch`x'`y'=mean(W00001) if semester_2==`x' & treatment=="`y'", by(clavecct)
}
sum shch`x'C shch`x'T1 shch`x'T2 shch`x'T3
ttest shch`x'C == shch`x'T1, unp
ttest shch`x'C == shch`x'T2, unp
ttest shch`x'C == shch`x'T3, unp

ttest shch`x'T1 == shch`x'T2, unp
ttest shch`x'T1 == shch`x'T3, unp
ttest shch`x'T3 == shch`x'T2, unp
}

******* All students in sample *******
global y "C T1 T2 T3"
foreach y in $y  {
egen shch_3y`y'=mean(W00001) if treatment=="`y'", by(clavecct)
}
sum shch_3yC shch_3yT1 shch_3yT2 shch_3yT3
ttest shch_3yC == shch_3yT1, unp
ttest shch_3yC == shch_3yT2, unp
ttest shch_3yC == shch_3yT3, unp

ttest shch_3yT1 == shch_3yT2, unp
ttest shch_3yT1 == shch_3yT3, unp
ttest shch_3yT3 == shch_3yT2, unp

******* Differences between years *******

ttest shch2C == shch4C, unp
ttest shch2T1 == shch4T1, unp
ttest shch2T2 == shch4T2, unp
ttest shch2T3 == shch4T3, unp

ttest shch2C == shch6C, unp
ttest shch2T1 == shch6T1, unp
ttest shch2T2 == shch6T2, unp
ttest shch2T3 == shch6T3, unp

ttest shch4C == shch6C, unp
ttest shch4T1 == shch6T1, unp
ttest shch4T2 == shch6T2, unp
ttest shch4T3 == shch6T3, unp


**************     Table3 logit with treatment dummies     *********************
********    normalized math score (0,1)   *********

xi: logit W00001 $treat if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr replace keep($treat)

xi: logit W00001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr append keep($treat)

xi: logit W00001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: logit W00001 $treat if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr append keep($treat)

xi: logit W00001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr append keep($treat)

xi: logit W00001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: logit W00001 $treat  if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr append keep($treat )

xi: logit W00001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr append keep($treat)

xi: logit W00001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: regress omega_final $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table3.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

************   Appendix Table 6   *************


xi: probit W00001 $treat if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable6.out", se bdec(3) coefastr replace keep($treat)

xi: probit W00001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable6.out", se bdec(3) coefastr append keep($treat)

xi: probit W00001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/aTable6.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: probit W00001 $treat if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2  using "$datcheating/ATable6.out", se bdec(3) coefastr append keep($treat)

xi: probit W00001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable6.out", se bdec(3) coefastr append keep($treat)

xi: probit W00001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable6.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: probit W00001 $treat  if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable6.out", se bdec(3) coefastr append  keep($treat)

xi: probit W00001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable6.out", se bdec(3) coefastr append keep($treat)

xi: probit W00001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable6.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)



************   Appendix Table5 linear probability model with treatment dummies + interactions   *************
xi: reg W00001 $treat if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2  using "$datcheating/ATable5.out", se bdec(3) coefastr replace keep($treat)

xi: reg W00001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable5.out", se bdec(3) coefastr append keep($treat)

xi: reg W00001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/aTable5.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: reg W00001 $treat if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable5.out", se bdec(3) coefastr append keep($treat)

xi: reg W00001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable5.out", se bdec(3) coefastr append keep($treat)

xi: reg W00001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable5.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: reg W00001 $treat  if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable5.out", se bdec(3) coefastr append keep($treat)

xi: reg W00001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable5.out", se bdec(3) coefastr append keep($treat)

xi: reg W00001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable5.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)



****VARYING LEVEL OF ALPHA AND REPLICATING TABLE 3****

***Appendix Table 7 ***
****W0001****
xi: logit W0001 $treat if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable7.out", se bdec(3) coefastr replace keep($treat)

xi: logit W0001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable7.out", se bdec(3) coefastr append keep($treat)

xi: logit W0001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable7.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: logit W0001 $treat if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable7.out", se bdec(3) coefastr append keep($treat)

xi: logit W0001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable7.out", se bdec(3) coefastr append keep($treat)

xi: logit W0001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable7.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: logit W0001 $treat  if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable7.out", se bdec(3) coefastr append keep($treat)

xi: logit W0001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable7.out", se bdec(3) coefastr append keep($treat)

xi: logit W0001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable7.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)


***Appendix Table 8***
****W001***
xi: logit W001 $treat if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable8.out", se bdec(3) coefastr replace keep($treat)

xi: logit W001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable8.out", se bdec(3) coefastr append keep($treat)

xi: logit W001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable8.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: logit W001 $treat if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable8.out", se bdec(3) coefastr append keep($treat)

xi: logit W001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable8.out", se bdec(3) coefastr append keep($treat)

xi: logit W001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable8.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

xi: logit W001 $treat  if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable8.out", se bdec(3) coefastr append keep($treat)

xi: logit W001 $treat $child_background $miss_child $hh_char $miss_hh if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable8.out", se bdec(3) coefastr append keep($treat)

xi: logit W001 $treat $child_background $miss_child $hh_char $miss_hh $inter if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable8.out", se bdec(3) coefastr append keep($treat std_mat_score $inter)

***construct incentive payment;

*** REFEREE COMMENT- estimate incentive for control group and assume cheaters stay in their same category, any improvement is due to cheating;
gen payment=0 if ali_level==1

replace payment=0 if ali_level==2 & (mat_level==3 | mat_level==4) & anio==2008
replace payment=0 if ali_level==2 & (mat_level==2 | mat_level==3 | mat_level==4) & anio==2009
replace payment=4000 if ali_level==2 & mat_level==1 & (anio==2008 | anio==2009)
replace payment=2500 if ali_level==2 & mat_level==2 & anio==2008

replace payment=9000 if ali_level==3 & mat_level==1 & (anio==2008 | anio==2009)
replace payment=7500 if ali_level==3 & mat_level==2 & (anio==2008 | anio==2009)
replace payment=6000 if ali_level==3 & mat_level==3 & (anio==2008 | anio==2009)
replace payment=4500 if ali_level==3 & mat_level==4 & (anio==2008 | anio==2009)

replace payment=15000 if ali_level==4 & mat_level==1 & (anio==2008 | anio==2009)
replace payment=13500 if ali_level==4 & mat_level==2 & (anio==2008 | anio==2009)
replace payment=12000 if ali_level==4 & mat_level==3 & (anio==2008 | anio==2009)
replace payment=10500 if ali_level==4 & mat_level==4 & (anio==2008 | anio==2009)

replace payment=0 if ali_level==2 & anio==2010
replace payment=5000 if ali_level==3 & anio==2010
replace payment=10000 if ali_level==4 & anio==2010

sort anio treatment W0001
by anio treatment W0001:  sum payment


********  Appendix table 3 logit (treatment status on background characteristics)  ********
xi: logit alltreat $child_background $miss_child $hh_char $miss_hh $school if semester_2==2, cluster(clavecct)  
margins, dydx(*) post
outreg2 using "$datcheating/ATable3.out", se bdec(3) coefastr replace keep($child_background $hh_char $school)

xi: logit t1 $child_background $miss_child $hh_char $miss_hh $school if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable3.out", se bdec(3) coefastr append keep($child_background $hh_char $school)

xi: logit t2 $child_background $miss_child $hh_char $miss_hh $school if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable3.out", se bdec(3) coefastr append keep($child_background $hh_char $school)

xi: logit t3 $child_background $miss_child $hh_char $miss_hh $school if semester_2==2, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable3.out", se bdec(3) coefastr append keep($child_background $hh_char $school)


**************     Table4 logit -     ***************
gen cheater2 = (W00001==1 & anio==2008)
bysort folio: egen cheat2=max(cheater2) 
gen ch2_treat1 = cheat2*treat1
gen ch2_treat2 = cheat2*treat2
gen ch2_treat3 = cheat2*treat3

gen cheater4 = (W00001==1 & anio==2009)
bysort folio: egen cheat4=max(cheater4) 
gen ch4_treat1 = cheat4*treat1
gen ch4_treat2 = cheat4*treat2
gen ch4_treat3 = cheat4*treat3

xi: logit W00001 $treat cheat2 ch2_treat1 ch2_treat2 ch2_treat3 $child_background $miss_child $hh_char $miss_hh  if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table4.out", se bdec(3) coefastr replace keep($treat cheat2 ch2_treat1 ch2_treat2 ch2_treat3 )

xi: logit W00001 $treat cheat4 ch4_treat1 ch4_treat2 ch4_treat3 $child_background $miss_child $hh_char $miss_hh  if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table4.out", se bdec(3) coefastr append keep($treat cheat4 ch4_treat1 ch4_treat2 ch4_treat3 )

xi: regress omega_final $treat cheat4 ch4_treat1 ch4_treat2 ch4_treat3 $child_background $miss_child $hh_char $miss_hh if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/Table4.out", se bdec(3) coefastr append keep($treat cheat4 ch4_treat1 ch4_treat2 ch4_treat3 )


*****Appendix table 9****
xi: reg W00001 $treat cheat2 ch2_treat1 ch2_treat2 ch2_treat3 $child_background $miss_child $hh_char $miss_hh if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable9.out", se bdec(3) coefastr replace keep($treat cheat2 ch2_treat1 ch2_treat2 ch2_treat3 )

xi: reg W00001 $treat cheat4 ch4_treat1 ch4_treat2 ch4_treat3 $child_background $miss_child $hh_char $miss_hh if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable9.out", se bdec(3) coefastr append keep($treat cheat4 ch4_treat1 ch4_treat2 ch4_treat3 )

xi: probit W00001 $treat cheat2 ch2_treat1 ch2_treat2 ch2_treat3 $child_background $miss_child $hh_char $miss_hh if semester_2==4, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable9.out", se bdec(3) coefastr append keep($treat cheat2 ch2_treat1 ch2_treat2 ch2_treat3 )

xi: probit W00001 $treat cheat4 ch4_treat1 ch4_treat2 ch4_treat3 $child_background $miss_child $hh_char $miss_hh if semester_2==6, cluster(clavecct)
margins, dydx(*) post
outreg2 using "$datcheating/ATable9.out", se bdec(3) coefastr append keep($treat cheat4 ch4_treat1 ch4_treat2 ch4_treat3 )


clear all





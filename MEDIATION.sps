* Encoding: UTF-8.
set mprint yes.
DEFINE mediation (data= !charend('/')/yvar = !charend('/')/avar = !charend('/')/mvar = !charend('/')/cvar=!charend('/')/NC=!charend('/')/a0=!charend('/')/a1=!charend('/')/m=!charend('/')/
 yreg=!charend('/')/mreg=!charend('/')/interaction=!charend('/')/boot=!charend('/') !default(FALSE)/nobs=!charend('/') !default(FALSE)/casecontrol=!charend('/') !default(FALSE)/Output=!charend('/') !default(REDUCED)/c=!charend('/') !default(NA)).

set mxloops = 10000.

Get file =!data.
PRINT.


!If (!interaction=TRUE) !Then.
compute Int=!avar*!mvar.
!IFEND.

!If (!boot~=FALSE) !Then.
 compute  id=$casenum.
 set seed = 1234.  
save outfile !path+ 'bootdata.sav'.
input program.
	!IF (!boot=TRUE) !Then.
compute #reps=1000.
	!IFEND.
	!IF (!boot~=TRUE) !Then.
compute #reps=!boot.
	!IFEND.
compute #ssize  =!nobs. /* sample size desired.
compute #psize = #ssize .  /* size of population .
loop samp=1 to #reps .
loop v = 1 to  #ssize  .
compute id=trunc(rv.uniform(1,#psize +1) ) .
end case.
leave samp.
end loop.
end loop.
end file.
end input program .
execute.
sort cases by id.
match files / file * / table !path+ 'bootdata.sav' / by id  .
sort cases by samp.
split file by samp .


*/OUTCOME REGRESSION*/.

 !If (!interaction=TRUE & !yreg=LINEAR & !NC~=0) !Then.
  REGRESSION 
   /DEPENDENT !yvar 
   /METHOD=ENTER !avar !mvar Int !cvar 
   / outfile= covb(!path+ 'out1.sav').

 !IFEND.

 !If (!interaction~=TRUE & !yreg=LINEAR & !NC~=0) !Then.
  REGRESSION 
   /DEPENDENT !yvar 
   /METHOD=ENTER !avar !mvar !cvar 
   / outfile= covb(!path+ 'out1.sav').

 !IFEND.

!If (!interaction=TRUE & !yreg=LINEAR & !NC=0) !Then.
  REGRESSION 
   /DEPENDENT !yvar 
   /METHOD=ENTER !avar !mvar Int  
   / outfile= covb(!path+ 'out1.sav').

!IFEND.

!If (!interaction=FALSE & !yreg=LINEAR & !NC=0) !Then.
  REGRESSION 
   /DEPENDENT !yvar 
   /METHOD=ENTER !avar !mvar
   / outfile= covb(!path+ 'out1.sav').

!IFEND.


!If (!interaction=TRUE & !yreg=LOGLINEAR & !NC~=0) !Then.
GENLIN
  !yvar (ORDER = DESCENDING)   WITH  !avar !mvar Int !cvar 
 /MODEL  !avar !mvar Int !cvar 
  INTERCEPT=YES
  DISTRIBUTION=BINOMIAL
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.

!IFEND.

!If (!interaction~=TRUE & !yreg=LOGLINEAR & !NC~=0) !Then.
 GENLIN
  !yvar (ORDER = DESCENDING)    WITH  !avar !mvar  !cvar 
 /MODEL  !avar !mvar !cvar 
  INTERCEPT=YES
  DISTRIBUTION=BINOMIAL
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.

!IFEND.

!If (!interaction=TRUE & !yreg=LOGLINEAR & !NC=0) !Then.
 GENLIN
  !yvar   (ORDER = DESCENDING)   WITH    !avar !mvar Int 
 /MODEL  !avar !mvar Int 
  INTERCEPT=YES
  DISTRIBUTION=BINOMIAL
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=FALSE & !yreg=LOGLINEAR & !NC=0) !Then.
 GENLIN
  !yvar  (ORDER = DESCENDING)    WITH  !avar !mvar
 /MODEL  !avar !mvar 
  INTERCEPT=YES
  DISTRIBUTION=BINOMIAL
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.


!If (!interaction=TRUE & !yreg=POISSON & !NC~=0) !Then.
GENLIN
  !yvar   WITH  !avar !mvar Int !cvar 
 /MODEL  !avar !mvar Int !cvar 
  INTERCEPT=YES
  DISTRIBUTION=POISSON
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=FALSE & !yreg=POISSON & !NC~=0) !Then.
 GENLIN
  !yvar   WITH  !avar !mvar !cvar 
 /MODEL  !avar !mvar !cvar 
  INTERCEPT=YES
  DISTRIBUTION=POISSON
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=TRUE & !yreg=POISSON & !NC=0) !Then.

GENLIN
  !yvar    WITH  !avar !mvar Int
 /MODEL  !avar !mvar Int 
  INTERCEPT=YES
  DISTRIBUTION=POISSON
  LINK=LOG
/OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=FALSE & !yreg=POISSON & !NC=0) !Then.
 GENLIN
  !yvar   WITH  !avar !mvar
 /MODEL  !avar !mvar 
  INTERCEPT=YES
  DISTRIBUTION=POISSON
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.


!If (!interaction=TRUE & !yreg=NEGBIN & !NC~=0) !Then.
GENLIN
  !yvar   WITH  !avar !mvar Int !cvar 
 /MODEL  !avar !mvar Int !cvar 
  INTERCEPT=YES
  DISTRIBUTION=NEGBIN (MLE)
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction~=TRUE & !yreg=NEGBIN & !NC~=0) !Then.
 GENLIN
  !yvar   WITH  !avar !mvar  !cvar 
 /MODEL  !avar !mvar !cvar 
  INTERCEPT=YES
  DISTRIBUTION=NEGBIN (MLE)
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=TRUE & !yreg=NEGBIN & !NC=0) !Then.
 GENLIN
  !yvar    WITH  !avar !mvar Int
 /MODEL  !avar !mvar Int 
  INTERCEPT=YES
  DISTRIBUTION=NEGBIN (MLE)
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=FALSE & !yreg=NEGBIN & !NC=0) !Then.
 GENLIN
  !yvar    WITH  !avar !mvar 
 /MODEL  !avar !mvar 
  INTERCEPT=YES
  DISTRIBUTION=NEGBIN (MLE)
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=TRUE & !yreg=LOGISTIC & !NC~=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
  outfile=!path+ 'out11.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
  outfile=!path+ 'corr1.sav'.
  LOGISTIC REGRESSION VARIABLES = !yvar With !avar, !mvar,Int,!cvar
  /PRINT=ALL.
OMSEND.
!IFEND.

!If (!interaction=FALSE & !yreg=LOGISTIC & !NC~=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out11.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
  outfile=!path+ 'corr1.sav'.
  LOGISTIC REGRESSION VARIABLES = !yvar With !avar, !mvar,!cvar
  /PRINT=ALL.
OMSEND.
!IFEND.

!If (!interaction=TRUE & !yreg=LOGISTIC & !NC=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out11.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr1.sav'.
  LOGISTIC REGRESSION VARIABLES = !yvar With !avar, !mvar,Int
/PRINT=ALL.
OMSEND.
!IFEND.

!If (!interaction=FALSE & !yreg=LOGISTIC & !NC=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out11.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr1.sav'.
  LOGISTIC REGRESSION VARIABLES = !yvar With !avar, !mvar
/PRINT=ALL.
OMSEND.
!IFEND.


*/MEDIATOR REGRESSION*/.
OMS
 /SELECT TABLES
 /if COMMAND= ["REGRESSION"]
    SUBTYPES= ["Model Summary"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'rmse.sav'.
!If ( !mreg=LINEAR & !NC~=0) !Then.
!If (!casecontrol=TRUE) !Then.
RECODE !yvar (1 = 0)(0 = 1).
FILTER BY !yvar.
!IFEND.
 REGRESSION 
   /DEPENDENT !mvar 
   /METHOD=ENTER !avar !cvar
   / outfile= covb(!path+ 'out2.sav').
OMSEND.
!If (!casecontrol=TRUE) !Then.
FILTER OFF.
RECODE !yvar (1 = 0)(0 = 1).
!IFEND.
!IFEND.




OMS
 /SELECT TABLES
 /if COMMAND= ["REGRESSION"]
    SUBTYPES= ["Model Summary"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'rmse.sav'.
!If ( !mreg=LINEAR & !NC=0) !Then.
!If (!casecontrol=TRUE) !Then.
RECODE !yvar (1 = 0)(0 = 1).
FILTER BY !yvar.
!IFEND.
 REGRESSION 
   /DEPENDENT !mvar 
   /METHOD=ENTER !avar 
   / outfile= covb(!path+ 'out2.sav').
OMSEND.
!If (!casecontrol=TRUE) !Then.
FILTER OFF.
RECODE !yvar (1 = 0)(0 = 1).
!IFEND.
!IFEND.

!If ( !mreg=LOGISTIC & !NC~=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out21.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr2.sav'.
!If (!casecontrol=TRUE) !Then.
RECODE !yvar (1 = 0)(0 = 1).
FILTER BY !yvar.
!IFEND.
LOGISTIC REGRESSION VARIABLES = !mvar With !avar, !cvar
/PRINT=ALL.
OMSEND.
!If (!casecontrol=TRUE) !Then.
FILTER OFF.
RECODE !yvar (1 = 0)(0 = 1).
!IFEND.
!IFEND.




!If ( !mreg=LOGISTIC & !NC=0) !Then.
!If (!casecontrol=TRUE) !Then.
RECODE !yvar (1 = 0)(0 = 1).
FILTER BY !yvar.
!IFEND.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out21.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr2.sav'.
LOGISTIC REGRESSION VARIABLES = !mvar With !avar
/PRINT=ALL.
OMSEND.
!If (!casecontrol=TRUE) !Then.
FILTER OFF.
RECODE !yvar (1 = 0)(0 = 1).
 !IFEND.
 !IFEND.

!IFEND.

OUTPUT CLOSE *.






!if (!boot~=FALSE) !then.
	!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
get file=!path+ 'out1.sav'.
save  outfile=!path+ 'out1.sav'
  /DROp samp RowType_ VarName_.
	!IFEND.
	!IF (!yreg=LINEAR) !then.
get file=!path+ 'out1.sav'.
save  outfile=!path+ 'out1.sav'
  /DROp samp.
	!IFEND.


	!IF (!mreg=LINEAR) !then.
get file=!path+ 'out2.sav'.
save  outfile=!path+ 'out2.sav'
 /DROp samp.
	!IFEND.
	!IF (!mreg=LOGISTIC) !then.
get file=!path+ 'out21.sav'.
save  outfile=!path+ 'out21.sav'
 /DROp Var1.
GET FILE= !path + "out21.sav".
SORT CASES BY Var2.
SELECT IF (Var2="Step 1").
save  outfile=!path+ 'out21.sav'.
 !ifend.
 !IF (!yreg=LOGISTIC) !then.
get file=!path+ 'out11.sav'.
save  outfile=!path+ 'out11.sav'
 /DROp Var1.
GET FILE= !path + "out11.sav".
SORT CASES BY Var2.
SELECT IF (Var2="Step 1").
save  outfile=!path+ 'out11.sav'.
!ifend.


	MATRIX.
	!IF (!boot=TRUE) !Then.
compute #reps=1000.
		!ifend.	
		!IF (!boot~=TRUE) !Then.
compute #reps=!boot.
		!ifend.
compute theta0=make(#reps,1,0).
compute theta1=make(#reps,1,0).
compute theta2=make(#reps,1,0). 
  !if (!interaction=TRUE) !then.
compute theta3=make(#reps,1,0).
  !ifend.
compute beta0=make(#reps,1,0).
compute beta1=make(#reps,1,0).
  !if (!NC~=0) !then.
compute beta2 =make(#reps,!NC,0).
compute meanc =make(1,!NC,0).
compute cval =make(1,!NC,0).
  !ifend.



		!If (!yreg=LINEAR | !yreg=POISSON  | !yreg=LOGLINEAR | !yreg=NEGBIN ) !Then.
			!If (!NC~=0) !Then.
				!If (!interaction=TRUE) !Then.
					!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
Get x1/file=!path+ 'out1.sav' /MISSING=0.
					!ifend.
					!IF (!yreg=LINEAR) !THEN.
Get x1/file=!path+ 'out1.sav'/VARIABLES=const_ !AVAR !MVAR INT !CVAR/MISSING=OMIT.
					!IFEND.
Loop j=1 To (#reps).
compute theta0(j,1)=X1((4+!NC+1)*j+3*(j-1),1).
compute theta1(j,1)=X1((4+!NC+1)*j+3*(j-1),2).
compute theta2(j,1)=X1((4+!NC+1)*j+3*(j-1),3).
compute theta3(j,1)=X1((4+!NC+1)*j+3*(j-1),4).
End Loop.
				!IFEND.
   

				!If (!interaction=FALSE) !Then.
					!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
Get x1/file=!path+ 'out1.sav'/MISSING=0.
					!ifend.
					!IF (!yreg=LINEAR) !THEN.
Get x1/file=!path+ 'out1.sav'/VARIABLES=const_ !AVAR !MVAR  !CVAR/MISSING=OMIT.
					!IFEND.
Loop j=1 To (#reps).
compute theta0(j,1)=X1((3+!NC+1)*j+3*(j-1),1).
compute theta1(j,1)=X1((3+!NC+1)*j+3*(j-1),2).
compute theta2(j,1)=X1((3+!NC+1)*j+3*(j-1),3).
End Loop.
				!IFEND.
			!IFEND.

			!If (!NC=0) !Then.
				!If (!interaction=TRUE) !Then.
					!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
Get x1/file=!path+ 'out1.sav' /VARIABLES=P1 P2 P3 P4/MISSING=OMIT.
					!ifend.
					!IF (!yreg=LINEAR) !then.
Get x1/file=!path+ 'out1.sav'/VARIABLES=const_ !AVAR !MVAR INT /MISSING=OMIT.
					!IFEND.
Loop j=1 To (#reps).
compute theta0(j,1)=X1((4+1)*j+3*(j-1),1).
compute theta1(j,1)=X1((4+1)*j+3*(j-1),2).
compute theta2(j,1)=X1((4+1)*j+3*(j-1),3).
compute theta3(j,1)=X1((4+1)*j+3*(j-1),4).
End Loop.
				!IFEND.
				!If (!interaction=FALSE) !Then.
					!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
Get x1/file=!path+ 'out1.sav'/VARIABLES=P1 P2 P3 /MISSING=OMIT.
					!ifend.
					!IF (!yreg=LINEAR) !then.
Get x1/file=!path+ 'out1.sav'/VARIABLES=const_ !AVAR !MVAR  /MISSING=OMIT.
					!ifend.
Loop j=1 To (#reps).
compute theta0(j,1)=X1((3+1)*j+3*(j-1),1).
compute theta1(j,1)=X1((3+1)*j+3*(j-1),2).
compute theta2(j,1)=X1((3+1)*j+3*(j-1),3).
End Loop.
				!IFEND.
			!IFEND.
		!IFEND.
	
		!If (!yreg=LOGISTIC ) !Then.
			!If (!NC~=0) !Then.
				!If (!interaction=TRUE) !Then.
Get x1/file=!path+ 'out11.sav'/VARIABLES=B/MISSING=OMIT.
Loop j=1 To (#reps).
compute theta1(j,1)=X1(1+(3+!NC+1)*(j-1),1).
compute theta2(j,1)=X1(2+(3+!NC+1)*(j-1),1).
compute theta3(j,1)=X1(3+(3+!NC+1)*(j-1),1).
*compute theta0(j,1)=X1((3+!NC+1)+(3+!NC+1)*(j-1),1).
End Loop.

				!ifend
				!If (!interaction=FALSE) !Then.
Get x1/file=!path+ 'out11.sav'/VARIABLES=B/MISSING=OMIT.
Loop j=1 To #reps.
compute theta1(j,1)=X1(1+(2+!NC+1)*(j-1),1).
compute theta2(j,1)=X1(2+(2+!NC+1)*(j-1),1).
*compute theta0(j,1)=X1((2+!NC+1)+(2+!NC+1)*(j-1),1).
End Loop.
				!IFEND.
			!IFEND.
			!If (!NC=0) !Then.
				!If (!interaction=TRUE) !Then.
Get x1/file=!path+ 'out11.sav'/VARIABLES=B/MISSING=OMIT.
Loop j=1 To #reps.
compute theta1(j,1)=X1(1+(3+1)*(j-1),1).
compute theta2(j,1)=X1(2+(3+1)*(j-1),1).
compute theta3(j,1)=X1(3+(3+1)*(j-1),1).
*compute theta0(j,1)=X1((3+1)+(3+1)*(j-1),1).
End Loop.
				!IFEND.
    !If (!interaction=FALSE) !Then.
Get x1/file=!path+ 'out11.sav'/VARIABLES=B /MISSING=OMIT.
Loop j=1 To #reps.
compute theta1(j,1)=X1(1+(2+1)*(j-1),1).
compute theta2(j,1)=X1(2+(2+1)*(j-1),1).
*compute theta0(j,1)=X1((2+1)+(2+1)*(j-1),1).
End Loop.
				!IFEND.
			!IFEND.
		!IFEND.


		!If (!mreg=LINEAR) !Then.
Get rmse/file=!path+ 'rmse.sav'/VARIABLES=Var1 R RSquare AdjustedRSquare Std.ErroroftheEstimate /MISSING=OMIT.
compute s2=make(#reps,1,0).
Loop j=1 to #reps.
compute s2(j,1)=rmse(j,5).
END LOOP.
			!If (!NC~=0) !Then.
Get x2/file=!path+ 'out2.sav'/VARIABLES=const_ !AVAR !CVAR/MISSING=OMIT.
Get datacov/file=!data/VARIABLES= !CVAR/MISSING=OMIT.
compute n=nrow(datacov).
LOOP j=1 to #reps.
compute beta0(j,1)=X2((2+!NC+1)*j+3*(j-1),1).
compute beta1(j,1)=X2((2+!NC+1)*j+3*(j-1),2).
Loop i=1 To (!NC).
compute beta2(j,i)=X2((2+!NC+1)*j+3*(j-1),(i+2)).
End Loop.
END LOOP.

Loop I=1TO (!NC).
compute meanc(1,I)=CSUM(datacov(:,I))/n.
End Loop.
				!IF (!c~=NA) !THEN.
Get c/file=!c/MISSING=OMIT.
compute cval(1,1:!NC)=c(1,:).
				!IFEND.
			!IFEND.
			!If (!NC=0) !Then.
Get x2/file=!path+ 'out2.sav'/VARIABLES=const_ !AVAR /MISSING=OMIT.
LOOP j=1 to #reps.
compute beta0(j,1)=X2((2+1)*j+3*(j-1),1).
compute beta1(j,1)=X2((2+1)*j+3*(j-1),2).
END LOOP.
			!IFEND.
		!IFEND.

		!If ( !mreg=LOGISTIC) !Then.
			!If (!NC~=0) !Then.
Get x2/file=!path+ 'out21.sav'/VARIABLES=B /MISSING=OMIT.

Loop j=1 To #reps.

compute beta1(j,1)=X2(1+(1+!NC+1)*(j-1),1).
compute beta0(j,1)=X2((1+!NC+1)+(1+!NC+1)*(j-1),1).
Loop i=1 To (!NC).
compute beta2(j,i)=X2((i+1)+(1+!NC+1)*(j-1),1).
End Loop.
End Loop.
Get datacov/file=!data/VARIABLES= !CVAR/MISSING=OMIT.
compute n=nrow(datacov).
Loop I=1TO (!NC).
compute meanc(1,I)=CSUM(datacov(:,I))/n.
End Loop.
				!IF (!c~=NA) !THEN.
Get c/file=!c/MISSING=OMIT.
compute cval(1,1:!NC)=c(1,:).
				!IFEND.
			!IFEND.

			!If (!NC=0) !Then.
Get x2/file=!path+ 'out21.sav'/VARIABLES=B/MISSING=OMIT.
Loop j=1 To #reps.
compute beta1(j,1)=X2(1+(1+1)*(j-1),1).
compute beta0(j,1)=X2((1+1)+(1+1)*(j-1),1).
End Loop.
			!IFEND.
		!IFEND.


compute effect=make(#reps,12,0).


loop j=1 to #reps.


*/Y LINEAR M LINEAR CASE*/.

!If (!interaction=TRUE & !yreg=LINEAR & !mreg=LINEAR & !NC~=0 ) !Then.
*/compute the effects*/.

!IF (!c~=NA) !THEN.
Loop i=1 To !NC.
*/CONDITIONAL CDE*/.
compute effect(j,1)=(theta1(j,1))*(!a1-!a0)+(theta3(j,1)*(!m))*(!a1-!a0).   
      */CONDITIONAL NDE*/.
compute effect(j,2)=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+csum(theta3(j,1)*beta2(j,i)*cval(1,i)))*(!a1-!a0).
      */CONDITIONAL NIE*/.
compute effect(j,3)=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a0)*(!a1-!a0).
	  */CONDITIONAL TNDE*/.
compute effect(j,4)=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a1+csum(theta3(j,1)*beta2(j,i)*cval(1,i)))*(!a1-!a0).
	  */ CONDITIONAL TNIE*/.
compute effect(j,5)=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).
End Loop.
!IFEND.
Loop i=1 To !NC.
      */MARGINAL CDE*/.
compute effect(j,6)=(theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0).
	  */MARGINAL NDE*/.
compute effect(j,7)=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+csum(theta3(j,1)*beta2(j,i)*meanc(1,i)))*(!a1-!a0).
	  */MARGINAL NIE*/.
compute effect(j,8)=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a0)*(!a1-!a0).
	  */ MARGINAL TNDE*/.
compute effect(j,9)=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a1+csum(theta3(j,1)*beta2(j,i)*meanc(1,i)))*(!a1-!a0).
	  */ MARGINAL TNIE*/.
compute effect(j,10)=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).
	  */ TE*/.
compute effect(j,11)=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+csum(theta3(j,1)*beta2(j,i)*meanc(1,i))+theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).
	End Loop.
!IF (!c~=NA) !THEN.
Loop i=1 To !NC.
compute effect(j,12)=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+csum(theta3(j,1)*beta2(j,i)*cval(1,i))+theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).
End Loop.
!IFEND.

!IFEND.



!If (!interaction=FALSE & !yreg=LINEAR & !mreg=LINEAR) !Then.
*/compute the effects*/.

*/CDE AND NDE*/.
compute effect(j,1)=(theta1(j,1))*(!a1-!a0).   
*/NIE*/.
compute effect(j,3)=(theta2(j,1)*beta1(j,1))*(!a1-!a0).
		*/ TE*/.
compute effect(j,11)=effect(j,1)+ effect(j,3).
	
!IFEND.




!If (!interaction=TRUE & !yreg=LINEAR & !mreg=LINEAR & !NC=0 ) !Then.
*/compute the effects*/.
*/  CDE*/.
compute effect(j,1)=(theta1(j,1))*(!a1-!a0)+(theta3(j,1)*(!m))*(!a1-!a0). 
      */  NDE*/.
compute effect(j,2)=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0)*(!a1-!a0).

      */  NIE*/.
compute effect(j,3)=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a0)*(!a1-!a0).
	  */  TNDE*/.
compute effect(j,4)=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).
	  */   TNIE*/.
compute effect(j,5)=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).
	  */ TE*/.
compute effect(j,11)=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).

!IFEND.




*/Y LINEAR M BINARY CASE*/.

!If (!interaction=TRUE & !yreg=LINEAR & !mreg=LOGISTIC & !NC~=0 ) !Then.
*/compute the effects*/.

!If (!c~=NA) !Then.
Loop i=1 To !NC.
*/CONDITIONAL CDE*/.
compute effect(j,1)=(theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0).
*/CONDITIONAL NDE*/.
compute effect(j,2)=(theta1(j,1)+theta3(j,1)*exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i))))/(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i))))))*(!a1-!a0).
*/CONDITIONAL NIE*/.
compute effect(j,3)=(theta2(j,1)+theta3(j,1)*!a0)*(
exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i)))))-
exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i)))))
).
*/CONDITIONAL TNDE*/.
compute effect(j,4)=(theta1(j,1)+theta3(j,1)*exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i))))/(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i))))))*(!a1-!a0).
*/ CONDITIONAL TNIE*/.
compute effect(j,5)=(theta2(j,1)+theta3(j,1)*!a1)*(
exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i)))))-
exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i)))))
).
*/conditional te*/.
compute effect(j,12)=(effect(j,2))+(effect(j,5)).
End Loop.
!IFEND.
Loop i=1 To !NC.
*/MARGINAL CDE*/.
compute effect(j,6)=(theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0).
*/MARGINAL NDE*/.
compute effect(j,7)=(theta1(j,1)+theta3(j,1)*exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i))))/(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i))))))*(!a1-!a0).
*/MARGINAL NIE*/.
compute effect(j,8)=(theta2(j,1)+theta3(j,1)*!a0)*(
exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i)))))-
exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i)))))
).
*/ MARGINAL TNDE*/.
compute effect(j,9)=(theta1(j,1)+theta3(j,1)*exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i))))/(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i))))))*(!a1-!a0).
*/ MARGINAL TNIE*/.
compute effect(j,10)=(theta2(j,1)+theta3(j,1)*!a1)*(exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i)))))-exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i))))/(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i)))))).
*/marginal te*/.
compute effect(j,11)=(effect(j,7))+(effect(j,10)).

End Loop.


!IFEND.







!If (!interaction=TRUE & !yreg=LINEAR & !mreg=LOGISTIC & !NC=0 ) !Then.
*/compute the effects*/.
*/ CDE*/.
compute effect(j,1)=(theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0).

*/ NDE*/.
compute effect(j,2)=(theta1(j,1)+theta3(j,1)*exp(beta0(j,1)+beta1(j,1)*!a0)/(1+exp(beta0(j,1)+beta1(j,1)*!a0)))*(!a1-!a0).
*/ NIE*/.
compute effect(j,3)=(theta2(j,1)+theta3(j,1)*!a0)*(
exp(beta0(j,1)+beta1(j,1)*!a1)/
(1+exp(beta0(j,1)+beta1(j,1)*!a1))-
exp(beta0(j,1)+beta1(j,1)*!a0)/
(1+exp(beta0(j,1)+beta1(j,1)*!a0))
).
*/ TNDE*/.
compute effect(j,4)=(theta1(j,1)+theta3(j,1)*exp(beta0(j,1)+beta1(j,1)*!a1)/(1+exp(beta0(j,1)+beta1(j,1)*!a1)))*(!a1-!a0).
*/  TNIE*/.
compute effect(j,5)=(theta2(j,1)+theta3(j,1)*!a1)*(
exp(beta0(j,1)+beta1(j,1)*!a1)/
(1+exp(beta0(j,1)+beta1(j,1)*!a1))-
exp(beta0(j,1)+beta1(j,1)*!a0)/
(1+exp(beta0(j,1)+beta1(j,1)*!a0))
).
*/ te*/.
compute effect(j,11)=(effect(j,2))+(effect(j,5)).

!IFEND.



!If (!interaction=FALSE & !yreg=LINEAR & !mreg=LOGISTIC & !NC~=0 ) !Then.
*/compute the effects*/.

!If (!c~=NA) !Then.
Loop i=1 To !NC.
*/CONDITIONAL CDE*/.
compute effect(j,1)=(theta1(j,1))*(!a1-!a0).
*/CONDITIONAL NDE*/.
compute effect(j,2)=(theta1(j,1))*(!a1-!a0).
*/CONDITIONAL NIE*/.
compute effect(j,3)=(theta2(j,1))*(
exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i)))))-
exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i)))))
).
*/CONDITIONAL TNDE*/.
compute effect(j,4)=(
theta1(j,1)
)*(!a1-!a0).
*/ CONDITIONAL TNIE*/.
compute effect(j,5)=(theta2(j,1))*(
exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(cval(1,i)))))-
exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(cval(1,i)))))
).
*/conditional te*/.
compute effect(j,12)=(effect(j,2))+(effect(j,5)).
End Loop.
!IFEND.

Loop i=1 To !nc.
*/MARGINAL CDE*/.
compute effect(j,6)=(theta1(j,1))*(!a1-!a0).
*/MARGINAL NDE*/.
compute effect(j,7)=(theta1(j,1))*(!a1-!a0).
*/MARGINAL NIE*/.
compute effect(j,8)=(theta2(j,1))*(
exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i)))))-
exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i))))/
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i)))))
).
*/ MARGINAL TNDE*/.
compute effect(j,9)=(theta1(j,1))*(!a1-!a0).
*/ MARGINAL TNIE*/.
compute effect(j,10)=(theta2(j,1))*(exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i))))/(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*(meanc(1,i)))))
-exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i))))/(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*(meanc(1,i)))))).
*/marginal te*/.
compute effect(j,11)=(effect(j,7))+(effect(j,10)).

End Loop.

!IFEND.



!If (!interaction=FALSE & !yreg=LINEAR & !mreg=LOGISTIC & !NC=0 ) !Then.
*/compute the effects*/.


*/ CDE*/.
compute effect(j,1)=(theta1(j,1))*(!a1-!a0).
*/ NDE*/.
compute effect(j,2)=(theta1(j,1))*(!a1-!a0).
*/ NIE*/.
compute effect(j,3)=(theta2(j,1))*(
exp(beta0(j,1)+beta1(j,1)*!a1)/
(1+exp(beta0(j,1)+beta1(j,1)*!a1))-
exp(beta0(j,1)+beta1(j,1)*!a0)/
(1+exp(beta0(j,1)+beta1(j,1)*!a0))
).
*/ TNDE*/.
compute effect(j,4)=(theta1(j,1))*(!a1-!a0).
*/  TNIE*/.
compute effect(j,5)=(theta2(j,1))*(
exp(beta0(j,1)+beta1(j,1)*!a1)/
(1+exp(beta0(j,1)+beta1(j,1)*!a1))-
exp(beta0(j,1)+beta1(j,1)*!a0)/
(1+exp(beta0(j,1)+beta1(j,1)*!a0))
).
*/ te*/.
compute effect(j,11)=(effect(j,2))+(effect(j,5)).

!IFEND.



/*Y LOGISTIC M LINEAR/*.


!IF ((!yreg=LOGISTIC & !mreg=LINEAR & !interaction=TRUE & !NC~=0) | (!yreg=LOGLINEAR & !mreg=LINEAR & !interaction=TRUE & !NC~=0) | (!yreg=POISSON & !mreg=LINEAR & !interaction=TRUE & !NC~=0) 
 | (!yreg=NEGBIN & !mreg=LINEAR & !interaction=TRUE & !NC~=0))  !THEN.

compute tsq=(theta3(j,1)**2).
compute  rm=s2(j,1)**2.
compute s2(j,1)=s2(j,1)**2.
compute  asq=(!a1**2).
compute	 a1sq=(!a0**2).

Loop i=1 To !NC.
!If (!c~=NA) !Then.
	  */CONDITIONAL CDE*/.
	compute  x1=(theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0).
 compute  effect(j,1)=exp(x1).
      */CONDITIONAL NDE*/.	  
	compute  x2=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+csum(theta3(j,1)*beta2(j,i)*t(cval(1,i)))+theta3(j,1)*theta2(j,1)*rm)*(!a1-!a0)+(1/2)*tsq*rm*(asq-a1sq).
	compute  effect(j,2)=exp(x2).
      */CONDITIONAL NIE*/.
	compute  x3=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a0)*(!a1-!a0).
 compute   effect(j,3)=exp(x3).
      */CONDITIONAL TNDE*/.
compute x4=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a1+csum(theta3(j,1)*beta2(j,i)*t(cval(1,i)))+theta3(j,1)*theta2(j,1)*rm)*(!a1-!a0)+(1/2)*tsq*rm*(asq-a1sq).
compute effect(j,4)=exp(x4).
      */ CONDITIONAL TNIE*/.
	compute  x5=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).
 compute     effect(j,5)=exp(x5).
	  !IFEND.
	  */MARGINAL CDE*/.
compute	  x6=(theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0).
	compute  effect(j,6)=exp(x6).
	   */MARGINAL NDE*/.
	 compute x7=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+csum(theta3(j,1)*beta2(j,i)*t(meanc(1,i)))+theta3(j,1)*theta2(j,1)*rm)*(!a1-!a0)+1/2*tsq*rm*(asq-a1sq).
 compute     effect(j,7)=exp(x7).
	  */MARGINAL NIE*/.
	compute  x8=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a0)*(!a1-!a0).
compute	  effect(j,8)=exp(x8).
	  */ MARGINAL TNDE*/.
	 compute x9=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a1+csum(theta3(j,1)*beta2(j,i)*t(meanc(1,i)))+theta3(j,1)*theta2(j,1)*rm)*(!a1-!a0)+1/2*tsq*rm*(asq-a1sq).
	 compute effect(j,9)=exp(x9).
	  */ MARGINAL TNIE*/.
	compute  x10=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).
	compute  effect(j,10)=exp(x10).
!If (!c ~=NA ) !Then.
compute logtecon=(theta1(j,1)+theta3(j,1)*beta0(j,1)+
theta3(j,1)*beta1(j,1)*!a0+csum(theta3(j,1)*beta2(j,i)*t(cval(1,i)))+theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1+theta3(j,1)*(rm)*theta2(j,1))*(!a1-!a0)+0.5*(theta3(j,1)**2)*(rm)*(!a1**2-!a0**2).
compute effect(j,12)=exp(logtecon).
!IFEND.
compute logtemar=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+
csum(theta3(j,1)*beta2(j,i)*t(meanc(1,i)))+theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1+theta3(j,1)*(rm)*theta2(j,1))*(!a1-!a0)+0.5*(theta3(j,1)**2)*(rm)*(!a1**2-!a0**2).
compute effect(j,11)=exp(logtemar).
End Loop.

!IFEND





!IF ((!yreg=LOGISTIC & !mreg=LINEAR & !interaction=TRUE & !NC=0) |(!yreg=LOGLINEAR & !mreg=LINEAR & !interaction=TRUE & !NC=0) | (!yreg=POISSON & !mreg=LINEAR & !interaction=TRUE & !NC=0)
 | (!yreg=NEGBIN & !mreg=LINEAR & !interaction=TRUE & !NC=0)) !THEN.

compute tsq=(theta3(j,1)**2).
compute  rm=s2(j,1)**2.
compute s2(j,1)=s2(j,1)**2.
compute  asq=(!a1**2).
compute	 a1sq=(!a0**2).

	  */ CDE*/.
	compute  x1=(theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0).
 compute  effect(j,1)=exp(x1).
      */ NDE*/.	  
	compute  x2=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+theta3(j,1)*theta2(j,1)*rm)*(!a1-!a0)+(1/2)*tsq*rm*(asq-a1sq).
	compute  effect(j,2)=exp(x2).
      */ NIE*/.
	compute  x3=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a0)*(!a1-!a0).
 compute   effect(j,3)=exp(x3).
      */ TNDE*/.
compute x4=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a1+theta3(j,1)*theta2(j,1)*rm)*(!a1-!a0)+(1/2)*tsq*rm*(asq-a1sq).
compute effect(j,4)=exp(x4).
      */  TNIE*/.
	compute  x5=(theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1)*(!a1-!a0).
 compute effect(j,5)=exp(x5).
compute logtemar=(theta1(j,1)+theta3(j,1)*beta0(j,1)+theta3(j,1)*beta1(j,1)*!a0+theta2(j,1)*beta1(j,1)+theta3(j,1)*beta1(j,1)*!a1+theta3(j,1)*(rm)*theta2(j,1))*(!a1-!a0)+0.5*(theta3(j,1)**2)*(rm)*(!a1**2-!a0**2).
compute effect(j,11)=exp(logtemar).

!IFEND







!IF ((!yreg=LOGISTIC & !mreg=LINEAR & !interaction=FALSE & !NC~=0) | (!yreg=LOGLINEAR & !mreg=LINEAR & !interaction=FALSE & !NC~=0) | (!yreg=POISSON & !mreg=LINEAR & !interaction=FALSE & !NC~=0) 
 | (!yreg=NEGBIN & !mreg=LINEAR & !interaction=FALSE & !NC~=0)) !THEN. 


compute  rm=s2(j,1)**2.
compute s2(j,1)=s2(j,1)**2.
compute  asq=(!a1**2).
compute	 a1sq=(!a0**2).

	  */ CDE*/.
	compute  x1=(theta1(j,1))*(!a1-!a0).
 compute  effect(j,1)=exp(x1).
      */ NDE*/.	  
	compute  x2=(theta1(j,1))*(!a1-!a0).
	compute  effect(j,2)=exp(x2).
      */ NIE*/.
	compute  x3=(theta2(j,1)*beta1(j,1))*(!a1-!a0).
 compute   effect(j,3)=exp(x3).
      */ TNDE*/.
compute x4=(theta1(j,1))*(!a1-!a0).
compute effect(j,4)=exp(x4).
      */  TNIE*/.
	compute  x5=(theta2(j,1)*beta1(j,1))*(!a1-!a0).
 compute     effect(j,5)=exp(x5).
compute logtemar=(theta1(j,1)+theta2(j,1)*beta1(j,1))*(!a1-!a0).
compute effect(j,11)=exp(logtemar).
!IFEND


!IF ((!yreg=LOGISTIC & !mreg=LINEAR & !interaction=FALSE & !NC=0) | (!yreg=LOGLINEAR & !mreg=LINEAR & !interaction=FALSE & !NC=0) | (!yreg=POISSON & !mreg=LINEAR & !interaction=FALSE & !NC=0)
 | (!yreg=NEGBIN & !mreg=LINEAR & !interaction=FALSE & !NC=0)) !THEN.

compute  rm=s2(j,1)**2.
compute s2(j,1)=s2(j,1)**2.
compute  asq=(!a1**2).
compute	 a1sq=(!a0**2).


	  */ CDE*/.
	compute  x1=(theta1(j,1))*(!a1-!a0).
 compute  effect(j,1)=exp(x1).
      */ NDE*/.	  
	compute  x2=(theta1(j,1))*(!a1-!a0).
	compute  effect(j,2)=exp(x2).
      */ NIE*/.
	compute  x3=(theta2(j,1)*beta1(j,1))*(!a1-!a0).
 compute   effect(j,3)=exp(x3).
      */ TNDE*/.
compute x4=(theta1(j,1))*(!a1-!a0).
compute effect(j,4)=exp(x4).
      */  TNIE*/.
	compute  x5=(theta2(j,1)*beta1(j,1))*(!a1-!a0).
 compute     effect(j,5)=exp(x5).
compute logtemar=(theta1(j,1)+theta2(j,1)*beta1(j,1))*(!a1-!a0).
compute effect(j,11)=exp(logtemar).

!IFEND.




/*Y LOGISTIC M LOGISTIC/*.



!If ((!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) 
 | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=TRUE & !NC=0)) !Then.

*/ CDE*/.
compute x1=exp((theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0)).
compute effect(j,1)=x1.
*/ NDE*/.
compute effect(j,2)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a0))/
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a0)).
*/ NIE*/.
compute effect(j,3)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0))*
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a1))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1))*
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a0))
).
*/ TNDE*/.
compute effect(j,4)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a1))/
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a1)).

compute effect(j,5)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a1))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a0))
).

compute logtemar=ln((effect(j,2))*(effect(j,5))).
compute effect(j,11)=(effect(j,2))*(effect(j,5)).

!IFEND.




!If ((!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=TRUE & !NC~=0)  | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=TRUE & !NC~=0) | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=TRUE & !NC~=0)
 | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=TRUE & !NC~=0)) !Then. 

Loop i=1 To !NC.
!If (!c~=NA) !Then.

*/CONDITIONAL CDE*/.
compute x1=exp((theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0)).
compute effect(j,1)=x1.
*/CONDITIONAL NDE*/.
compute effect(j,2)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))/
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i))))).
*/CONDITIONAL NIE*/.
compute effect(j,3)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))*
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))*
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))
).
*/CONDITIONAL TNDE*/.
compute effect(j,4)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))/
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i))))).

compute effect(j,5)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))
).
!IFEND.

*/MARGINAL CDE*/.
compute x6=(theta1(j,1)+theta3(j,1)*!m)*(!a1-!a0).
compute effect(j,6)=exp(x6).
*/MARGINAL NDE*/.
compute effect(j,7)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i)))))/
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i))))).
*/MARGINAL NIE*/.
compute effect(j,8)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i)))))*
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))*
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i)))))
).
*/ MARGINAL TNDE*/.
compute effect(j,9)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))/
(1+exp(theta2(j,1)+theta3(j,1)*!a0+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i))))).
*/ MARGINAL TNIE*/.
compute effect(j,10)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i)))))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))*
(1+exp(theta2(j,1)+theta3(j,1)*!a1+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i)))))
).

!If (!c~=NA) !Then.


compute effect(j,12)=(effect(j,2))*(effect(j,5)).
compute logtecon=ln(effect(j,12)).
!IFEND.


compute effect(j,11)=(effect(j,7))*(effect(j,10)).
compute logtemar=ln((effect(j,7))*(effect(j,10))).
End Loop.

!IFEND.




!If ((!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=FALSE & !NC~=0)  | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=FALSE & !NC~=0)  | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=FALSE & !NC~=0) 
 | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=FALSE & !NC~=0)) !Then.

Loop i=1 To !NC.
!If (!c~=NA) !Then.

*/CONDITIONAL CDE*/.
compute x1=exp((theta1(j,1))*(!a1-!a0)).
compute effect(j,1)=x1.
*/CONDITIONAL NDE*/.
compute effect(j,2)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))/
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i))))).
*/CONDITIONAL NIE*/.
compute effect(j,3)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))
).
*/CONDITIONAL TNDE*/.
compute effect(j,4)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))/
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i))))).

compute effect(j,5)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(cval(1,i)))))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(cval(1,i)))))
).
!IFEND.

*/MARGINAL CDE*/.
compute x6=(theta1(j,1))*(!a1-!a0).
compute effect(j,6)=exp(x6).

*/MARGINAL NDE*/.
compute effect(j,7)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i)))))/
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i))))).

*/MARGINAL NIE*/.
compute effect(j,8)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i)))))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i)))))
).

*/ MARGINAL TNDE*/.
compute effect(j,9)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))/
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i))))).

*/ MARGINAL TNIE*/.
compute effect(j,10)=(
(1+exp(beta0(j,1)+csum(beta2(j,i)*t(meanc(1,i)))))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1+csum(beta2(j,i)*t(meanc(1,i)))))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0+csum(beta2(j,i)*t(meanc(1,i)))))
).

!If (!c~=NA) !Then.
compute logtecon=ln((effect(j,2))*(effect(j,5))).
compute effect(j,12)=(effect(j,2))*(effect(j,5)).

!IFEND.

compute logtemar=ln((effect(j,7))*(effect(j,10))).
compute effect(j,11)=(effect(j,7))*(effect(j,10)).

End Loop.

!IFEND.




!If ((!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=FALSE & !NC=0)  | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=FALSE & !NC=0) | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=FALSE & !NC=0) 
 | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=FALSE & !NC=0)) !Then.


*/ CDE*/.
compute x1=exp((theta1(j,1))*(!a1-!a0)).
compute effect(j,1)=x1.
*/ NDE*/.
compute effect(j,2)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0))/
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0)).
*/ NIE*/.
compute effect(j,3)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0))
).
*/ TNDE*/.
compute effect(j,4)=exp(theta1(j,1)*(!a1-!a0))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1))/
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1)).

compute effect(j,5)=(
(1+exp(beta0(j,1)+beta1(j,1)*!a0))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a1))
)/
(
(1+exp(beta0(j,1)+beta1(j,1)*!a1))*
(1+exp(theta2(j,1)+beta0(j,1)+beta1(j,1)*!a0))
).

compute logtemar=ln((effect(j,2))*(effect(j,5))).
compute effect(j,11)=(effect(j,2))*(effect(j,5)).
!IFEND.


end loop.
SAVE {effect(:,1)} / outfile=!path+'effect1.sav'.
SAVE {effect(:,2)} / outfile=!path+'effect2.sav'.
SAVE {effect(:,3)} / outfile=!path+'effect3.sav'.
SAVE {effect(:,4)} / outfile=!path+'effect4.sav'.
SAVE {effect(:,5)} / outfile=!path+'effect5.sav'.
SAVE {effect(:,6)} / outfile=!path+'effect6.sav'.
SAVE {effect(:,7)} / outfile=!path+'effect7.sav'.
SAVE {effect(:,8)} / outfile=!path+'effect8.sav'.
SAVE {effect(:,9)} / outfile=!path+'effect9.sav'.
SAVE {effect(:,10)} / outfile=!path+'effect10.sav'.
SAVE {effect(:,11)} / outfile=!path+'effect11.sav'.
SAVE {effect(:,12)} / outfile=!path+'effect12.sav'.


end matrix.


Get file =!path+'effect1.sav'.
PRINT.
SORT CASES BY COL1.
SAVE  outfile=!path+'effect1.sav'.
Get file =!path+'effect2.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect2.sav'.
Get file =!path+'effect3.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect3.sav'.
Get file =!path+'effect4.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect4.sav'.
Get file =!path+'effect5.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect5.sav'.
Get file =!path+'effect6.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect6.sav'.
Get file =!path+'effect7.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect7.sav'.
Get file =!path+'effect8.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect8.sav'.
Get file =!path+'effect9.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect9.sav'.
Get file =!path+'effect10.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect10.sav'.
Get file =!path+'effect11.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect11.sav'.
Get file =!path+'effect12.sav'.
PRINT.
SORT CASES BY COL1 ( A) .
SAVE  outfile=!path+'effect12.sav'.



matrix.

	!IF (!boot=TRUE) !Then.
compute #reps=1000.
		!ifend.	
		!IF (!boot~=TRUE) !Then.
compute #reps=!boot.
		!ifend.
Get effect1/file=!path+ 'effect1.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect2/file=!path+ 'effect2.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect3/file=!path+ 'effect3.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect4/file=!path+ 'effect4.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect5/file=!path+ 'effect5.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect6/file=!path+ 'effect6.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect7/file=!path+ 'effect7.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect8/file=!path+ 'effect8.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect9/file=!path+ 'effect9.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect10/file=!path+ 'effect10.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect11/file=!path+ 'effect11.sav'/VARIABLES=COL1/MISSING=OMIT.
Get effect12/file=!path+ 'effect12.sav'/VARIABLES=COL1/MISSING=OMIT.

compute effect={effect1,effect2,effect3,effect4,effect5,effect6,effect7,effect8,effect9,effect10,effect11,effect12}.

compute mean=make(#reps,12,0).
compute se=make(12,1,0).
compute cl=make(12,1,0).
compute cu=make(12,1,0).
compute x=make(#reps,12,0).


!If ((!interaction=TRUE & !yreg=LINEAR & !mreg=LINEAR & !NC~=0 ) | (!yreg=LINEAR & !mreg=LOGISTIC & !NC~=0 )
 | (!yreg~=LINEAR & !interaction=TRUE & !NC~=0) | (!yreg~=LINEAR & !mreg=LOGISTIC & !interaction=FALSE & !NC~=0))  !Then.
 
*/compute the effects*/.

!IF (!c~=NA) !THEN.
loop i=1 to 12.
loop j=1 to #reps.
compute mean(j,i)=csum(effect(:,i))/#reps.
end loop.
loop j=1 to #reps.
compute x(j,i)=(effect(j,i)-mean(j,i))**2.
end loop.
compute se(i,1)=csum(x(:,i))/#reps.
compute cl(i,1)=effect((25*#reps)/1000,i).
compute cu(i,1)=effect((976*#reps)/1000,i).
end loop.

!IF (!Output=FULL) !THEN.
compute tmean= T(mean(1,:)).
compute table={tmean,se,cl,cu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","se"," p_95CIlow","p_95CIup"
 /rnames={"ccde";"cpnde";"cpnie";"ctnde";"ctnie";"mcde";"mpnde";"mpnie";"mtnde";"mtnie";"mte";"cte"}.
!ifend.

!IF (!output~=FULL) !THEN.
compute tmean1=transpos(mean(1,6:7)).
compute tmean2=transpos(mean(1,10:11)).
compute tmean={tmean1;tmean2}.
compute se1=se(6:7,1).
compute se2=se(10:11,1).
compute cl1=cl(6:7,1).
compute cl2=cl(10:11,1).
compute cu1=cu(6:7,1).
compute cu2=cu(10:11,1).
compute se={se1;se2}.
compute cl={cl1;cl2}.
compute cu={cu1;cu2}.
compute table={tmean,se,cl,cu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","se"," p_95CIlow","p_95CIup"
 /rnames={"cde";"pnde";"tnie";"te"}.
!ifend.

!ifend.

!IF (!c=NA) !THEN.
Loop i=6 to 11.
loop j=1 to #reps.
compute mean(j,i)=csum(effect(:,i))/#reps.
end loop.
loop j=1 to #reps.
compute x(j,i)=(effect(j,i)-mean(j,i))**2.
end loop.
compute se(i,1)=csum(x(:,i))/#reps.
compute cl(i,1)=effect((25*#reps)/1000,i).
compute cu(i,1)=effect((976*#reps)/1000,i).
end loop.

!IF (!output=FULL) !THEN.
compute tmean=transpos(mean(1,6:11)).
compute table={tmean,se,cl,cu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","se"," p_95CIlow","p_95CIup"
 /rnames={"mcde";"mpnde";"mpnie";"mtnde";"mtnie";"mte"}.
!ifend.

!IF (!output~=FULL) !THEN.
compute tmean1=transpos(mean(1,6:7)).
compute tmean2=transpos(mean(1,10:11)).
compute tmean={tmean1;tmean2}.
compute se1=se(6:7,1).
compute se2=se(10:11,1).
compute cl1=cl(6:7,1).
compute cl2=cl(10:11,1).
compute cu1=cu(6:7,1).
compute cu2=cu(10:11,1).
compute se={se1;se2}.
compute cl={cl1;cl2}.
compute cu={cu1;cu2}.
compute table={tmean,se,cl,cu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","se"," p_95CIlow","p_95CIup"
 /rnames={"cde";"pnde";"tnie";"te"}.
!ifend.

!ifend.



!IFEND.

!If ((!interaction=FALSE & !yreg=LINEAR & !mreg=LINEAR & !NC~=0 ) |  (!interaction=FALSE & !yreg=LINEAR & !mreg=LINEAR & !NC=0 ) | (!interaction=FALSE & !yreg=LINEAR & !mreg=LOGISTIC & !NC=0 ) | 
 (!interaction=FALSE & !yreg=LOGISTIC & !mreg=LINEAR))  !Then.
*/compute the effects*/.


loop j=1 to #reps.
compute mean(j,3)=csum(effect(:,3))/#reps.
compute mean(j,1)=csum(effect(:,1))/#reps.
compute mean(j,11)=csum(effect(:,11))/#reps.
end loop.
loop j=1 to #reps.
compute x(j,1)=(effect(j,1)-mean(j,1))**2.
compute x(j,3)=(effect(j,3)-mean(j,3))**2.
compute x(j,11)=(effect(j,11)-mean(j,1))**2.
end loop.
compute se(3,1)=csum(x(:,3))/#reps.
compute cl(3,1)=effect((25*#reps)/1000,3).
compute cu(3,1)=effect((976*#reps)/1000,3).
compute se(1,1)=csum(x(:,1))/#reps.
compute cl(1,1)=effect((25*#reps)/1000,1).
compute cu(1,1)=effect((976*#reps)/1000,1).
compute se(11,1)=csum(x(:,11))/#reps.
compute cl(11,1)=effect((25*#reps)/1000,11).
compute cu(11,1)=effect((976*#reps)/1000,11).
compute se={se(1,1);se(3,1);se(11,1)}.
compute mean={mean(1,1);mean(1,3);mean(1,11)}.
compute cl={cl(1,1);cl(3,1);cl(11,1)}.
compute cu={cu(1,1);cu(3,1);cu(11,1)}.
compute table={mean,se,cl,cu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","se"," p_95CIlow","p_95CIup"
 /rnames={"cde=nde";"nie"; "te"}.

!IFEND.








!If ((!interaction=TRUE & !yreg=LINEAR & !mreg=LINEAR & !NC=0 ) | (!interaction=TRUE & !yreg=LINEAR & !mreg=LOGISTIC & !NC=0 ) | ((!yreg=LOGISTIC & !mreg=LINEAR & !interaction=TRUE & !NC=0)
 | (!yreg=LOGLINEAR & !mreg=LINEAR & !interaction=TRUE & !NC=0) | (!yreg=POISSON & !mreg=LINEAR & !interaction=TRUE & !NC=0)| (!yreg=NEGBIN & !mreg=LINEAR & !interaction=TRUE & !NC=0)) 
 | (!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) 
 | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) | (!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=FALSE & !NC=0)  | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=FALSE & !NC=0) 
 | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=FALSE & !NC=0) | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=FALSE & !NC=0)) !Then.
Loop i=1 to 5.
loop j=1 to #reps.
compute mean(j,i)=csum(effect(:,i))/#reps.
end loop.
loop j=1 to #reps.
compute x(j,i)=(effect(j,i)-mean(j,i))**2.
end loop.
compute se(i,1)=csum(x(:,i))/#reps.
compute cl(i,1)=effect((25*#reps)/1000,i).
compute cu(i,1)=effect((976*#reps)/1000,i).
end loop.

loop j=1 to #reps.
compute mean(j,11)=csum(effect(:,11))/#reps.
end loop.
loop j=1 to #reps.
compute x(j,11)=(effect(j,11)-mean(j,11))**2.
end loop.
compute se(11,1)=csum(x(:,11))/#reps.
compute cl(11,1)=effect((25*#reps)/1000,11).
compute cu(11,1)=effect((976*#reps)/1000,11).

!IF (!output=FULL) !THEN.
compute tmean=transpos(mean(1,1:5)).
compute mean={tmean;mean(1,11)}.
compute se={se(1:5,1);se(11,1)}.
compute cl={cl(1:5,1);cl(11,1)}.
compute cu={cu(1:5,1);cu(11,1)}.
compute table={mean,se,cl,cu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","se"," p_95CIlow","p_95CIup"
 /rnames={"cde";"pnde";"pnie";"tnde";"tnie";"te"}.
!IFEND.
!IF (!output~=FULL) !THEN.
compute tmean=transpos(mean(1,1:5)).
compute mean={tmean(1:2,1);mean(1,5);mean(1,11)}.
compute se={se(1:2,1);se(5,1);se(11,1)}.
compute cl={cl(1:2,1);cl(5,1);cl(11,1)}.
compute cu={cu(1:2,1);cu(5,1);cu(11,1)}.
compute table={mean,se,cl,cu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","se"," p_95CIlow","p_95CIup"
 /rnames={"cde";"pnde";"tnie";"te"}.
!ifend.
!IFEND.




END MATRIX.
!IFEND.



Get file =!data.
PRINT.

!If (!interaction=TRUE) !Then.
compute Int=!avar*!mvar.
!IFEND.

*/OUTCOME REGRESSION*/.

!If (!interaction=TRUE & !yreg=LINEAR & !NC~=0) !Then.
  REGRESSION 
   /DEPENDENT !yvar 
   /METHOD=ENTER !avar !mvar Int !cvar 
   / outfile= covb(!path+ 'out1.sav').
!IFEND.

!If (!interaction~=TRUE & !yreg=LINEAR & !NC~=0) !Then.
  REGRESSION 
   /DEPENDENT !yvar 
   /METHOD=ENTER !avar !mvar !cvar 
   / outfile= covb(!path+ 'out1.sav').
!IFEND.

!If (!interaction=TRUE & !yreg=LINEAR & !NC=0) !Then.
  REGRESSION 
   /DEPENDENT !yvar 
   /METHOD=ENTER !avar !mvar Int  
   / outfile= covb(!path+ 'out1.sav').
!IFEND.

!If (!interaction=FALSE & !yreg=LINEAR & !NC=0) !Then.
  REGRESSION 
   /DEPENDENT !yvar 
   /METHOD=ENTER !avar !mvar
   / outfile= covb(!path+ 'out1.sav').
!IFEND.


!If (!interaction=TRUE & !yreg=LOGLINEAR & !NC~=0) !Then.
GENLIN
  !yvar (ORDER = DESCENDING)   WITH  !avar !mvar Int !cvar 
 /MODEL  !avar !mvar Int !cvar 
  INTERCEPT=YES
  DISTRIBUTION=BINOMIAL
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction~=TRUE & !yreg=LOGLINEAR & !NC~=0) !Then.
 GENLIN
  !yvar (ORDER = DESCENDING)    WITH  !avar !mvar  !cvar 
 /MODEL  !avar !mvar !cvar 
  INTERCEPT=YES
  DISTRIBUTION=BINOMIAL
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=TRUE & !yreg=LOGLINEAR & !NC=0) !Then.
 GENLIN
  !yvar   (ORDER = DESCENDING)   WITH    !avar !mvar Int 
 /MODEL  !avar !mvar Int 
  INTERCEPT=YES
  DISTRIBUTION=BINOMIAL
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=FALSE & !yreg=LOGLINEAR & !NC=0) !Then.
 GENLIN
  !yvar  (ORDER = DESCENDING)    WITH  !avar !mvar
 /MODEL  !avar !mvar 
  INTERCEPT=YES
  DISTRIBUTION=BINOMIAL
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.


!If (!interaction=TRUE & !yreg=POISSON & !NC~=0) !Then.
GENLIN
  !yvar   WITH  !avar !mvar Int !cvar 
 /MODEL  !avar !mvar Int !cvar 
  INTERCEPT=YES
  DISTRIBUTION=POISSON
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=FALSE & !yreg=POISSON & !NC~=0) !Then.
 GENLIN
  !yvar   WITH  !avar !mvar !cvar 
 /MODEL  !avar !mvar !cvar 
  INTERCEPT=YES
  DISTRIBUTION=POISSON
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=TRUE & !yreg=POISSON & !NC=0) !Then.

GENLIN
  !yvar    WITH  !avar !mvar Int
 /MODEL  !avar !mvar Int 
  INTERCEPT=YES
  DISTRIBUTION=POISSON
  LINK=LOG
/OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=FALSE & !yreg=POISSON & !NC=0) !Then.
 GENLIN
  !yvar   WITH  !avar !mvar
 /MODEL  !avar !mvar 
  INTERCEPT=YES
  DISTRIBUTION=POISSON
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.


!If (!interaction=TRUE & !yreg=NEGBIN & !NC~=0) !Then.
GENLIN
  !yvar   WITH  !avar !mvar Int !cvar 
 /MODEL  !avar !mvar Int !cvar 
  INTERCEPT=YES
  DISTRIBUTION=NEGBIN (MLE)
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction~=TRUE & !yreg=NEGBIN & !NC~=0) !Then.
 GENLIN
  !yvar   WITH  !avar !mvar  !cvar 
 /MODEL  !avar !mvar !cvar 
  INTERCEPT=YES
  DISTRIBUTION=NEGBIN (MLE)
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=TRUE & !yreg=NEGBIN & !NC=0) !Then.
 GENLIN
  !yvar    WITH  !avar !mvar Int
 /MODEL  !avar !mvar Int 
  INTERCEPT=YES
  DISTRIBUTION=NEGBIN (MLE)
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=FALSE & !yreg=NEGBIN & !NC=0) !Then.
 GENLIN
  !yvar    WITH  !avar !mvar 
 /MODEL  !avar !mvar 
  INTERCEPT=YES
  DISTRIBUTION=NEGBIN (MLE)
  LINK=LOG
   /OUTFILE
 COVB=!path+ 'out1.sav'.
!IFEND.

!If (!interaction=TRUE & !yreg=LOGISTIC & !NC~=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out11.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr1.sav'.
  LOGISTIC REGRESSION VARIABLES = !yvar With !avar, !mvar,Int,!cvar
  /PRINT=ALL.
OMSEND.
!IFEND.

!If (!interaction=FALSE & !yreg=LOGISTIC & !NC~=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out11.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr1.sav'.
  LOGISTIC REGRESSION VARIABLES = !yvar With !avar, !mvar,!cvar
  /PRINT=ALL.
OMSEND.
!IFEND.

!If (!interaction=TRUE & !yreg=LOGISTIC & !NC=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out11.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr1.sav'.
  LOGISTIC REGRESSION VARIABLES = !yvar With !avar, !mvar,Int
/PRINT=ALL.
OMSEND.
!IFEND.

!If (!interaction=FALSE & !yreg=LOGISTIC & !NC=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out11.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr1.sav'.
  LOGISTIC REGRESSION VARIABLES = !yvar With !avar, !mvar
/PRINT=ALL.
OMSEND.
!IFEND.


*/MEDIATOR REGRESSION*/.
OMS
 /SELECT TABLES
 /if COMMAND= ["REGRESSION"]
    SUBTYPES= ["Model Summary"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'rmse.sav'.
!If ( !mreg=LINEAR & !NC~=0) !Then.
!If (!casecontrol=TRUE) !Then.
RECODE !yvar (1 = 0)(0 = 1).
FILTER BY !yvar.
!IFEND.
 REGRESSION 
   /DEPENDENT !mvar 
   /METHOD=ENTER !avar !cvar
   / outfile= covb(!path+ 'out2.sav').
OMSEND.
!If (!casecontrol=TRUE) !Then.
FILTER OFF.
RECODE !yvar (1 = 0)(0 = 1).
!IFEND.
!IFEND.




OMS
 /SELECT TABLES
 /if COMMAND= ["REGRESSION"]
    SUBTYPES= ["Model Summary"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'rmse.sav'.
!If ( !mreg=LINEAR & !NC=0) !Then.
!If (!casecontrol=TRUE) !Then.
RECODE !yvar (1 = 0)(0 = 1).
FILTER BY !yvar.
!IFEND.
 REGRESSION 
   /DEPENDENT !mvar 
   /METHOD=ENTER !avar 
   / outfile= covb(!path+ 'out2.sav').
OMSEND.
!If (!casecontrol=TRUE) !Then.
FILTER OFF.
RECODE !yvar (1 = 0)(0 = 1).
!IFEND.
!IFEND.

!If ( !mreg=LOGISTIC & !NC~=0) !Then.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out21.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr2.sav'.
!If (!casecontrol=TRUE) !Then.
RECODE !yvar (1 = 0)(0 = 1).
FILTER BY !yvar.
!IFEND.
LOGISTIC REGRESSION VARIABLES = !mvar With !avar, !cvar
/PRINT=ALL.
OMSEND.
!If (!casecontrol=TRUE) !Then.
FILTER OFF.
RECODE !yvar (1 = 0)(0 = 1).
!IFEND.
!IFEND.




!If ( !mreg=LOGISTIC & !NC=0) !Then.
!If (!casecontrol=TRUE) !Then.
RECODE !yvar (1 = 0)(0 = 1).
FILTER BY !yvar.
!IFEND.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
    SUBTYPES= ["Variables in the Equation"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'out21.sav'.
OMS
 /SELECT TABLES
 /if COMMAND= ["LOGISTIC REGRESSION"]
   SUBTYPES= [ "Correlation Matrix"]
 /DESTINATION FORMAT = SAV
 outfile=!path+ 'corr2.sav'.
LOGISTIC REGRESSION VARIABLES = !mvar With !avar
/PRINT=ALL.
OMSEND.
!If (!casecontrol=TRUE) !Then.
FILTER OFF.
RECODE !yvar (1 = 0)(0 = 1).
!IFEND.
!IFEND.




!if (!boot=FALSE) !THEN.




!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
get file=!path+ 'out1.sav'.
save  outfile=!path+ 'out1.sav'
 /DROp RowType_ VarName_.
!IFEND.



MATRIX.
*/saving objects*/.

!if (!NC~=0) !then.
compute meanc =make(1,!NC,0).
compute cval =make(1,!NC,0).				
!IFEND.

!IF (!interaction=TRUE) !THEN.
compute estheta=4+1.
compute nvtheta=4.
!IFEND.

!IF (!interaction=FALSE) !THEN.
compute estheta=3+1.
compute nvtheta=3.
!IFEND.
  
compute estbeta=2+1.
compute nvbeta=2.
compute neffects=12.
compute a=!a1.
compute a1=!a0.
compute m=!m.


!If (!yreg=LINEAR | !yreg=POISSON ) !Then.
!If (!NC~=0) !Then.
!If (!interaction=TRUE) !Then.
!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
Get x1/file=!path+ 'out1.sav' /MISSING=0.
!ifend.
!IF (!yreg=LINEAR) !THEN.
Get x1/file=!path+ 'out1.sav'/VARIABLES=const_ !AVAR !MVAR INT !CVAR/MISSING=OMIT.
!IFEND.
compute sigtheta=X1(1:(!NC+4),1:(!NC+4)).
compute theta0=X1(4+!NC+1,1).
compute theta1=X1(4+!NC+1,2).
compute theta2=X1(4+!NC+1,3).
compute theta3=X1(4+!NC+1,4).
!IFEND.
!If (!interaction=FALSE) !Then.
!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
Get x1/file=!path+ 'out1.sav'/MISSING=0.
!ifend.
!IF (!yreg=LINEAR) !THEN.
Get x1/file=!path+ 'out1.sav'/VARIABLES=const_ !AVAR !MVAR  !CVAR/MISSING=OMIT.
!IFEND.
compute sigtheta=X1(1:(!NC+3),1:(!NC+3)).
compute theta0=X1(3+!NC+1,1).
compute theta1=X1(3+!NC+1,2).
compute theta2=X1(3+!NC+1,3).
!IFEND.
Loop I=1 To (!NC).
compute theta4I=X1(estheta,(I+nvtheta)).
End Loop.
!IFEND.
!If (!NC=0) !Then.
!If (!interaction=TRUE) !Then.
!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
Get x1/file=!path+ 'out1.sav' /VARIABLES=P1 P2 P3 P4/MISSING=OMIT.
!ifend.
!IF (!yreg=LINEAR) !then.
Get x1/file=!path+ 'out1.sav'/VARIABLES=const_ !AVAR !MVAR INT /MISSING=OMIT.
!IFEND.
compute sigtheta=X1(1:(4),1:(4)).
compute theta0=X1(4+1,1).
compute theta1=X1(4+1,2).
compute theta2=X1(4+1,3).
compute theta3=X1(4+1,4).
!IFEND.
!If (!interaction=FALSE) !Then.
!IF (!yreg=POISSON | !yreg=LOGLINEAR | !yreg=NEGBIN) !then.
Get x1/file=!path+ 'out1.sav'/VARIABLES=P1 P2 P3 /MISSING=OMIT.
!ifend.
!IF (!yreg=LINEAR) !then.
Get x1/file=!path+ 'out1.sav'/VARIABLES=const_ !AVAR !MVAR  /MISSING=OMIT.
!ifend.
compute sigtheta=X1(1:(3),1:(3)).
compute theta0=X1(3+1,1).
compute theta1=X1(3+1,2).
compute theta2=X1(3+1,3).
!IFEND.
!IFEND.
!IFEND.


!If (!yreg=LOGISTIC ) !Then.
!If (!NC~=0) !Then.
!If (!interaction=TRUE) !Then.
Get x1/file=!path+ 'out11.sav'/VARIABLES=B/MISSING=OMIT.
compute theta0=X1(4+!NC+1,1).
compute theta1=X1(2,1).
compute theta2=X1(3,1).
compute theta3=X1(4,1).
Loop I=1 To (!NC).
compute theta4I=X1(4+I,1).
End Loop.
get corr1 /file=!path+ 'corr1.sav'
 /variables=CONSTANT !avar !mvar int !cvar
/missing = OMIT.

get se /file=!path+ 'out11.sav'
/variables = S.E.

compute se0=se(4+!NC+1,1).
compute se1=se(2,1).
compute se2=se(3,1).
compute se3=se(4,1).
compute row=nrow(se)-1.
compute se4=se(5:row,1).
compute stder={se0;se1;se2;se3;se4}.
compute sigma=mdiag(stder).
compute co1 = sigma*corr1*sigma.
compute sigtheta=co1.

!IFEND.
!If (!interaction=FALSE) !Then.
Get x1/file=!path+ 'out11.sav'/VARIABLES=B/MISSING=OMIT.
compute theta0=X1(3+!NC+1,1).
compute theta1=X1(2,1).
compute theta2=X1(3,1).
Loop I=1 To (!NC).
compute theta4I=X1(3+I,1).
End Loop.
get corr1 /file=!path+ 'corr1.sav'
 /variables=CONSTANT !avar !mvar !cvar
/missing = 0.
get se /file=!path+ 'out11.sav'
/variables =  S.E.
compute se0=se(3+!NC+1,1).
compute se1=se(2,1).
compute se2=se(3,1).
compute row=nrow(se)-1.
compute se4=se(4:row,1).
compute stder={se0;se1;se2;se4}.
compute sigma=mdiag(stder).
compute co1 = sigma*corr1*sigma.
compute sigtheta=co1.
!IFEND.
!IFEND.
!If (!NC=0) !Then.
!If (!interaction=TRUE) !Then.
Get x1/file=!path+ 'out11.sav'/VARIABLES=B/MISSING=OMIT.
compute theta0=X1(4+1,1).
compute theta1=X1(2,1).
compute theta2=X1(3,1).
compute theta3=X1(4,1).
get corr1 /file=!path+ 'corr1.sav'
 /variables=CONSTANT !avar !mvar int 
/missing = OMIT.
get se /file=!path+ 'out11.sav'
/variables =  S.E.
compute se0=se(4+1,1).
compute se1=se(2,1).
compute se2=se(3,1).
compute se3=se(4,1).
compute stder={se0;se1;se2;se3}.
compute sigma=mdiag(stder).
compute co1 = sigma*corr1*sigma.
compute sigtheta=co1
!IFEND.
!If (!interaction=FALSE) !Then.
Get x1/file=!path+ 'out11.sav'/VARIABLES=B /MISSING=OMIT.
compute theta0=X1(3+1,1).
compute theta1=X1(2,1).
compute theta2=X1(3,1).
get corr1 /file=!path+ 'corr1.sav'
 /variables=CONSTANT !avar !mvar
/missing = OMIT.
get se /file=!path+ 'out11.sav'
/variables = S.E.
compute se0=se(3+1,1).
compute se1=se(2,1).
compute se2=se(3,1).
compute stder={se0;se1;se2}.
compute sigma=mdiag(stder).
compute co1 = sigma*corr1*sigma.
compute sigtheta=co1.
!IFEND.
!IFEND.
!IFEND.

!If (!mreg=LINEAR) !Then.
Get rmse/file=!path+ 'rmse.sav'/VARIABLES=Var1 R RSquare AdjustedRSquare Std.ErroroftheEstimate /MISSING=OMIT.
compute s2=rmse(1,5).
!If (!NC~=0) !Then.
Get x2/file=!path+ 'out2.sav'/VARIABLES=const_ !AVAR !CVAR/MISSING=OMIT.
compute sigbeta=X2(1:(!NC+2),1:(!NC+2)).
compute beta2=X2((!NC+3),3:(!NC+2)).
Get datacov/file=!data/VARIABLES= !CVAR/MISSING=OMIT.
compute n=nrow(datacov).
compute beta0=X2(2+!NC+1,1).
compute beta1=X2(2+!NC+1,2).
compute beta2 =make(1,!NC,0).
Loop i=1 To (!NC).
compute beta2(1,i)=X2(estbeta,(i+nvbeta)).
End Loop.
Loop I=1TO (!NC).
compute meanc(1,I)=CSUM(datacov(:,I))/n.
End Loop.
		!IF (!c~=NA) !THEN.
Get c/file=!c/MISSING=OMIT.
compute cval(1,1:!NC)=c(1,:).
				!IFEND.
!IFEND.
!If (!NC=0) !Then.
Get x2/file=!path+ 'out2.sav'/VARIABLES=const_ !AVAR /MISSING=OMIT.
compute sigbeta=X2(1:(2),1:(2)).
compute beta0=X2(2+1,1).
compute beta1=X2(2+1,2).
!IFEND.
!IFEND.

!If ( !mreg=LOGISTIC) !Then.
!If (!NC~=0) !Then.
Get x2/file=!path+ 'out21.sav'/VARIABLES=B /MISSING=OMIT.
COMPUTE beta2=t(X2(3:(3+!NC-1),1)).
Get datacov/file=!data/VARIABLES= !CVAR/MISSING=OMIT.
compute n=nrow(datacov).
compute beta0=X2(2+!NC+1,1).
compute beta1=X2(2,1).
compute beta2 =make(1,!NC,0).
Loop i=1 To (!NC).
compute beta2(1,i)=X2(2+i,1).
End Loop.
Loop I=1TO (!NC).
compute meanc(1,I)=CSUM(datacov(:,I))/n.
End Loop.
				!IF (!c~=NA) !THEN.
Get c/file=!c/MISSING=OMIT.
compute cval(1,1:!NC)=c(1,:).
				!IFEND.
get corr2 /file=!path+ 'corr2.sav'
 /variables=constant !avar !cvar 
/missing = OMIT.
get se /file=!path+ 'out21.sav'
/variables =  S.E.
compute se0=se(2+!NC+1,1).
compute se1=se(2,1).
compute row=nrow(se)-1.
compute se2=se(3:row,1).
compute stder={se0;se1;se2}.
compute sigma=mdiag(stder).
compute CO2 = sigma*corr2*sigma.
compute sigbeta=co2.

!IFEND.



!If (!NC=0) !Then.
Get x2/file=!path+ 'out21.sav'/VARIABLES=B/MISSING=OMIT.
compute beta0=X2(2+1,1).
compute beta1=X2(2,1).
get corr2 /file=!path+ 'corr2.sav'
 /variables=constant !avar /missing = OMIT.
get se /file=!path+ 'out21.sav'
/variables =  S.E.
compute se0=se(2+1,1).
compute se1=se(2,1).
compute stder={se0;se1}.
compute sigma=mdiag(stder).
compute CO2 = sigma*corr2*sigma.
compute sigbeta=co2.
!IFEND.
!IFEND.




*/Y LINEAR M LINEAR CASE*/.

!If (!interaction=TRUE & !yreg=LINEAR & !mreg=LINEAR & !NC~=0 ) !Then.
*/compute the effects*/.

!IF (!c~=NA) !THEN.
Loop i=1 To !NC.
*/CONDITIONAL CDE*/.
compute effect1=(theta1)*(!a1-!a0)+(theta3*(!m))*(!a1-!a0).   
      */CONDITIONAL NDE*/.
compute effect2=(theta1+theta3*beta0+theta3*beta1*!a0+csum(theta3*beta2(1,i)*cval(1,i)))*(!a1-!a0).
      */CONDITIONAL NIE*/.
compute effect3=(theta2*beta1+theta3*beta1*!a0)*(!a1-!a0).
	  */CONDITIONAL TNDE*/.
compute effect4=(theta1+theta3*beta0+theta3*beta1*!a1+csum(theta3*beta2(1,i)*cval(1,i)))*(!a1-!a0).
	  */ CONDITIONAL TNIE*/.
compute effect5=(theta2*beta1+theta3*beta1*!a1)*(!a1-!a0).
End Loop.
!IFEND.
Loop i=1 To !NC.
      */MARGINAL CDE*/.
compute effect6=(theta1+theta3*!m)*(!a1-!a0).
	  */MARGINAL NDE*/.
compute effect7=(theta1+theta3*beta0+theta3*beta1*!a0+csum(theta3*beta2(1,i)*meanc(1,i)))*(!a1-!a0).
	  */MARGINAL NIE*/.
compute effect8=(theta2*beta1+theta3*beta1*!a0)*(!a1-!a0).
	  */ MARGINAL TNDE*/.
compute effect9=(theta1+theta3*beta0+theta3*beta1*!a1+csum(theta3*beta2(1,i)*meanc(1,i)))*(!a1-!a0).
	  */ MARGINAL TNIE*/.
compute effect10=(theta2*beta1+theta3*beta1*!a1)*(!a1-!a0).
	  */ TE*/.
compute effect11=(theta1+theta3*beta0+theta3*beta1*!a0+csum(theta3*beta2(1,i)*meanc(1,i))+theta2*beta1+theta3*beta1*!a1)*(!a1-!a0).
	  */ PM*/.
compute effect12=(theta2*beta1+theta3*beta1*!a1)/(theta1+theta3*beta0+theta3*beta1*!a0+csum(theta3*beta2(1,i)*meanc(1,i))+theta2*beta1+theta3*beta1*!a1).
End Loop.
!IF (!c~=NA) !THEN.
Loop i=1 To !NC.
compute effect13=(theta1+theta3*beta0+theta3*beta1*!a0+csum(theta3*beta2(1,i)*cval(1,i))+theta2*beta1+theta3*beta1*!a1)*(!a1-!a0).
End Loop.
!IFEND.

*/compute the standard errors*/.
compute z1=make((!NC+4),(!NC+2),0).
compute z2=make((!NC+2),(!NC+4),0).
compute sigma1={sigbeta,z2}.
compute sigma2={z1,sigtheta}.
compute sigma={sigma1;sigma2}.
compute cmean=(csum(DATACOV)/N).
compute beta2=X2((!NC+3),3:(!NC+2)).
compute zeros=make(1,!NC,0).
!IF (!c~=NA) !THEN.
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute d8=zeros.
compute gamma1={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=theta3.
compute d2=theta3*!a0.
compute d3=T(theta3*t(cval(1,:))).
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=T(beta0+beta1*!a0+(beta2)*t(cval(1,:))).
compute d8=zeros.
compute gamma2={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=0.
compute d2=theta2+theta3*!a0.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a0.
compute d8=zeros.
compute gamma3={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=theta3.
compute d2=theta3*!a1.
compute d3=T(theta3*t(cval(1,:))).
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=T(beta0+beta1*!a1+(beta2)*t(cval(1,:))).
compute d8=zeros.
compute gamma4={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=0.
compute d2=theta2+theta3*!a1.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a1.
compute d8=zeros.
compute gamma5={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=theta3.
compute d2=(theta3*!a1+theta3*!a0+theta2).
compute d3=theta3*(cval(1,:)).
compute d4=0.
compute d5=1.
compute d6=beta1.
compute d7=beta0+beta1*(!a1+!a0)+beta2*t(cval(1,:)).
compute d8=zeros.
compute gamma13={d1, d2, d3, d4, d5, d6, d7, d8}.
!IFEND.
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute d8=zeros.
compute gamma6={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=theta3.
compute d2=theta3*!a0.
compute d3=T(theta3*t(cmean)).
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=T(beta0+beta1*!a0+(beta2)*t(cmean)).
compute d8=zeros.
compute gamma7={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=0.
compute d2=theta2+theta3*!a0.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a0.
compute d8=zeros.
compute gamma8={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=theta3.
compute d2=theta3*!a1.
compute d3=T(theta3*t(cmean)).
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=T(beta0+beta1*!a1+(beta2)*t(cmean)).
compute d8=zeros.
compute gamma9={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=0.
compute d2=theta2+theta3*!a1.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a1.
compute d8=zeros.
compute gamma10={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=theta3.
compute d2=(theta3*!a1+theta3*!a0+theta2).
compute d3=theta3*(cmean).
compute d4=0.
compute d5=1.
compute d6=beta1.
compute d7=beta0+beta1*(!a1+!a0)+beta2*t(cmean).
compute d8=zeros.
compute gamma11={d1, d2, d3, d4, d5, d6, d7, d8}.
!IFEND.





!If (!interaction=FALSE & !yreg=LINEAR & !mreg=LINEAR & !NC~=0 ) !Then.
*/compute the effects*/.
*/CDE AND NDE*/.
compute effect1=(theta1)*(!a1-!a0).   
*/NIE*/.
compute effect3=(theta2*beta1)*(!a1-!a0).
		  */ TE*/.
compute effect11=(theta1+theta2*beta1)*(!a1-!a0).
	  */ PM*/.
compute effect12=(theta2*beta1)/(theta1+theta2*beta1).

*/compute the standard errors*/.
compute z1=make((!NC+3),(!NC+2),0).
compute z2=make((!NC+2),(!NC+3),0).
compute sigma1={sigbeta,z2}.
compute sigma2={z1,sigtheta}.
compute sigma={sigma1;sigma2}.
compute cmean=(csum(DATACOV)/N).
compute beta2=X2((!NC+3),3:(!NC+2)).
compute zeros=make(1,!NC,0).
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d8=zeros.
compute gamma1={d1, d2, d3, d4, d5, d6, d8}.
compute d1=0.
compute d2=theta2.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d8=zeros.
compute gamma3={d1, d2, d3, d4, d5, d6,  d8}.
compute d1=0.
compute d2=(theta2).
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=beta1.
compute d8=zeros.
compute gamma11={d1, d2, d3, d4, d5, d6, d8}.
!IFEND.





!If (!interaction=TRUE & !yreg=LINEAR & !mreg=LINEAR & !NC=0 ) !Then.
*/compute the effects*/.
*/  CDE*/.      */  NDE*/.  */  TNDE*/.   */   TNIE*/. */ TE*/.  */ PM*/.
compute effect1=(theta1)*(!a1-!a0)+(theta3*(!m))*(!a1-!a0). 
     
compute effect2=(theta1+theta3*beta0+theta3*beta1*!a0)*(!a1-!a0).
compute effect3=(theta2*beta1+theta3*beta1*!a0)*(!a1-!a0).
	  
compute effect4=(theta1+theta3*beta0+theta3*beta1*!a1)*(!a1-!a0).
	
compute effect5=(theta2*beta1+theta3*beta1*!a1)*(!a1-!a0).
	  
compute effect11=(theta1+theta3*beta0+theta3*beta1*!a0+theta2*beta1+theta3*beta1*!a1)*(!a1-!a0).
	 
compute effect12=(theta2*beta1+theta3*beta1*!a1)/(theta1+theta3*beta0+theta3*beta1*!a0+theta2*beta1+theta3*beta1*!a1).
*/compute the standard errors*/.
compute z1=make((4),(2),0).
compute z2=make((2),(4),0).
compute sigma1={sigbeta,z2}.
compute sigma2={z1,sigtheta}.
compute sigma={sigma1;sigma2}.
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute gamma1={d1, d2, d4, d5, d6, d7}.
compute d1=theta3.
compute d2=theta3*!a0.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=T(beta0+beta1*!a0).
compute gamma2={d1, d2, d4, d5, d6, d7}.
compute d1=0.
compute d2=theta2+theta3*!a0.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a0.
compute gamma3={d1, d2, d4, d5, d6, d7}.
compute d1=theta3.
compute d2=theta3*!a1.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=T(beta0+beta1*!a1).
compute gamma4={d1, d2, d4, d5, d6, d7}.
compute d1=0.
compute d2=theta2+theta3*!a1.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a1.
compute gamma5={d1, d2, d4, d5, d6, d7}.
compute d1=theta3.
compute d2=(theta3*!a1+theta3*!a0+theta2).
compute d4=0.
compute d5=1.
compute d6=beta1.
compute d7=beta0+beta1*(!a1+!a0).
compute gamma11={d1, d2, d4, d5, d6, d7}.
!IFEND.



!If (!interaction=FALSE & !yreg=LINEAR & !mreg=LINEAR & !NC=0 ) !Then.
*/compute the effects*/.
*/ CDE=NDE*/.
compute effect1=(theta1)*(!a1-!a0).   
 */ NIE*/.
compute effect3=(theta2*beta1)*(!a1-!a0).
	  */ TE*/.
compute effect11=(theta1+theta2*beta1)*(!a1-!a0).
	  */ PM*/.
compute effect12=(theta2*beta1)/(theta1+theta2*beta1).
*/compute the standard errors*/.
compute z1=make((3),(2),0).
compute z2=make((2),(3),0).
compute sigma1={sigbeta,z2}.
compute sigma2={z1,sigtheta}.
compute sigma={sigma1;sigma2}.
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute gamma1={d1, d2, d4, d5, d6}.
compute d1=0.
compute d2=theta2.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute gamma3={d1, d2, d4, d5, d6}.
compute d1=0.
compute d2=(theta2).
compute d4=0.
compute d5=1.
compute d6=beta1.
compute gamma11={d1, d2, d4, d5, d6}.
!IFEND.




*/Y LINEAR M BINARY CASE*/.

!If (!interaction=TRUE & !yreg=LINEAR & !mreg=LOGISTIC & !NC~=0 ) !Then.
*/compute the effects*/.

!If (!c~=NA) !Then.
Loop i=1 To !NC.
*/CONDITIONAL CDE*/.
compute effect1=(theta1+theta3*!m)*(!a1-!a0).
*/CONDITIONAL NDE*/.
compute effect2=(theta1+theta3*exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i))))/(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i))))))*(!a1-!a0).
*/CONDITIONAL NIE*/.
compute effect3=(theta2+theta3*!a0)*(
exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i))))/
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i)))))-
exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i))))/
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i)))))
).
*/CONDITIONAL TNDE*/.
compute effect4=(theta1+theta3*exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i))))/(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i))))))*(!a1-!a0).
*/ CONDITIONAL TNIE*/.
compute effect5=(theta2+theta3*!a1)*(
exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i))))/
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i)))))-
exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i))))/
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i)))))
).

*/conditional te*/.
compute effect13=(effect2)+(effect5).
End Loop.
!IFEND.
Loop i=1 To !NC.
*/MARGINAL CDE*/.
compute effect6=(theta1+theta3*!m)*(!a1-!a0).
*/MARGINAL NDE*/.
compute effect7=(theta1+theta3*exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i))))/(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i))))))*(!a1-!a0).
*/MARGINAL NIE*/.
compute effect8=(theta2+theta3*!a0)*(
exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i))))/
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i)))))-
exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i))))/
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i)))))
).
*/ MARGINAL TNDE*/.
compute effect9=(theta1+theta3*exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i))))/(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i))))))*(!a1-!a0).
*/ MARGINAL TNIE*/.
compute effect10=(theta2+theta3*!a1)*(exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i))))/(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i)))))-exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i))))/(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i)))))).
*/marginal te*/.
compute effect11=(effect7)+(effect10).
*/pm*/.
compute effect12=(effect10)/(effect11).
End Loop.

*/compute the standard errors*/.

compute z1=make((!NC+4),(!NC+2),0).
compute z2=make((!NC+2),(!NC+4),0).
compute sigma1={sigbeta,z2}.
compute sigma2={z1,sigtheta}.
compute sigma={sigma1;sigma2}.
compute cmean=(csum(DATACOV)/N).
compute zeros=make(1,!NC,0).
COMPUTE beta2=t(X2(3:(3+!NC-1),1)).
!If (!c~=NA) !Then.
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute d8=zeros.
compute gamma1={d1, d2, d3, d4, d5, d6, d7, d8}.
compute B=exp(beta0+beta1*!a0+beta2*t(cval)).
compute A=(1+B).
compute d1=theta3*(A*B-B**2)/(A**2).
compute d2=theta3*!a0*(A*B-B**2)/(A**2).
compute d3=theta3*cval*(A*B-B**2)/(A**2).
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=t(B/A).
compute d8=zeros.
compute gamma2={d1, d2, d3, d4, d5, d6, d7, d8}.
compute B=exp(beta0+beta1*!a1+beta2*t(cval(1,:))).
compute A=(1+B).
compute d1=theta3*(A*B-B**2)/(A**2).
compute d2=theta3*!a1*(A*B-B**2)/(A**2).
compute d3=theta3*cval*(A*B-B**2)/(A**2).
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=t(B/A).
compute d8=zeros.
compute gamma4={d1, d2, d3, d4, d5, d6, d7, d8}.
compute B=exp(beta0+beta1*!a0+beta2*t(cval)).
compute A=(1+B).
compute D=exp(beta0+beta1*!a1+beta2*t(cval)).
compute X=(1+D).
compute d1=(theta2+theta3*!a0)*((D*X-D**2)/(X**2)-(A*B-B**2)/(A**2)).
compute d2=(theta2+theta3*!a0)*(!a1*(D*X-D**2)/(X**2)-!a0*(A*B-B**2)/(A**2)).
compute d3=cval*(theta2+theta3*!a0)*((D*X-D**2)/(X**2)-(A*B-B**2)/(A**2)).
compute d4=0.
compute d5=0.
compute d6=t(D/X-B/A).
compute d7=!a0*d6.
compute d8=zeros.
compute gamma3={d1, d2, d3, d4, d5, d6, d7, d8}.
compute B=exp(beta0+beta1*!a0+beta2*t(cval)).
compute A=(1+B).
compute D=exp(beta0+beta1*!a1+beta2*t(cval)).
compute X=(1+D).
compute d1=(theta2+theta3*!a1)*((D*X-D**2)/(X**2)-(A*B-B**2)/(A**2)).
compute d2=(theta2+theta3*!a1)*(!a1*(D*X-D**2)/(X**2)-!a0*(A*B-B**2)/(A**2)).
compute d3=cval*(theta2+theta3*!a0)*((D*X-D**2)/(X**2)-(A*B-B**2)/(A**2)).
compute d4=0.
compute d5=0.
compute d6=t(D/X-B/A).
compute d7=!a1*d6.
compute d8=zeros.
compute gamma5={d1, d2, d3, d4, d5, d6, d7, d8}.
compute A=exp(beta0+beta1*!a0+beta2*t(cval)).
compute B=(1+A).
compute D=exp(beta0+beta1*!a1+beta2*t(cval)).
compute E=(1+D).
compute d1=theta3*(!a1-!a0)*(A*B-B**2)/(B**2)+(theta2+theta3*!a0)*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d2=!a0*theta3*(!a1-!a0)*(A*B-B**2)/(B**2)+(theta2+theta3*!a0)*(!a1*(D*E-D**2)/(E**2)-!a0*(A*B-A**2)/(B**2)).
compute d3=theta3*cval*(!a1-!a0)*((A*B-B**2)/(B**2))+(theta2+theta3*!a0)*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=t(D/E-A/B).
compute d7=(!a1-!a0)*t(A/B)+!a0*d6.
compute d8=zeros.
compute gamma13={d1, d2, d3, d4, d5, d6, d7, d8}.
!IFEND.
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute d8=zeros.
compute gamma6={d1, d2, d3, d4, d5, d6, d7, d8}.
compute A=exp(beta0+beta1*!a0+beta2*t(cmean)).
compute B=(1+A).
compute d1=theta3*(A*B-A**2)/(B**2).
compute d2=theta3*!a0*(A*B-A**2)/(B**2).
compute d3=theta3*cmean*(A*B-A**2)/(B**2).
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=t(A/B).
compute d8=zeros.
compute gamma7={d1, d2, d3, d4, d5, d6, d7, d8}.
compute A=exp(beta0+beta1*!a1+beta2*t(cmean)).
compute B=(1+A).
compute d1=theta3*(A*B-A**2)/(B**2).
compute d2=theta3*!a1*(A*B-A**2)/(B**2).
compute d3=theta3*cmean*(A*B-A**2)/(B**2).
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=t(A/B).
compute d8=zeros.
compute gamma9={d1, d2, d3, d4, d5, d6, d7, d8}.
compute A=exp(beta0+beta1*!a0+beta2*t(cmean)).
compute B=(1+A).
compute D=exp(beta0+beta1*!a1+beta2*t(cmean)).
compute E=(1+A).
compute d1=(theta2+theta3*!a0)*((D*E-D**2)/(E**2)-(A*B-A**2)/(B**2)).
compute d2=(theta2+theta3*!a0)*(!a1*(D*E-D**2)/(E**2)-!a0*(A*B-A**2)/(B**2)).
compute d3=cmean*(theta2+theta3*!a1)*((D*E-D**2)/(E**2)-(A*B-A**2)/(B**2)).
compute d4=0.
compute d5=0.
compute d6=t(D/E-A/B).
compute d7=!a0*d6.
compute d8=zeros.
compute gamma8={d1, d2, d3, d4, d5, d6, d7, d8}.
compute d1=(theta2+theta3*!a1)*((D*E-D**2)/(E**2)-(A*B-A**2)/(B**2)).
compute d2=(theta2+theta3*!a1)*(!a1*(D*E-D**2)/(E**2)-!a0*(A*B-A**2)/(B**2)).
compute d3=cmean*(theta2+theta3*!a1)*((D*E-D**2)/(E**2)-(A*B-A**2)/(B**2)).
compute d4=0.
compute d5=0.
compute d6=t(D/E-A/B).
compute d7=!a1*d6.
compute d8=zeros.
compute gamma10={d1, d2, d3, d4, d5, d6, d7, d8}.
compute A=exp(beta0+beta1*!a0+beta2*t(cmean)).
compute B=(1+A).
compute D=exp(beta0+beta1*!a1+beta2*t(cmean)).
compute E=(1+D).
compute d1=theta3*(!a1-!a0)*(A*B-B**2)/(B**2)+(theta2+theta3*!a1)*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d2=!a0*theta3*(!a1-!a0)*(A*B-B**2)/(B**2)+(theta2+theta3*!a1)*(!a1*(D*E-D**2)/(E**2)-!a0*(A*B-A**2)/(B**2)).
compute d3=theta3*cmean*(!a1-!a0)*((A*B-B**2)/(B**2))+(theta2+theta3*!a1)*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=t(D/E-A/B).
compute d7=(!a1-!a0)*t(A/B)+!a1*d6.
compute d8=zeros.
compute gamma11={d1, d2, d3, d4, d5, d6, d7, d8}.

!IFEND.






!If (!interaction=TRUE & !yreg=LINEAR & !mreg=LOGISTIC & !NC=0 ) !Then.
*/compute the effects*/.
*/ CDE*/.
compute effect1=(theta1+theta3*!m)*(!a1-!a0).
*/ NDE*/.
compute effect2=(theta1+theta3*exp(beta0+beta1*!a0)/(1+exp(beta0+beta1*!a0)))*(!a1-!a0).
*/ NIE*/.
compute effect3=(theta2+theta3*!a0)*(
exp(beta0+beta1*!a1)/
(1+exp(beta0+beta1*!a1))-
exp(beta0+beta1*!a0)/
(1+exp(beta0+beta1*!a0))
).
*/ TNDE*/.
compute effect4=(theta1+theta3*exp(beta0+beta1*!a1)/(1+exp(beta0+beta1*!a1)))*(!a1-!a0).
*/  TNIE*/.
compute effect5=(theta2+theta3*!a1)*(
exp(beta0+beta1*!a1)/
(1+exp(beta0+beta1*!a1))-
exp(beta0+beta1*!a0)/
(1+exp(beta0+beta1*!a0))
).
*/ te*/.
compute effect11=(effect2)+(effect5).
*/pm*/.
compute effect12=(effect5)/(effect11).

*/compute the standard errors*/.
compute z1=make((4),(2),0).
compute z2=make((2),(4),0).
compute sigma1={sigbeta,z2}.
compute sigma2={z1,sigtheta}.
compute sigma={sigma1;sigma2}.


compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute gamma1={d1, d2,  d4, d5, d6, d7}.
compute B=exp(beta0+beta1*!a0).
compute A=(1+B).
compute d1=theta3*(A*B-B**2)/A**2.
compute d2=theta3*!a0*(A*B-B**2)/A**2.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=t(B/A).
compute gamma2={d1, d2, d4, d5, d6, d7}.
compute B=exp(beta0+beta1*!a1).
compute A=(1+B).
compute d1=theta3*(A*B-B**2)/A**2.
compute d2=theta3*!a1*(A*B-B**2)/A**2.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=t(B/A).
compute gamma4={d1, d2, d4, d5, d6, d7}.
compute D=exp(beta0+beta1*!a1).
compute X=(1+D).
compute B=exp(beta0+beta1*!a0).
compute A=(1+B).
compute d1=(theta2+theta3*!a0)*((D*X-D**2)/X**2-(A*B-B**2)/A**2).
compute d2=(theta2+theta3*!a0)*(!a1*(D*X-D**2)/X**2-!a0*(A*B-B**2)/A**2).
compute d4=0.
compute d5=0.
compute d6=t(D/X-B/A).
compute d7=!a0*d6.
compute gamma3={d1, d2, d4, d5, d6, d7}.
compute d1=(theta2+theta3*!a1)*((D*X-D**2)/X**2-(A*B-B**2)/A**2).
compute d2=(theta2+theta3*!a1)*(!a1*(D*X-D**2)/X**2-!a0*(A*B-B**2)/A**2).
compute d4=0.
compute d5=0.
compute d6=t(D/X-B/A).
compute d7=!a1*d6.
compute gamma5={d1, d2, d4, d5, d6, d7}.
compute A=exp(beta0+beta1*!a0).
compute B=(1+A).
compute D=exp(beta0+beta1*!a1).
compute E=(1+D).
compute d1=theta3*(!a1-!a0)*(A*B-B**2)/(B**2)+(theta2+theta3*!a1)*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d2=!a0*theta3*(!a1-!a0)*(A*B-B**2)/(B**2)+(theta2+theta3*!a1)*!a1*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=t(D/E-A/B).
compute d7=(!a1-!a0)*t(A/B)+!a1*d6.
compute gamma11={d1, d2, d4, d5, d6, d7}.
compute A=exp(beta0+beta1*!a0).
compute B=(1+A).
compute D=exp(beta0+beta1*!a1).
compute E=(1+D).
compute d1=theta3*(!a1-!a0)*(A*B-B**2)/(B**2)+(theta2+theta3*!a1)*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d2=!a0*theta3*(!a1-!a0)*(A*B-B**2)/(B**2)+(theta2+theta3*!a1)*!a1*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=t(D/E-A/B).
compute d7=(!a1-!a0)*t(A/B)+!a1*d6.
compute gamma11={d1, d2, d4, d5, d6, d7}.

!IFEND.



!If (!interaction=FALSE & !yreg=LINEAR & !mreg=LOGISTIC & !NC~=0 ) !Then.
*/compute the effects*/.

!If (!c~=NA) !Then.
Loop i=1 To !NC.
*/CONDITIONAL CDE*/.
compute effect1=(theta1)*(!a1-!a0).
*/CONDITIONAL NDE*/.
compute effect2=(theta1)*(!a1-!a0).
*/CONDITIONAL NIE*/.
compute effect3=(theta2)*(
exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i))))/
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i)))))-
exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i))))/
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i)))))
).
*/CONDITIONAL TNDE*/.
compute effect4=(
theta1
)*(!a1-!a0).
*/ CONDITIONAL TNIE*/.
compute effect5=(theta2)*(
exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i))))/
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(cval(1,i)))))-
exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i))))/
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(cval(1,i)))))
).
*/conditional te*/.
compute effect13=(effect2)+(effect5).
End Loop.
!IFEND.

Loop i=1 To !nc.
*/MARGINAL CDE*/.
compute effect6=(theta1)*(!a1-!a0).
*/MARGINAL NDE*/.
compute effect7=(theta1)*(!a1-!a0).
*/MARGINAL NIE*/.
compute effect8=(theta2)*(
exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i))))/
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i)))))-
exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i))))/
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i)))))
).
*/ MARGINAL TNDE*/.
compute effect9=(theta1)*(!a1-!a0).
*/ MARGINAL TNIE*/.
compute effect10=(theta2)*(exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i))))/(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*(meanc(1,i)))))-exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i))))/(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*(meanc(1,i)))))).
*/marginal te*/.
compute effect11=(effect7)+(effect10).
*/pm*/.
compute effect12=(effect10)/(effect11).
End Loop.

*/compute the standard errors*/.
compute z1=make((!NC+3),(!NC+2),0).
compute z2=make((!NC+2),(!NC+3),0).
compute sigma1={sigbeta,z2}.
compute sigma2={z1,sigtheta}.
compute sigma={sigma1;sigma2}.
compute cmean=(csum(DATACOV)/N).
compute beta2=t(X2(3:(3+!NC-1),1)).

compute zeros=make(1,!NC,0).

!If (!c~=NA) !Then.
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d8=zeros.
compute gamma1={d1, d2, d3, d4, d5, d6, d8}.
compute d1nde=0.
compute d2nde=0.
compute d3nde=zeros.
compute d4nde=0.
compute d5nde=1.
compute d6nde=0.
compute d8nde=zeros.
compute gamma2={d1nde, d2nde, d3nde, d4nde, d5nde, d6nde, d8nde}.
compute gamma4={d1nde, d2nde, d3nde, d4nde, d5nde, d6nde, d8nde}.
compute B=exp(beta0+beta1*!a0+beta2*t(cval(1,:))).
compute A=(1+B).
compute D=exp(beta0+beta1*!a1+beta2*t(cval(1,:))).
compute X=(1+D).
compute d1nie=(theta2)*((D*X-D**2)/X**2-(A*B-B**2)/A**2).
compute d2nie=(theta2)*(!a1*(D*X-D**2)/X**2-!a0*(A*B-B**2)/A**2).
compute d3nie=cval(1,:)*(theta2)*((D*X-D**2)/X**2-(A*B-B**2)/A**2).
compute d4nie=0.
compute d5nie=0.
compute d6nie=t(D/X-B/A).
compute d8nie=zeros.
compute gamma3={d1nie, d2nie, d3nie, d4nie, d5nie, d6nie, d8nie}.
compute gamma5={d1nie, d2nie, d3nie, d4nie, d5nie, d6nie, d8nie}.
compute d1=d1nde+d1nie.
compute d2=d2nde+d2nie.
compute d3=d3nde+d3nie.
compute d4=d4nde+d4nie.
compute d5=(!a1-!a0).
compute d1=d6nde+d6nie.
compute d8=zeros.
compute gamma13={d1, d2, d3, d4, d5, d6,  d8}.

!IFEND.





compute d1nde=0.
compute d2nde=0.
compute d3nde=zeros.
compute d4nde=0.
compute d5nde=1.
compute d6nde=0.
compute d8nde=zeros.
compute gamma6={d1nde, d2nde, d3nde, d4nde, d5nde, d6nde, d8nde}.
compute gamma7={d1nde, d2nde, d3nde, d4nde, d5nde, d6nde, d8nde}.
compute gamma9={d1nde, d2nde, d3nde, d4nde, d5nde, d6nde, d8nde}.
compute D=exp(beta0+beta1*!a1+beta2*t(cmean)).
compute E=(1+D).
compute A=exp(beta0+beta1*!a0+beta2*t(cmean)).
compute B=(1+A).
compute d1nie=(theta2)*((D*E-D**2)/E**2-(A*B-A**2)/B**2).
compute d2nie=(theta2)*(!a1*(D*E-D**2)/E**2-!a0*(A*B-A**2)/B**2).
compute d3nie=cmean*(theta2)*((D*E-D**2)/E**2-(A*B-A**2)/B**2).
compute d4nie=0.
compute d5nie=0.
compute d6nie=t(D/E-A/B).
compute d8nie=zeros.
compute gamma8={d1nie, d2nie, d3nie, d4nie, d5nie, d6nie, d8nie}.
compute gamma10={d1nie, d2nie, d3nie, d4nie, d5nie, d6nie, d8nie}.
compute d1=d1nde+d1nie.
compute d2=d2nde+d2nie.
compute d3=d3nde+d3nie.
compute d4=d4nde+d4nie.
compute d5=(!a1-!a0).
compute d1=d6nde+d6nie.
compute d8=zeros.

compute gamma11={d1, d2, d3, d4, d5, d6, d8}.


!IFEND.



!If (!interaction=FALSE & !yreg=LINEAR & !mreg=LOGISTIC & !NC=0 ) !Then.
*/compute the effects*/.


*/ CDE*/.
compute effect1=(theta1)*(!a1-!a0).
*/ NDE*/.
compute effect2=(theta1)*(!a1-!a0).
*/ NIE*/.
compute effect3=(theta2)*(
exp(beta0+beta1*!a1)/
(1+exp(beta0+beta1*!a1))-
exp(beta0+beta1*!a0)/
(1+exp(beta0+beta1*!a0))
).
*/ TNDE*/.
compute effect4=(theta1)*(!a1-!a0).
*/  TNIE*/.
compute effect5=(theta2)*(
exp(beta0+beta1*!a1)/
(1+exp(beta0+beta1*!a1))-
exp(beta0+beta1*!a0)/
(1+exp(beta0+beta1*!a0))
).
*/ te*/.
compute effect11=(effect2)+(effect5).
*/pm*/.
compute effect12=(effect5)/(effect11).


*/compute the standard errors*/.
compute z1=make((3),(2),0).
compute z2=make((2),(3),0).
compute sigma1={sigbeta,z2}.
compute sigma2={z1,sigtheta}.
compute sigma={sigma1;sigma2}.
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute gamma1={d1, d2,  d4, d5, d6}.
compute B=exp(beta0+beta1*!a0).
compute A=(1+B).
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute gamma2={d1, d2, d4, d5, d6}.
compute B=exp(beta0+beta1*!a1).
compute A=(1+B).
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute gamma4={d1, d2, d4, d5, d6}.
compute D=exp(beta0+beta1*!a1).
compute B=exp(beta0+beta1*!a0).
compute A=(1+B).
compute X=(1+D).
compute d1=(theta2)*((D*X-D**2)/X**2-(A*B-B**2)/A**2).
compute d2=(theta2)*(!a1*(D*X-D**2)/X**2-!a0*(A*B-B**2)/A**2).
compute d4=0.
compute d5=0.
compute d6=t(D/X-B/A).
compute gamma3={d1, d2, d4, d5, d6}.
compute d1=(theta2)*((D*X-D**2)/X**2-(A*B-B**2)/A**2).
compute d2=(theta2)*(!a1*(D*X-D**2)/X**2-!a0*(A*B-B**2)/A**2).
compute d4=0.
compute d5=0.
compute d6=t(D/X-B/A).
compute gamma5={d1, d2, d4, d5, d6}.
compute A=exp(beta0+beta1*!a0).
compute B=(1+A).
compute D=exp(beta0+beta1*!a1).
compute E=(1+D).
compute d1=(theta2)*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d2=(theta2)*!a1*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=t(D/E-A/B).
compute gamma11={d1, d2, d4, d5, d6}.
compute A=exp(beta0+beta1*!a0).
compute B=(1+A).
compute D=exp(beta0+beta1*!a1).
compute E=(1+D).
compute d1=(theta2)*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d2=(theta2)*!a1*(((D*E-D**2)/(E**2))-((A*B-B**2)/(B**2))).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=t(D/E-A/B).
compute gamma11={d1, d2, d4, d5, d6}.

!IFEND.



/*Y LOGISTIC M LINEAR/*.


!IF ((!yreg=LOGISTIC & !mreg=LINEAR & !interaction=TRUE & !NC~=0) | (!yreg=LOGLINEAR & !mreg=LINEAR & !interaction=TRUE & !NC~=0) | (!yreg=POISSON & !mreg=LINEAR & !interaction=TRUE & !NC~=0) 
 | (!yreg=NEGBIN & !mreg=LINEAR & !interaction=TRUE & !NC~=0))  !THEN.

compute tsq=(theta3**2).
compute  rm=s2**2.

compute s2=s2**2.
compute  asq=(!a1**2).
compute	 a1sq=(!a0**2).

Loop i=1 To !NC.
!If (!c~=NA) !Then.
	  */CONDITIONAL CDE*/.
	compute  x1=(theta1+theta3*!m)*(!a1-!a0).
 compute  effect1=exp(x1).
      */CONDITIONAL NDE*/.	  
	compute  x2=(theta1+theta3*beta0+theta3*beta1*!a0+csum(theta3*beta2(1,i)*t(cval(1,i)))+theta3*theta2*rm)*(!a1-!a0)+(1/2)*tsq*rm*(asq-a1sq).
	compute  effect2=exp(x2).
      */CONDITIONAL NIE*/.
	compute  x3=(theta2*beta1+theta3*beta1*!a0)*(!a1-!a0).
 compute   effect3=exp(x3).
      */CONDITIONAL TNDE*/.
compute x4=(theta1+theta3*beta0+theta3*beta1*!a1+csum(theta3*beta2(1,i)*t(cval(1,i)))+theta3*theta2*rm)*(!a1-!a0)+(1/2)*tsq*rm*(asq-a1sq).
compute effect4=exp(x4).
      */ CONDITIONAL TNIE*/.
	compute  x5=(theta2*beta1+theta3*beta1*!a1)*(!a1-!a0).
 compute     effect5=exp(x5).
	  !IFEND.
	  */MARGINAL CDE*/.
compute	  x6=(theta1+theta3*!m)*(!a1-!a0).
	compute  effect6=exp(x6).
	   */MARGINAL NDE*/.
	 compute x7=(theta1+theta3*beta0+theta3*beta1*!a0+csum(theta3*beta2(1,i)*t(meanc(1,i)))+theta3*theta2*rm)*(!a1-!a0)+1/2*tsq*rm*(asq-a1sq).
 compute     effect7=exp(x7).
	  */MARGINAL NIE*/.
	compute  x8=(theta2*beta1+theta3*beta1*!a0)*(!a1-!a0).
compute	  effect8=exp(x8).
	  */ MARGINAL TNDE*/.
	 compute x9=(theta1+theta3*beta0+theta3*beta1*!a1+csum(theta3*beta2(1,i)*t(meanc(1,i)))+theta3*theta2*rm)*(!a1-!a0)+1/2*tsq*rm*(asq-a1sq).
	 compute effect9=exp(x9).
	  */ MARGINAL TNIE*/.
	compute  x10=(theta2*beta1+theta3*beta1*!a1)*(!a1-!a0).
	compute  effect10=exp(x10).
!If (!c ~=NA ) !Then.
compute logtecon=(theta1+theta3*beta0+theta3*beta1*!a0+csum(theta3*beta2(1,i)*t(cval(1,i)))+theta2*beta1+theta3*beta1*!a1+theta3*(rm)*theta2)*(!a1-!a0)+0.5*(theta3**2)*(rm)*(!a1**2-!a0**2).
compute effect13=exp(logtecon).
!IFEND.
compute logtemar=(theta1+theta3*beta0+theta3*beta1*!a0+csum(theta3*beta2(1,i)*t(meanc(1,i)))+theta2*beta1+theta3*beta1*!a1+theta3*(rm)*theta2)*(!a1-!a0)+0.5*(theta3**2)*(rm)*(!a1**2-!a0**2).
compute effect11=exp(logtemar).
compute effect12=(effect7)*((effect10)-1)/((effect7)*(effect10)-1).
End Loop.

compute zero1=make((!NC+4),(!NC+2),0).
compute zero2=make((!NC+2),(!NC+4),0).
compute z2=make((!NC+4),1,0).
compute z3=make((!NC+2),1,0).
compute sigma1={sigbeta,zero2,z3}.
compute sigma2={zero1,sigtheta,z2}.
compute z=make(1,(!NC+4)+(!NC+2),0).
compute sigma3={z,rm}.
compute sigma={ sigma1;sigma2;sigma3}.
compute zeros=make(1,!NC,0).
compute cmean=(csum(DATACOV)/N).

!If (!c~=NA) !Then.
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute d8=zeros.
compute d9=0.
compute gamma1={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
compute d1=theta3.
compute d2=theta3*!a0.
compute d3=t(theta3*t(cval(1,:))).
compute d4=0.
compute d5=1.
compute d6=rm*theta3.
compute d7=beta0+beta1*!a0+(beta2)*t(cval)+theta2*rm+theta3*rm*(!a1+!a0).
compute d8=zeros.
compute d9=theta3*theta2+0.5*(theta3**2)*(!a1+!a0).
compute gamma2={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
compute d1=theta3.
compute d2=theta3*!a1.
compute d3=t(theta3*t(cval)).
compute d4=0.
compute d5=1.
compute d6=rm*theta3.
compute d7=beta0+beta1*!a1+(beta2)*t(cval)+theta2*rm+theta3*rm*(!a1+!a0).
compute d8=zeros.
compute d9=theta3*theta2+0.5*(theta3**2)*(!a1+!a0).
compute gamma4={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
compute d1=0.
compute d2=theta2+theta3*!a1.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a1.
compute d8=zeros.
compute d9=0.
compute gamma5={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
compute d1=0.
compute d2=theta2+theta3*!a0.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a0.
compute d8=zeros.
compute d9=0.
compute gamma3={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
!IFEND.
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute d8=zeros.
compute d9=0.
compute gamma6={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
compute d1=theta3.
compute d2=theta3*!a0.
compute d3=t(theta3*t(cmean)).
compute d4=0.
compute d5=1.
compute d6=rm*theta3.
compute d7=beta0+beta1*!a0+(beta2)*t(cmean)+theta2*rm+theta3*rm*(!a1+!a0).
compute d8=zeros.
compute d9=theta3*theta2+0.5*(theta3**2)*(!a1+!a0).
compute gamma7={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
compute d1=theta3.
compute d2=theta3*!a1.
compute d3=t(theta3*t(cmean)).
compute d7=beta0+beta1*!a1+(beta2)*t(cmean)+theta2*rm+theta3*rm*(!a1+!a0).
compute d4=0.
compute d5=1.
compute d6=rm*theta3.
compute d8=zeros.
compute d9=theta3*theta2+0.5*theta3**2*(!a1+!a0).
compute gamma9={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
compute d1=0.
compute d2=theta2+theta3*!a1.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a1.
compute d8=zeros.
compute d9=0.
compute gamma10={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
compute d1=0.
compute d2=theta2+theta3*!a0.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a0.
compute d8=zeros.
compute d9=0.
compute gamma8={d1,d2,d3,d4,d5,d6,d7,d8,d9}.
!If (!c~=NA) !Then.
compute  d2pnde=theta3*!a0.
compute d3pnde=theta3*(cval(1,:)).
compute d7pnde=beta0+beta1*!a0+(beta2)*t(cval(1,:))+theta2*rm+theta3*rm*(!a1+!a0).
compute d6pnde=rm*theta3.
compute d9pnde=theta3*theta2+0.5*(theta3**2)*(!a1+!a0).
compute d2tnie=theta2+theta3*!a1.
compute d7tnie=beta1*!a1.
compute d2=d2pnde+d2tnie.
compute d3=d3pnde.
compute d6=d6pnde+beta1.
compute d7=d7pnde+d7tnie.
compute d9=d9pnde.
compute gamma13={theta3,d2,d3,0,1,d6,d7,zeros,d9}.
!IFEND.
compute d2pnde=theta3*!a0.
compute d3pnde=theta3*(cmean).
compute d7pnde=beta0+beta1*!a0+(beta2)*t(cmean)+theta2*rm+theta3*rm*(!a1+!a0).
compute d6pnde=rm*theta3.
compute d9pnde=theta3*theta2+0.5*(theta3**2)*(!a1+!a0).
compute d2tnie=theta2+theta3*!a1.
compute d7tnie=beta1*!a1.
compute d2=d2pnde+d2tnie.
compute d3=d3pnde.
compute d6=d6pnde+beta1.
compute d7=d7pnde+d7tnie.
compute d9=d9pnde.
compute gamma11={theta3,d2,d3,0,1,d6,d7,zeros,d9}.
!IFEND





!IF ((!yreg=LOGISTIC & !mreg=LINEAR & !interaction=TRUE & !NC=0) |(!yreg=LOGLINEAR & !mreg=LINEAR & !interaction=TRUE & !NC=0) | (!yreg=POISSON & !mreg=LINEAR & !interaction=TRUE & !NC=0)
 | (!yreg=NEGBIN & !mreg=LINEAR & !interaction=TRUE & !NC=0)) !THEN.

compute tsq=(theta3**2).
compute  rm=s2**2.
compute s2=s2**2.
compute  asq=(!a1**2).
compute	 a1sq=(!a0**2).

	  */ CDE*/.
	compute  x1=(theta1+theta3*!m)*(!a1-!a0).
 compute  effect1=exp(x1).
      */ NDE*/.	  
	compute  x2=(theta1+theta3*beta0+theta3*beta1*!a0+theta3*theta2*rm)*(!a1-!a0)+(1/2)*tsq*rm*(asq-a1sq).
	compute  effect2=exp(x2).
      */ NIE*/.
	compute  x3=(theta2*beta1+theta3*beta1*!a0)*(!a1-!a0).
 compute   effect3=exp(x3).
      */ TNDE*/.
compute x4=(theta1+theta3*beta0+theta3*beta1*!a1+theta3*theta2*rm)*(!a1-!a0)+(1/2)*tsq*rm*(asq-a1sq).
compute effect4=exp(x4).
      */  TNIE*/.
	compute  x5=(theta2*beta1+theta3*beta1*!a1)*(!a1-!a0).
 compute effect5=exp(x5).
compute logtemar=(theta1+theta3*beta0+theta3*beta1*!a0+theta2*beta1+theta3*beta1*!a1+theta3*(rm)*theta2)*(!a1-!a0)+0.5*(theta3**2)*(rm)*(!a1**2-!a0**2).
compute effect11=exp(logtemar).
compute effect12=(effect2)*((effect5)-1)/((effect2)*(effect5)-1).

compute zero1=make((4),(2),0).
compute zero2=make((2),(4),0).
compute z2=make((4),1,0).
compute z3=make((2),1,0).
compute sigma1={sigbeta,zero2,z3}.
compute sigma2={zero1,sigtheta,z2}.
compute z=make(1,(4)+(2),0).
compute sigma3={z,rm}.
compute sigma={ sigma1;sigma2;sigma3}.
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute d9=0.
compute gamma1={d1,d2,d4,d5,d6,d7,d9}.
compute d1=theta3.
compute d2=theta3*!a0.
compute d4=0.
compute d5=1.
compute d6=s2*theta3.
compute d7=beta0+beta1*!a0+theta2*s2+theta3*s2*(!a1+!a0).
compute d9=theta3*theta2+0.5*(theta3**2)*(!a1+!a0).
compute gamma2={d1,d2,d4,d5,d6,d7,d9}.
compute d1=theta3.
compute d2=theta3*!a1.
compute d4=0.
compute d5=1.
compute d6=s2*theta3.
compute d7=beta0+beta1*!a1+theta2*s2+theta3*s2*(!a1+!a0).
compute d9=theta3*theta2+0.5*theta3**2*(!a1+!a0).
compute gamma4={d1,d2,d4,d5,d6,d7,d9}.
compute d1=0.
compute d2=theta2+theta3*!a1.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a1.
compute d9=0.
compte gamma5={d1,d2,d4,d5,d6,d7,d9}.
compute d1=0.
compute d2=theta2+theta3*!a0.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d7=beta1*!a0.
compute d9=0.
compute gamma3={d1,d2,d4,d5,d6,d7,d9}.
compute d2pnde=0.
compute d7pnde=beta0+beta1*!a0+theta2*s2+theta3*s2*(!a1+!a0).
compute d6pnde=s2*theta3.
compute d9pnde=theta3*theta2+0.5*(theta3**2)*(!a1+!a0).
compute d2tnie=theta2+theta3*!a1.
compute d7tnie=beta1*!a1.
compute d2=d2pnde+d2tnie.
compute d6=d6pnde+beta1.
compute d7=d7pnde+d7tnie.
compute d9=d9pnde.
compute gamma11={theta3,d2,0,1,d6,d7,d9}.
!IFEND







!IF ((!yreg=LOGISTIC & !mreg=LINEAR & !interaction=FALSE & !NC~=0) | (!yreg=LOGLINEAR & !mreg=LINEAR & !interaction=FALSE & !NC~=0) | (!yreg=POISSON & !mreg=LINEAR & !interaction=FALSE & !NC~=0) 
 | (!yreg=NEGBIN & !mreg=LINEAR & !interaction=FALSE & !NC~=0)) !THEN.


compute  rm=s2**2.
compute s2=s2**2.
compute  asq=(!a1**2).
compute	 a1sq=(!a0**2).

	  */ CDE*/.
	compute  x1=(theta1)*(!a1-!a0).
 compute  effect1=exp(x1).
      */ NDE*/.	  
	compute  x2=(theta1)*(!a1-!a0).
	compute  effect2=exp(x2).
      */ NIE*/.
	compute  x3=(theta2*beta1)*(!a1-!a0).
 compute   effect3=exp(x3).
      */ TNDE*/.
compute x4=(theta1)*(!a1-!a0).
compute effect4=exp(x4).
      */  TNIE*/.
	compute  x5=(theta2*beta1)*(!a1-!a0).
 compute     effect5=exp(x5).
compute logtemar=(theta1+theta2*beta1)*(!a1-!a0).
compute effect11=exp(logtemar).
compute effect12=(effect2)*((effect5)-1)/((effect2)*(effect5)-1).


compute zero1=make((!NC+3),(!NC+2),0).
compute zero2=make((!NC+2),(!NC+3),0).
compute z2=make((!NC+3),1,0).
compute z3=make((!NC+2),1,0).
compute sigma1={sigbeta,zero2,z3}.
compute sigma2={zero1,sigtheta,z2}.
compute z=make(1,(!NC+3)+(!NC+2),0).
compute sigma3={z,rm}.
compute sigma={ sigma1;sigma2;sigma3}.
compute zeros=make(1,!NC,0).
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d8=zeros.
compute d9=0.
compute gamma1={d1,d2,d3,d4,d5,d6,d8,d9}.
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d8=zeros.
compute d9=0.
compute gamma2={d1,d2,d3,d4,d5,d6,d8,d9}.
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d8=zeros.
compute d9=0.
compute gamma4={d1,d2,d3,d4,d5,d6,d8,d9}.
compute d1=0.
compute d2=theta2.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d8=zeros.
compute d9=0.
compute gamma5={d1,d2,d3,d4,d5,d6,d8,d9}.
compute d1=0.
compute d2=theta2.
compute d3=zeros.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d8=zeros.
compute d9=0.
compute gamma3={d1,d2,d3,d4,d5,d6,d8,d9}.
compute d2pnde=0.
compute d3pnde=zeros.
compute d6pnde=0.
compute d9pnde=0.
compute d2tnie=theta2.
compute d2=d2pnde+d2tnie.
compute d3=d3pnde.
compute d6=d6pnde+beta1.
compute d9=d9pnde.
compute gamma11={0,d2,d3,0,1,d6,zeros,d9}.
!IFEND


!IF ((!yreg=LOGISTIC & !mreg=LINEAR & !interaction=FALSE & !NC=0) | (!yreg=LOGLINEAR & !mreg=LINEAR & !interaction=FALSE & !NC=0) | (!yreg=POISSON & !mreg=LINEAR & !interaction=FALSE & !NC=0)
 | (!yreg=NEGBIN & !mreg=LINEAR & !interaction=FALSE & !NC=0)) !THEN.

compute  rm=s2**2.
compute s2=s2**2.
compute  asq=(!a1**2).
compute	 a1sq=(!a0**2).


	  */ CDE*/.
	compute  x1=(theta1)*(!a1-!a0).
 compute  effect1=exp(x1).
      */ NDE*/.	  
	compute  x2=(theta1)*(!a1-!a0).
	compute  effect2=exp(x2).
      */ NIE*/.
	compute  x3=(theta2*beta1)*(!a1-!a0).
 compute   effect3=exp(x3).
      */ TNDE*/.
compute x4=(theta1)*(!a1-!a0).
compute effect4=exp(x4).
      */  TNIE*/.
	compute  x5=(theta2*beta1)*(!a1-!a0).
 compute     effect5=exp(x5).
compute logtemar=(theta1+theta2*beta1)*(!a1-!a0).
compute effect11=exp(logtemar).
compute effect12=(effect2)*((effect5)-1)/((effect2)*(effect5)-1).


compute zero1=make((3),(2),0).
compute zero2=make((2),(3),0).
compute z2=make((3),1,0).
compute z3=make((2),1,0).
compute sigma1={sigbeta,zero2,z3}.
compute sigma2={zero1,sigtheta,z2}.
compute z=make(1,(3)+(2),0).
compute sigma3={z,rm}.
compute sigma={ sigma1;sigma2;sigma3}.
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute d9=0.
compute gamma1={d1,d2,d4,d5,d6,d9}.
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute d9=0.
compute gamma2={d1,d2,d4,d5,d6,d9}.
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute d9=0.
compute gamma4={d1,d2,d4,d5,d6,d9}.
compute d1=0.
compute d2=theta2.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d9=0.
compute gamma5={d1,d2,d4,d5,d6,d9}.
compute d1=0.
compute d2=theta2.
compute d4=0.
compute d5=0.
compute d6=beta1.
compute d9=0.
compute gamma3={d1,d2,d4,d5,d6,d9}.
compute d2pnde=0.
compute d6pnde=0.
compute d9pnde=0.
compute d2tnie=theta2.
compute d2=d2pnde+d2tnie.
compute d6=d6pnde+beta1.
compute d9=d9pnde.
compute gamma11={0,d2,0,1,d6,d9}.
!IFEND.




/*Y LOGISTIC M LOGISTIC/*.



!If ((!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=TRUE & !NC=0) 
 | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=TRUE & !NC=0)) !Then.

*/ CDE*/.
compute x1=exp((theta1+theta3*!m)*(!a1-!a0)).
compute effect1=x1.
*/ NDE*/.
compute effect2=exp(theta1*(!a1-!a0))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a0))/
(1+exp(theta2+theta3*!a0+beta0+beta1*!a0)).
*/ NIE*/.
compute effect3=(
(1+exp(beta0+beta1*!a0))*
(1+exp(theta2+theta3*!a0+beta0+beta1*!a1))
)/
(
(1+exp(beta0+beta1*!a1))*
(1+exp(theta2+theta3*!a0+beta0+beta1*!a0))
).
*/ TNDE*/.
compute effect4=exp(theta1*(!a1-!a0))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a1))/
(1+exp(theta2+theta3*!a0+beta0+beta1*!a1)).

compute effect5=(
(1+exp(beta0+beta1*!a0))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a1))
)/
(
(1+exp(beta0+beta1*!a1))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a0))
).

compute logtemar=ln((effect2)*(effect5)).
compute effect11=(effect2)*(effect5).
compute effect12=(effect2)*((effect5)-1)/((effect2)*(effect5)-1).


compute zero1=make((4),(2),0).
compute zero2=make((2),(4),0).
compute sigma1={sigbeta,zero2}.
compute sigma2={zero1,sigtheta}.
compute sigma={ sigma1;sigma2}.

compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute gamma1={d1,d2,d4,d5,d6,d7}.
compute A=exp(theta2+theta3*!a1+beta0+beta1*!a0).
compute B=(1+exp(theta2+theta3*!a1+beta0+beta1*!a0)).
compute D=exp(theta2+theta3*!a0+beta0+beta1*!a0).
compute E=(1+exp(theta2+theta3*!a0+beta0+beta1*!a0)).
compute d1nde=A/B-D/E.
compute d2nde=!a0*(d1nde).
compute d4nde=0.
compute d5nde=(!a1-!a0).
compute d6nde=d1nde.
compute d7nde=!a1*A/B-!a0*D/E.
compute gamma2={d1nde,d2nde,d4nde,d5nde,d6nde,d7nde}.
compute A=exp(theta2+theta3*!a1+beta0+beta1*!a1).
compute B=(1+exp(theta2+theta3*!a1+beta0+beta1*!a1)).
compute D=exp(theta2+theta3*!a0+beta0+beta1*!a1).
compute E=(1+exp(theta2+theta3*!a0+beta0+beta1*!a1)).
compute d1=A/B-D/E.
compute d2=!a1*(d1).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=d1.
compute d7=!a1*A/B-!a0*D/E.
compute gamma4={d1,d2,d4,d5,d6,d7}.
compute A=exp(theta2+theta3*!a1+beta0+beta1*!a1).
compute B=(1+exp(theta2+theta3*!a1+beta0+beta1*!a1)).
compute D=exp(theta2+theta3*!a1+beta0+beta1*!a0).
compute E=(1+exp(theta2+theta3*!a1+beta0+beta1*!a0)).
compute F=exp(beta0+beta1*!a0).
compute G=(1+exp(beta0+beta1*!a0)).
compute H=exp(beta0+beta1*!a1).
compute I=(1+exp(beta0+beta1*!a1)).
compute d1nie=F/G-H/I+A/B-D/E.
compute d2nie=!a0*F/G-!a1*H/I+!a1*A/B-!a0*D/E.
compute d4nie=0.
compute d5nie=0.
compute d6nie=A/B-D/E.
compute d7nie=!a1*(A/B-D/E).
compute gamma5={d1nie,d2nie,d4nie,d5nie,d6nie,d7nie}.
compute A=exp(theta2+theta3*!a0+beta0+beta1*!a1).
compute B=(1+exp(theta2+theta3*!a0+beta0+beta1*!a1)).
compute D=exp(theta2+theta3*!a0+beta0+beta1*!a0).
compute E=(1+exp(theta2+theta3*!a0+beta0+beta1*!a0)).
compute F=exp(beta0+beta1*!a0).
compute G=(1+exp(beta0+beta1*!a0)).
compute H=exp(beta0+beta1*!a1).
compute I=(1+exp(beta0+beta1*!a1)).
compute d1=F/G-H/I+A/B-D/E.
compute d2=!a1*F/G-!a0*H/I+!a0*A/B-!a1*D/E.
compute d4=0.
compute d5=0.
compute d6=A/B-D/E.
compute d7=!a0*(A/B-D/E).
compute gamma3={d1,d2,d4,d5,d6,d7}.
compute d1=((d1nie)+(d1nde)).
compute d2=((d2nie)+(d2nde)).
compute d4=((d4nie)+(d4nde)).
compute d5=((d5nie)+(d5nde)).
compute d6=((d6nie)+(d6nde)).
compute d7=((d7nie)+(d7nde)).
compute gamma11={d1,d2,d4,d5,d6,d7}.

!IFEND.




!If ((!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=TRUE & !NC~=0)  | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=TRUE & !NC~=0) | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=TRUE & !NC~=0)
 | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=TRUE & !NC~=0)) !Then.

Loop i=1 To !NC.
!If (!c~=NA) !Then.

*/CONDITIONAL CDE*/.
compute x1=exp((theta1+theta3*!m)*(!a1-!a0)).
compute effect1=x1.
*/CONDITIONAL NDE*/.
compute effect2=exp(theta1*(!a1-!a0))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))/
(1+exp(theta2+theta3*!a0+beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i))))).
*/CONDITIONAL NIE*/.
compute effect3=(
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))*
(1+exp(theta2+theta3*!a0+beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))
)/
(
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))*
(1+exp(theta2+theta3*!a0+beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))
).
*/CONDITIONAL TNDE*/.
compute effect4=exp(theta1*(!a1-!a0))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))/
(1+exp(theta2+theta3*!a0+beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i))))).
compute effect5=(
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))
)/
(
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))
).
!IFEND.

*/MARGINAL CDE*/.
compute x6=(theta1+theta3*!m)*(!a1-!a0).
compute effect6=exp(x6).
*/MARGINAL NDE*/.
compute effect7=exp(theta1*(!a1-!a0))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i)))))/
(1+exp(theta2+theta3*!a0+beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i))))).
*/MARGINAL NIE*/.
compute effect8=(
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i)))))*
(1+exp(theta2+theta3*!a0+beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))
)/
(
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))*
(1+exp(theta2+theta3*!a0+beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i)))))
).

*/ MARGINAL TNDE*/.
compute effect9=exp(theta1*(!a1-!a0))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))/
(1+exp(theta2+theta3*!a0+beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i))))).

*/ MARGINAL TNIE*/.
compute effect10=(
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i)))))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))
)/
(
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))*
(1+exp(theta2+theta3*!a1+beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i)))))
).

!If (!c~=NA) !Then.


compute effect13=(effect2)*(effect5).
compute logtecon=ln(effect13).

!IFEND.


compute effect11=(effect7)*(effect10).
compute logtemar=ln((effect7)*(effect10)).

compute effect12=(effect7)*((effect10)-1)/((effect7)*(effect10)-1).
End Loop.

compute zero1=make((!NC+4),(!NC+2),0).
compute zero2=make((!NC+2),(!NC+4),0).
compute sigma1={sigbeta,zero2}.
compute sigma2={zero1,sigtheta}.
compute sigma={ sigma1;sigma2}.
compute zeros=make(1,!NC,0).
compute beta2=t(X2(3:(3+!NC-1),1)).
compute cmean=(csum(DATACOV)/N).
compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d7=!m.
compute d8=zeros.
compute gamma1={d1,d2,d3,d4,d5,d6,d7,d8}.

!If (!c~=NA) !Then.
compute A=exp(theta2+theta3*!a1+beta0+beta1*!a0+beta2*t(cval)).
compute B=(1+exp(theta2+theta3*!a1+beta0+beta1*!a0+beta2*t(cval))).
compute D=exp(theta2+theta3*!a0+beta0+beta1*!a0+beta2*t(cval)).
compute E=(1+exp(theta2+theta3*!a0+beta0+beta1*!a0+beta2*t(cval))).
compute d1cnde=A/B-D/E.
compute d2cnde=!a0*(d1cnde).
compute d3cnde=(d1cnde)*(cval).
compute d4cnde=0.
compute d5cnde=(!a1-!a0).
compute d6cnde=d1cnde.
compute d7cnde=!a1*A/B-!a0*D/E.
compute d8cnde=zeros.
compute gamma2={d1cnde,d2cnde,d3cnde,d4cnde,d5cnde,d6cnde,d7cnde,d8cnde}.
compute A=exp(theta2+theta3*!a1+beta0+beta1*!a1+beta2*t(cval)).
compute B=(1+exp(theta2+theta3*!a1+beta0+beta1*!a1+beta2*t(cval))).
compute D=exp(theta2+theta3*!a0+beta0+beta1*!a1+beta2*t(cval)).
compute E=(1+exp(theta2+theta3*!a0+beta0+beta1*!a1+beta2*t(cval))).
compute d1=A/B-D/E.
compute d2=!a1*(d1).
compute d3=(d1)*(cval).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=d1.
compute d7=!a1*A/B-!a0*D/E.
compute d8=zeros.
compute gamma4={d1,d2,d3,d4,d5,d6,d7,d8}.
compute A=exp(theta2+theta3*!a1+beta0+beta1*!a1+beta2*t(cval)).
compute B=(1+exp(theta2+theta3*!a1+beta0+beta1*!a1+beta2*t(cval))).
compute D=exp(theta2+theta3*!a1+beta0+beta1*!a0+beta2*t(cval)).
compute E=(1+exp(theta2+theta3*!a1+beta0+beta1*!a0+beta2*t(cval))).
compute F=exp(beta0+beta1*!a0+beta2*t(cval)).
compute G=(1+exp(beta0+beta1*!a0+beta2*t(cval))).
compute H=exp(beta0+beta1*!a1+beta2*t(cval)).
compute I=(1+exp(beta0+beta1*!a1+beta2*t(cval))).
compute d1cnie=F/G-H/I+A/B-D/E.
compute d2cnie=!a0*F/G-!a1*H/I+!a1*A/B-!a0*D/E.
compute d3cnie=cval*(d1cnie).
compute d4cnie=0.
compute d5cnie=0.
compute d6cnie=A/B-D/E.
compute d7cnie=!a1*(A/B-D/E).
compute d8cnie=zeros.
compute gamma5={d1cnie,d2cnie,d3cnie,d4cnie,d5cnie,d6cnie,d7cnie,d8cnie}.
compute A=exp(theta2+theta3*!a0+beta0+beta1*!a1+beta2*t(cval)).
compute B=(1+exp(theta2+theta3*!a0+beta0+beta1*!a1+beta2*t(cval))).
compute D=exp(theta2+theta3*!a0+beta0+beta1*!a0+beta2*t(cval)).
compute E=(1+exp(theta2+theta3*!a0+beta0+beta1*!a0+beta2*t(cval))).
compute F=exp(beta0+beta1*!a0+beta2*t(cval)).
compute G=(1+exp(beta0+beta1*!a0+beta2*t(cval))).
compute H=exp(beta0+beta1*!a1+beta2*t(cval)).
compute I=(1+exp(beta0+beta1*!a1+beta2*t(cval))).
compute d1=F/G-H/I+A/B-D/E.
compute d2=!a1*F/G-!a0*H/I+!a0*A/B-!a1*D/E.
compute d3=cval*(d1).
compute d4=0.
compute d5=0.
compute d6=A/B-D/E.
compute d7=!a0*(A/B-D/E).
compute d8=zeros.
compute gamma3={d1,d2,d3,d4,d5,d6,d7,d8}.

!IFEND.

compute gamma6=gamma1.
compute A=exp(theta2+theta3*!a1+beta0+beta1*!a0+beta2*t(cmean)).
compute B=(1+exp(theta2+theta3*!a1+beta0+beta1*!a0+beta2*t(cmean))).
compute D=exp(theta2+theta3*!a0+beta0+beta1*!a0+beta2*t(cmean)).
compute E=(1+exp(theta2+theta3*!a0+beta0+beta1*!a0+beta2*t(cmean))).
compute d1nde=A/B-D/E.
compute d2nde=!a0*(d1nde).
compute d3nde=(d1nde)*(Cmean).
compute d4nde=0.
compute d5nde=(!a1-!a0).
compute d6nde=d1nde.
compute d7nde=!a1*A/B-!a0*D/E.
compute d8nde=zeros.
compute gamma7={d1nde,d2nde,d3nde,d4nde,d5nde,d6nde,d7nde,d8nde}.
compute A=exp(theta2+theta3*!a1+beta0+beta1*!a1+beta2*t(cmean)).
compute B=(1+exp(theta2+theta3*!a1+beta0+beta1*!a1+beta2*t(Cmean))).
compute D=exp(theta2+theta3*!a0+beta0+beta1*!a1+beta2*t(Cmean)).
compute E=(1+exp(theta2+theta3*!a0+beta0+beta1*!a1+beta2*t(Cmean))).
compute d1=A/B-D/E.
compute d2=!a1*(d1).
compute d3=(d1)*(Cmean).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=d1.
compute d7=!a1*A/B-!a0*D/E.
compute d8=zeros.
compute gamma9={d1,d2,d3,d4,d5,d6,d7,d8}.
compute A=exp(theta2+theta3*!a1+beta0+beta1*!a1+beta2*t(Cmean)).
compute B=(1+exp(theta2+theta3*!a1+beta0+beta1*!a1+beta2*t(Cmean))).
compute D=exp(theta2+theta3*!a1+beta0+beta1*!a0+beta2*t(Cmean)).
compute E=(1+exp(theta2+theta3*!a1+beta0+beta1*!a0+beta2*t(Cmean))).
compute F=exp(beta0+beta1*!a0+beta2*t(Cmean)).
compute G=(1+exp(beta0+beta1*!a0+beta2*t(Cmean))).
compute H=exp(beta0+beta1*!a1+beta2*t(Cmean)).
compute I=(1+exp(beta0+beta1*!a1+beta2*t(Cmean))).
compute d1nie=F/G-H/I+A/B-D/E.
compute d2nie=!a0*F/G-!a1*H/I+!a1*A/B-!a0*D/E.
compute d3nie=Cmean*(d1nie).
compute d4nie=0.
compute d5nie=0.
compute d6nie=A/B-D/E.
compute d7nie=!a1*(A/B-D/E).
compute d8nie=zeros.
compute gamma10={d1nie,d2nie,d3nie,d4nie,d5nie,d6nie,d7nie,d8nie}.
compute A=exp(theta2+theta3*!a0+beta0+beta1*!a1+beta2*t(Cmean)).
compute B=(1+exp(theta2+theta3*!a0+beta0+beta1*!a1+beta2*t(Cmean))).
compute D=exp(theta2+theta3*!a0+beta0+beta1*!a0+beta2*t(Cmean)).
compute E=(1+exp(theta2+theta3*!a0+beta0+beta1*!a0+beta2*t(Cmean))).
compute F=exp(beta0+beta1*!a0+beta2*t(Cmean)).
compute G=(1+exp(beta0+beta1*!a0+beta2*t(Cmean))).
compute H=exp(beta0+beta1*!a1+beta2*t(Cmean)).
compute I=(1+exp(beta0+beta1*!a1+beta2*t(Cmean))).
compute d1=F/G-H/I+A/B-D/E.
compute d2=!a1*F/G-!a0*H/I+!a0*A/B-!a1*D/E.
compute d3=Cmean*(d1).
compute d4=0.
compute d5=0.
compute d6=A/B-D/E.
compute d7=!a0*(A/B-D/E).
compute d8=zeros.
compute gamma8={d1,d2,d3,d4,d5,d6,d7,d8}.

!If (!c~=NA) !Then.
compute d1=((d1cnie)+(d1cnde)).
compute d2=((d2cnie)+(d2cnde)).
compute d3=((d3cnie)+(d3cnde)).
compute d4=((d4cnie)+(d4cnde)).
compute d5=((d5cnie)+(d5cnde)).
compute d6=((d6cnie)+(d6cnde)).
compute d7=((d7cnie)+(d7cnde)).
compute d8=((d8cnie)+(d8cnde)).
compute gamma13={d1,d2,d3,d4,d5,d6,d7,d8}.

!IFEND.


compute d1=((d1nie)+(d1nde)).
compute d2=((d2nie)+(d2nde)).
compute d3=((d3nie)+(d3nde)).
compute d4=((d4nie)+(d4nde)).
compute d5=((d5nie)+(d5nde)).
compute d6=((d6nie)+(d6nde)).
compute d7=((d7nie)+(d7nde)).
compute d8=((d8nie)+(d8nde)).
compute gamma11={d1,d2,d3,d4,d5,d6,d7,d8}.

!IFEND.




!If ((!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=FALSE & !NC~=0) | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=FALSE & !NC~=0) | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=FALSE & !NC~=0) 
 | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=FALSE & !NC~=0)) !Then.

Loop i=1 To !NC.
!If (!c~=NA) !Then.

*/CONDITIONAL CDE*/.
compute x1=exp((theta1)*(!a1-!a0)).
compute effect1=x1.
*/CONDITIONAL NDE*/.
compute effect2=exp(theta1*(!a1-!a0))*
(1+exp(theta2+beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))/
(1+exp(theta2+beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i))))).
*/CONDITIONAL NIE*/.
compute effect3=(
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))*
(1+exp(theta2+beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))
)/
(
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))*
(1+exp(theta2+beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))
).
*/CONDITIONAL TNDE*/.
compute effect4=exp(theta1*(!a1-!a0))*
(1+exp(theta2+beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))/
(1+exp(theta2+beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i))))).

compute effect5=(
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))*
(1+exp(theta2+beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))
)/
(
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*t(cval(1,i)))))*
(1+exp(theta2+beta0+beta1*!a0+csum(beta2(1,i)*t(cval(1,i)))))
).
!IFEND.

*/MARGINAL CDE*/.
compute x6=(theta1)*(!a1-!a0).
compute effect6=exp(x6).

*/MARGINAL NDE*/.
compute effect7=exp(theta1*(!a1-!a0))*
(1+exp(theta2+beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i)))))/
(1+exp(theta2+beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i))))).

*/MARGINAL NIE*/.
compute effect8=(
(1+exp(beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i)))))*
(1+exp(theta2+beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))
)/
(
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))*
(1+exp(theta2+beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i)))))
).

*/ MARGINAL TNDE*/.
compute effect9=exp(theta1*(!a1-!a0))*
(1+exp(theta2+beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))/
(1+exp(theta2+beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i))))).

*/ MARGINAL TNIE*/.
compute effect10=(
(1+exp(beta0+csum(beta2(1,i)*t(meanc(1,i)))))*
(1+exp(theta2+beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))
)/
(
(1+exp(beta0+beta1*!a1+csum(beta2(1,i)*t(meanc(1,i)))))*
(1+exp(theta2+beta0+beta1*!a0+csum(beta2(1,i)*t(meanc(1,i)))))
).

!If (!c~=NA) !Then.
compute logtecon=ln((effect2)*(effect5)).
compute effect13=(effect2)*(effect5).

!IFEND.

compute logtemar=ln((effect7)*(effect10)).
compute effect11=(effect7)*(effect10).

compute effect12=(effect7)*((effect10)-1)/((effect7)*(effect10)-1).

End Loop.



compute zero1=make((!NC+3),(!NC+2),0).
compute zero2=make((!NC+2),(!NC+3),0).
compute sigma1={sigbeta,zero2}.
compute sigma2={zero1,sigtheta}.
compute sigma={ sigma1;sigma2}.
compute zeros=make(1,!NC,0).
COMPUTE beta2=t(X2(3:(3+!NC-1),1)).
compute cmean=(csum(DATACOV)/N).

compute d1=0.
compute d2=0.
compute d3=zeros.
compute d4=0.
compute d5=1.
compute d6=0.
compute d8=zeros.
compute gamma1={d1,d2,d3,d4,d5,d6,d8}.
!If (!c~=NA) !Then.
compute A=exp(theta2+beta0+beta1*!a0+beta2*t(cval(1,:))).
compute B=(1+exp(theta2+beta0+beta1*!a0+beta2*t(cval(1,:)))).
compute D=exp(theta2+beta0+beta1*!a0+beta2*t(cval(1,:))).
compute E=(1+exp(theta2+beta0+beta1*!a0+beta2*t(cval(1,:)))).
compute d1cnde=A/B-D/E.
compute d2cnde=!a0*(d1cnde).
compute d3cnde=(d1cnde)*(cval(1,:)).
compute d4cnde=0.
compute d5cnde=(!a1-!a0).
compute d6cnde=d1cnde.
compute d8cnde=zeros.
compute gamma2={d1cnde,d2cnde,d3cnde,d4cnde,d5cnde,d6cnde,d8cnde}.
compute A=exp(theta2+beta0+beta1*!a1+beta2*t(cval(1,:))).
compute B=(1+exp(theta2+beta0+beta1*!a1+beta2*t(cval(1,:)))).
compute D=exp(theta2+beta0+beta1*!a1+beta2*t(cval(1,:))).
compute E=(1+exp(theta2+beta0+beta1*!a1+beta2*t(cval(1,:)))).
compute d1=A/B-D/E.
compute d2=!a1*(d1).
compute d3=(d1)*(cval(1,:)).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=d1.
compute d8=zeros.
compute gamma4={d1,d2,d3,d4,d5,d6,d8}.
compute A=exp(theta2+beta0+beta1*!a1+beta2*t(cval(1,:))).
compute B=(1+exp(theta2+beta0+beta1*!a1+beta2*t(cval(1,:)))).
compute D=exp(theta2+beta0+beta1*!a0+beta2*t(cval(1,:))).
compute E=(1+exp(theta2+beta0+beta1*!a0+beta2*t(cval(1,:)))).
compute F=exp(beta0+beta1*!a0+beta2*t(cval(1,:))).
compute G=(1+exp(beta0+beta1*!a0+beta2*t(cval(1,:)))).
compute H=exp(beta0+beta1*!a1+beta2*t(cval(1,:))).
compute I=(1+exp(beta0+beta1*!a1+beta2*t(cval(1,:)))).
compute d1cnie=F/G-H/I+A/B-D/E.
compute d2cnie=!a0*F/G-!a1*H/I+!a1*A/B-!a0*D/E.
compute d3cnie=cval(1,:)*(d1cnie).
compute d4cnie=0.
compute d5cnie=0.
compute d6cnie=A/B-D/E.
compute d8cnie=zeros.
compute gamma5={d1cnie,d2cnie,d3cnie,d4cnie,d5cnie,d6cnie,d8cnie}.
compute A=exp(theta2+beta0+beta1*!a1+beta2*t(cval(1,:))).
compute B=(1+exp(theta2+beta0+beta1*!a1+beta2*t(cval(1,:)))).
compute D=exp(theta2+beta0+beta1*!a0+beta2*t(cval(1,:))).
compute E=(1+exp(theta2+beta0+beta1*!a0+beta2*t(cval(1,:)))).
compute F=exp(beta0+beta1*!a0+beta2*t(cval(1,:))).
compute G=(1+exp(beta0+beta1*!a0+beta2*t(cval(1,:)))).
compute H=exp(beta0+beta1*!a1+beta2*t(cval(1,:))).
compute I=(1+exp(beta0+beta1*!a1+beta2*t(cval(1,:)))).
compute d1=F/G-H/I+A/B-D/E.
compute d2=!a1*F/G-!a0*H/I+!a0*A/B-!a1*D/E.
compute d3=cval(1,:)*(d1).
compute d4=0.
compute d5=0.
compute d6=A/B-D/E.
compute d8=zeros.
compute gamma3={d1,d2,d3,d4,d5,d6,d8}.

!IFEND.

compute gamma6=gamma1.
compute A=exp(theta2+beta0+beta1*!a0+beta2*t(cmean)).
compute B=(1+exp(theta2+beta0+beta1*!a0+beta2*t(cmean))).
compute D=exp(theta2+beta0+beta1*!a0+beta2*t(cmean)).
compute E=(1+exp(theta2+beta0+beta1*!a0+beta2*t(cmean))).
compute d1nde=A/B-D/E.
compute d2nde=!a0*(d1nde).
compute d3nde=(d1nde)*(Cmean).
compute d4nde=0.
compute d5nde=(!a1-!a0).
compute d6nde=d1nde.
compute d8nde=zeros.
compute gamma7={d1nde,d2nde,d3nde,d4nde,d5nde,d6nde,d8nde}.
compute A=exp(theta2+beta0+beta1*!a1+beta2*t(cmean)).
compute B=(1+exp(theta2+beta0+beta1*!a1+beta2*t(Cmean))).
compute D=exp(theta2+beta0+beta1*!a1+beta2*t(Cmean)).
compute E=(1+exp(theta2+beta0+beta1*!a1+beta2*t(Cmean))).
compute d1=A/B-D/E.
compute d2=!a1*(d1).
compute d3=(d1)*(Cmean).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=d1.
compute d8=zeros.
compute gamma9={d1,d2,d3,d4,d5,d6,d8}.
compute A=exp(theta2+beta0+beta1*!a1+beta2*t(Cmean)).
compute B=(1+exp(theta2+beta0+beta1*!a1+beta2*t(Cmean))).
compute D=exp(theta2+beta0+beta1*!a0+beta2*t(Cmean)).
compute E=(1+exp(theta2+beta0+beta1*!a0+beta2*t(Cmean))).
compute F=exp(beta0+beta1*!a0+beta2*t(Cmean)).
compute G=(1+exp(beta0+beta1*!a0+beta2*t(Cmean))).
compute H=exp(beta0+beta1*!a1+beta2*t(Cmean)).
compute I=(1+exp(beta0+beta1*!a1+beta2*t(Cmean))).
compute d1nie=F/G-H/I+A/B-D/E.
compute d2nie=!a0*F/G-!a1*H/I+!a1*A/B-!a0*D/E.
compute d3nie=Cmean*(d1nie).
compute d4nie=0.
compute d5nie=0.
compute d6nie=A/B-D/E.
compute d8nie=zeros.
compute gamma10={d1nie,d2nie,d3nie,d4nie,d5nie,d6nie,d8nie}.
compute A=exp(theta2+beta0+beta1*!a1+beta2*t(Cmean)).
compute B=(1+exp(theta2+beta0+beta1*!a1+beta2*t(Cmean))).
compute D=exp(theta2+beta0+beta1*!a0+beta2*t(Cmean)).
compute E=(1+exp(theta2+beta0+beta1*!a0+beta2*t(Cmean))).
compute F=exp(beta0+beta1*!a0+beta2*t(Cmean)).
compute G=(1+exp(beta0+beta1*!a0+beta2*t(Cmean))).
compute H=exp(beta0+beta1*!a1+beta2*t(Cmean)).
compute I=(1+exp(beta0+beta1*!a1+beta2*t(Cmean))).
compute d1=F/G-H/I+A/B-D/E.
compute d2=!a1*F/G-!a0*H/I+!a0*A/B-!a1*D/E.
compute d3=Cmean*(d1).
compute d4=0.
compute d5=0.
compute d6=A/B-D/E.
compute d8=zeros.
compute gamma8={d1,d2,d3,d4,d5,d6,d8}.

!If (!c~=NA) !Then.
compute d1=((d1cnie)+(d1cnde)).
compute d2=((d2cnie)+(d2cnde)).
compute d3=((d3cnie)+(d3cnde)).
compute d4=((d4cnie)+(d4cnde)).
compute d5=((d5nie)+(d5nde)).
compute d6=((d6nie)+(d6nde)).
compute d8=((d8cnie)+(d8cnde)).
compute gamma13={d1,d2,d3,d4,d5,d6,d8}.

!IFEND.

compute d1=((d1nie)+(d1nde)).
compute d2=((d2nie)+(d2nde)).
compute d3=((d3nie)+(d3nde)).
compute d4=0.
compute d5=((d5nie)+(d5nde)).
compute d6=((d6nie)+(d6nde)).
compute d8=((d8nie)+(d8nde)).
compute gamma11={d1,d2,d3,d4,d5,d6,d8}.

!IFEND.




!If ((!yreg=LOGISTIC & !mreg=LOGISTIC & !interaction=FALSE & !NC=0)  | (!yreg=LOGLINEAR & !mreg=LOGISTIC & !interaction=FALSE & !NC=0) | (!yreg=POISSON & !mreg=LOGISTIC & !interaction=FALSE & !NC=0) 
 | (!yreg=NEGBIN & !mreg=LOGISTIC & !interaction=FALSE & !NC=0)) !Then.


*/ CDE*/.
compute x1=exp((theta1)*(!a1-!a0)).
compute effect1=x1.
*/ NDE*/.
compute effect2=exp(theta1*(!a1-!a0))*
(1+exp(theta2+beta0+beta1*!a0))/
(1+exp(theta2+beta0+beta1*!a0)).
*/ NIE*/.
compute effect3=(
(1+exp(beta0+beta1*!a0))*
(1+exp(theta2+beta0+beta1*!a1))
)/
(
(1+exp(beta0+beta1*!a1))*
(1+exp(theta2+beta0+beta1*!a0))
).
*/ TNDE*/.
compute effect4=exp(theta1*(!a1-!a0))*
(1+exp(theta2+beta0+beta1*!a1))/
(1+exp(theta2+beta0+beta1*!a1)).

compute effect5=(
(1+exp(beta0+beta1*!a0))*
(1+exp(theta2+beta0+beta1*!a1))
)/
(
(1+exp(beta0+beta1*!a1))*
(1+exp(theta2+beta0+beta1*!a0))
).

compute logtemar=ln((effect2)*(effect5)).
compute effect11=(effect2)*(effect5).


compute effect12=(effect2)*((effect5)-1)/((effect2)*(effect5)-1).

compute zero1=make((3),(2),0).
compute zero2=make((2),(3),0).
compute sigma1={sigbeta,zero2}.
compute sigma2={zero1,sigtheta}.
compute sigma={ sigma1;sigma2}.
compute d1=0.
compute d2=0.
compute d4=0.
compute d5=1.
compute d6=0.
compute gamma1={d1,d2,d4,d5,d6}.
compute A=exp(theta2+beta0+beta1*!a0).
compute B=(1+exp(theta2+beta0+beta1*!a0)).
compute D=exp(theta2+beta0+beta1*!a0).
compute E=(1+exp(theta2+beta0+beta1*!a0)).
compute d1nde=A/B-D/E.
compute d2nde=!a0*(d1nde).
compute d4nde=0.
compute d5nde=(!a1-!a0).
compute d6nde=d1nde.

compute gamma2={d1nde,d2nde,d4nde,d5nde,d6nde}.
compute A=exp(theta2+beta0+beta1*!a1).
compute B=(1+exp(theta2+beta0+beta1*!a1)).
compute D=exp(theta2+beta0+beta1*!a1).
compute E=(1+exp(theta2+beta0+beta1*!a1)).
compute d1=A/B-D/E.
compute d2=!a1*(d1).
compute d4=0.
compute d5=(!a1-!a0).
compute d6=d1.
compute gamma4={d1,d2,d4,d5,d6}.
compute A=exp(theta2+beta0+beta1*!a1).
compute B=(1+exp(theta2+beta0+beta1*!a1)).
compute D=exp(theta2+beta0+beta1*!a0).
compute E=(1+exp(theta2+beta0+beta1*!a0)).
compute F=exp(beta0+beta1*!a0).
compute G=(1+exp(beta0+beta1*!a0)).
compute H=exp(beta0+beta1*!a1).
compute I=(1+exp(beta0+beta1*!a1)).
compute d1nie=F/G-H/I+A/B-D/E.
compute d2nie=!a0*F/G-!a1*H/I+!a1*A/B-!a0*D/E.
compute d4nie=0.
compute d5nie=0.
compute d6nie=A/B-D/E.
compute gamma5={d1nie,d2nie,d4nie,d5nie,d6nie}.
compute A=exp(theta2+beta0+beta1*!a1).
compute B=(1+exp(theta2+beta0+beta1*!a1)).
compute D=exp(theta2+beta0+beta1*!a0).
compute E=(1+exp(theta2+beta0+beta1*!a0)).
compute F=exp(beta0+beta1*!a0).
compute G=(1+exp(beta0+beta1*!a0)).
compute H=exp(beta0+beta1*!a1).
compute I=(1+exp(beta0+beta1*!a1)).
compute d1=F/G-H/I+A/B-D/E.
compute d2=!a1*F/G-!a0*H/I+!a0*A/B-!a1*D/E.
compute d4=0.
compute d5=0.
compute d6=A/B-D/E.
compute gamma3={d1,d2,d4,d5,d6}.
compute d1=((d1nie)+(d1nde)).
compute d2=((d2nie)+(d2nde)).
compute d4=0.
compute d5=((d5nie)+(d5nde)).
compute d6=((d6nie)+(d6nde)).
compute gamma11={d1,d2,d4,d5,d6}.
!IFEND.



!If ((!interaction=TRUE & !yreg=LINEAR  & !NC~=0 ) | (!interaction=FALSE & !yreg=LINEAR & !mreg=LOGISTIC  & !NC~=0 ) ) !Then.
!If ( !Output=FULL  & !c~=NA) !Then.

compute se1=SQRT(gamma1*sigma*t(gamma1)*ABS(!a1-!a0)).
compute se2=SQRT(gamma2*sigma*t(gamma2)*ABS(!a1-!a0)).
compute se4=SQRT(gamma4*sigma*t(gamma4)*ABS(!a1-!a0)).
compute se6=SQRT(gamma6*sigma*t(gamma6)*ABS(!a1-!a0)).
compute se7=SQRT(gamma7*sigma*t(gamma7)*ABS(!a1-!a0)).
compute se9=SQRT(gamma9*sigma*t(gamma9)*ABS(!a1-!a0)).



!If (!mreg=LINEAR) !Then.
compute se3=SQRT(gamma3*sigma*t(gamma3)*ABS(!a1-!a0)).
compute se5=SQRT(gamma5*sigma*t(gamma5)*ABS(!a1-!a0)).
compute se8=SQRT(gamma8*sigma*t(gamma8)*ABS(!a1-!a0)).
compute se10=SQRT(gamma10*sigma*t(gamma10)*ABS(!a1-!a0)).
compute se11=SQRT(gamma11*sigma*t(gamma11)*ABS(!a1-!a0)).
compute se13=SQRT(gamma13*sigma*t(gamma13)*ABS(!a1-!a0)).
!IFEND.

!If (!mreg=LOGISTIC) !Then.
compute se3=SQRT(gamma3*sigma*t(gamma3)).
compute se5=SQRT(gamma5*sigma*t(gamma5)).
compute se8=SQRT(gamma8*sigma*t(gamma8)).
compute se10=SQRT(gamma10*sigma*t(gamma10)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
compute se13=SQRT(gamma13*sigma*t(gamma13)).
!IFEND.

compute tsd={se1;se2;se3;se4;se5;se6;se7;se8;se9;se10;se11;se13}.
compute teffects={effect1;effect2;effect3;effect4;effect5;effect6;effect7;effect8;effect9;effect10;effect11;effect13}.
compute tcu=make(12,1,0).
compute tcl=make(12,1,0).
compute tpval=make(12,1,0).
Loop j=1 To 12.
compute tcu(j,1)=teffects(j,1)+((1.96)*(tsd(j,1))).
compute tcl(j,1)=teffects(j,1)-((1.96)*(tsd(j,1))).
compute tpval(j,1)=2*(1-cdfnorm(ABS(teffects(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tsd,tpval,tcl,tcu}.
Print table
 /title = "Direct and Indirect Effects" 
  /clabels="estimates","se","p-value"," 95CIlow","95CIup"
  /rnames={"ccde";"cpnde";"cpnie";"ctnde";"ctnie";"mcde";"mpnde";"mpnie";"mtnde";"mtnie";"mte";"cte"}.
!IFEND.

!If (  !Output~=FULL) !Then.
compute se6=SQRT(gamma6*sigma*t(gamma6)*ABS(!a1-!a0)).
compute se7=SQRT(gamma7*sigma*t(gamma7)*ABS(!a1-!a0)).



!If (!mreg=LINEAR) !Then.
compute se10=SQRT(gamma10*sigma*t(gamma10)*ABS(!a1-!a0)).
compute se11=SQRT(gamma11*sigma*t(gamma11)*ABS(!a1-!a0)).
!IFEND.

!If (!mreg=LOGISTIC) !Then.
compute se10=SQRT(gamma10*sigma*t(gamma10)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
!IFEND.


compute tsd={se6;se7;se10;se11}.
compute teffects={effect6;effect7;effect10;effect11}.
compute tcu=make(4,1,0).
compute tcl=make(4,1,0).
compute tpval=make(4,1,0).
Loop j=1 To 4.
compute tcu(j,1)=teffects(j,1)+((1.96)*(tsd(j,1))).
compute tcl(j,1)=teffects(j,1)-((1.96)*(tsd(j,1))).
compute tpval(j,1)=2*(1-cdfnorm(ABS(teffects(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tsd,tpval,tcl,tcu}.
Print table
 /title = "Direct and Indirect Effects" 
  /clabels="estimates","se","p-value"," 95CIlow","95CIup"
  /rnames={"cde";"nde";"nie";"mte"}.
!IFEND.
!IFEND.


!If (!interaction=TRUE & !yreg=LINEAR & !NC=0 ) !Then.
!If (!Output=FULL) !Then.
compute se1=SQRT(gamma1*sigma*t(gamma1)*ABS(!a1-!a0)).
compute se2=SQRT(gamma2*sigma*t(gamma2)*ABS(!a1-!a0)).
compute se4=SQRT(gamma4*sigma*t(gamma4)*ABS(!a1-!a0)).



!If (!mreg=LINEAR) !Then.
compute se3=SQRT(gamma3*sigma*t(gamma3)*ABS(!a1-!a0)).
compute se5=SQRT(gamma5*sigma*t(gamma5)*ABS(!a1-!a0)).
compute se11=SQRT(gamma11*sigma*t(gamma11)*ABS(!a1-!a0)).
!IFEND.

!If (!mreg=LOGISTIC) !Then.
compute se3=SQRT(gamma3*sigma*t(gamma3)).
compute se5=SQRT(gamma5*sigma*t(gamma5)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
!IFEND.



compute tsd={se1;se2;se3;se4;se5;se11}.
compute teffects={effect1;effect2;effect3;effect4;effect5;effect11}.
compute tcu=make(6,1,0).
compute tcl=make(6,1,0).
compute tpval=make(6,1,0).
Loop j=1 To 6.
compute tcu(j,1)=teffects(j,1)+((1.96)*(tsd(j,1))).
compute tcl(j,1)=teffects(j,1)-((1.96)*(tsd(j,1))).
compute tpval(j,1)=2*(1-cdfnorm(ABS(teffects(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tsd,tpval,tcl,tcu}.
Print table
 /title = "Direct and Indirect Effects" 
  /clabels="estimates","se","p-value"," 95CIlow","95CIup"
  /rnames={"cde";"pnde";"pnie";"tnde";"tnie";"te"}.
!IFEND.
!If (!Output~=FULL) !Then.
compute se1=SQRT(gamma1*sigma*t(gamma1)*ABS(!a1-!a0)).
compute se2=SQRT(gamma2*sigma*t(gamma2)*ABS(!a1-!a0)).

!If (!mreg=LINEAR) !Then.
compute se5=SQRT(gamma5*sigma*t(gamma5)*ABS(!a1-!a0)).
compute se11=SQRT(gamma11*sigma*t(gamma11)*ABS(!a1-!a0)).
!IFEND.

!If (!mreg=LOGISTIC) !Then.
compute se5=SQRT(gamma5*sigma*t(gamma5)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
!IFEND.


compute teffects={effect1;effect2;effect5;effect11}.
compute tsd={se1;se2;se5;se11}.
compute tcu=make(4,1,0).
compute tcl=make(4,1,0).
compute tpval=make(4,1,0).
Loop j=1 To 4.
compute tcu(j,1)=teffects(j,1)+((1.96)*(tsd(j,1))).
compute tcl(j,1)=teffects(j,1)-((1.96)*(tsd(j,1))).
compute tpval(j,1)=2*(1-cdfnorm(ABS(teffects(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tsd,tpval,tcl,tcu}.
Print table
 /title = "Direct and Indirect Effects" 
  /clabels="estimates","se","p-value"," 95CIlow","95CIup"
  /rnames={"cde";"nde";"nie";"te"}.
!IFEND.
!IFEND.



!If ((!interaction=FALSE & !yreg=LINEAR & !mreg=LINEAR) | (!interaction=FALSE & !yreg=LINEAR & !mreg=LOGISTIC & !NC=0) |(!interaction=FALSE & !yreg=LOGISTIC & !mreg=LINEAR)  |(!interaction=FALSE & !yreg=LOGLINEAR & !mreg=LINEAR) 
 |(!interaction=FALSE & !yreg=POISSON & !mreg=LINEAR)  |(!interaction=FALSE & !yreg=NEGBIN & !mreg=LINEAR)) !Then.

compute se1=SQRT(gamma1*sigma*t(gamma1)*ABS(!a1-!a0)).

!If (!mreg=LOGISTIC) !Then.
compute se3=SQRT(gamma3*sigma*t(gamma3)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
!IFEND.


!If (!mreg=LINEAR) !Then.
compute se3=SQRT(gamma3*sigma*t(gamma3)*ABS(!a1-!a0)).
compute se11=SQRT(gamma11*sigma*t(gamma11)*ABS(!a1-!a0)).
!IFEND.


compute tsd={se1;se3;se11}.

compute teffects={effect1;effect3;effect11}.
!if (!yreg=LINEAR) !then.
compute tcu=make(3,1,0).
compute tcl=make(3,1,0).
compute tpval=make(3,1,0).
Loop j=1 To 3.
compute tcu(j,1)=teffects(j,1)+((1.96)*(tsd(j,1))).
compute tcl(j,1)=teffects(j,1)-((1.96)*(tsd(j,1))).
compute tpval(j,1)=2*(1-cdfnorm(ABS(teffects(j,1)/tsd(j,1)))).
End Loop.
!ifend.

!if (!yreg~=LINEAR) !then.
compute tcu=make(3,1,0).
compute tcl=make(3,1,0).
compute tpval=make(3,1,0).
compute log=make(3,1,0).
compute logcu=make(3,1,0).
compute logcl=make(3,1,0).
Loop j=1 To 3.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
!ifend.

!if (!yreg=LINEAR) !then.
compute table={teffects,tsd,tpval,tcl,tcu}.
Print table
 /title = "Direct and Indirect Effects" 
  /clabels="estimates","se","p-value"," 95CIlow","95CIup"
  /rnames={"cde-nde";"nie";"te"}.
!ifend.
!if (!yreg~=LINEAR) !then.
compute table={teffects,tpval,tcl,tcu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde-nde";"nie";"te"}.
!ifend.

!IFEND.





!if (!yreg~=LINEAR & !interaction=TRUE & !NC~=0) !THEN.
!If ( !mreg=LOGISTIC & !c~=NA) !Then.
!If ( !Output=FULL & !c~=NA) !Then.
compute se1=SQRT(gamma1*sigma*t(gamma1)*ABS(!a1-!a0)).
compute se2=SQRT(gamma2*sigma*t(gamma2)).
compute se3=SQRT(gamma3*sigma*t(gamma3)).
compute se4=SQRT(gamma4*sigma*t(gamma4)).
compute se5=SQRT(gamma5*sigma*t(gamma5)).
compute se6=SQRT(gamma6*sigma*t(gamma6)*ABS(!a1-!a0)).
compute se7=SQRT(gamma7*sigma*t(gamma7)).
compute se8=SQRT(gamma8*sigma*t(gamma8)).
compute se9=SQRT(gamma9*sigma*t(gamma9)).
compute se10=SQRT(gamma10*sigma*t(gamma10)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
compute se13=SQRT(gamma13*sigma*t(gamma13)).
compute tsd={se1;se2;se3;se4;se5;se6;se7;se8;se9;se10;se11;se13}.
compute teffects={effect1;effect2;effect3;effect4;effect5;effect6;effect7;effect8;effect9;effect10;effect11;effect13}.
compute tcu=make(12,1,0).
compute tcl=make(12,1,0).
compute tpval=make(12,1,0).
compute logcu=make(12,1,0).
compute logcl=make(12,1,0).
Loop j=1 To 12.
compute log=ln(teffects).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"cpnde";"cpnie";"ctnde";"ctnie";"cde";"mpnde";"mpnie";"mtnde";"mtnie";"mte";"cte"}.
!IFEND.
!If ( !Output~=FULL ) !Then.
compute se6=SQRT(gamma6*sigma*t(gamma6) *ABS(!a1-!a0)).
compute se7=SQRT(gamma7*sigma*t(gamma7)).
compute se10=SQRT(gamma10*sigma*t(gamma10)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
compute tsd={se6;se7;se10;se11}.
compute teffects={effect6;effect7;effect10;effect11}.
compute tcu=make(4,1,0).
compute tcl=make(4,1,0).
compute tpval=make(4,1,0).
compute log=make(4,1,0).
compute logcu=make(4,1,0).
compute logcl=make(4,1,0).
Loop j=1 To 4.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"nde";"tnie";"mte"}.
!IFEND.
!IFEND.

!If ( !mreg=LINEAR & !c~=NA) !Then.
!If ( !Output=FULL & !c~=NA) !Then.
compute se1=SQRT(gamma1*sigma*t(gamma1)*ABS(!a1-!a0)).
compute se2=SQRT(gamma2*sigma*t(gamma2)*ABS(!a1-!a0)).
compute se3=SQRT(gamma3*sigma*t(gamma3)*ABS(!a1-!a0)).
compute se4=SQRT(gamma4*sigma*t(gamma4)*ABS(!a1-!a0)).
compute se5=SQRT(gamma5*sigma*t(gamma5)*ABS(!a1-!a0)).
compute se6=SQRT(gamma6*sigma*t(gamma6)*ABS(!a1-!a0)).
compute se7=SQRT(gamma7*sigma*t(gamma7)*ABS(!a1-!a0)).
compute se8=SQRT(gamma8*sigma*t(gamma8)*ABS(!a1-!a0)).
compute se9=SQRT(gamma9*sigma*t(gamma9)*ABS(!a1-!a0)).
compute se10=SQRT(gamma10*sigma*t(gamma10)*ABS(!a1-!a0)).
compute se11=SQRT(gamma11*sigma*t(gamma11)*ABS(!a1-!a0)).
compute se13=SQRT(gamma13*sigma*t(gamma13)*ABS(!a1-!a0)).
compute tsd={se1;se2;se3;se4;se5;se6;se7;se8;se9;se10;se11;se13}.
compute teffects={effect1;effect2;effect3;effect4;effect5;effect6;effect7;effect8;effect9;effect10;effect11;effect13}.
compute tcu=make(12,1,0).
compute tcl=make(12,1,0).
compute tpval=make(12,1,0).
compute logcu=make(12,1,0).
compute logcl=make(12,1,0).
Loop j=1 To 12.
compute log=ln(teffects).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"cpnde";"cpnie";"ctnde";"ctnie";"cde";"mpnde";"mpnie";"mtnde";"mtnie";"mte";"cte"}.
!IFEND.
!If ( !Output~=FULL ) !Then.
compute se6=SQRT(gamma6*sigma*t(gamma6) *ABS(!a1-!a0)).
compute se7=SQRT(gamma7*sigma*t(gamma7) *ABS(!a1-!a0)).
compute se10=SQRT(gamma10*sigma*t(gamma10) *ABS(!a1-!a0)).
compute se11=SQRT(gamma11*sigma*t(gamma11) *ABS(!a1-!a0)).
compute tsd={se6;se7;se10;se11}.
compute teffects={effect6;effect7;effect10;effect11}.
compute tcu=make(4,1,0).
compute tcl=make(4,1,0).
compute tpval=make(4,1,0).
compute log=make(4,1,0).
compute logcu=make(4,1,0).
compute logcl=make(4,1,0).
Loop j=1 To 4.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"nde";"tnie";"mte"}.
!IFEND.
!IFEND.
!IFEND.

!if (!yreg~=LINEAR & !mreg=LOGISTIC & !NC~=0 ) !THEN.
!If ( !Output=FULL & !c~=NA) !Then.
compute se1=SQRT(gamma1*sigma*t(gamma1) *ABS(!a1-!a0)).
compute se2=SQRT(gamma2*sigma*t(gamma2)).
compute se3=SQRT(gamma3*sigma*t(gamma3)).
compute se4=SQRT(gamma4*sigma*t(gamma4)).
compute se5=SQRT(gamma5*sigma*t(gamma5)).
compute se6=SQRT(gamma6*sigma*t(gamma6) *ABS(!a1-!a0)).
compute se7=SQRT(gamma7*sigma*t(gamma7)).
compute se8=SQRT(gamma8*sigma*t(gamma8)).
compute se9=SQRT(gamma9*sigma*t(gamma9)).
compute se10=SQRT(gamma10*sigma*t(gamma10)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
compute se13=SQRT(gamma13*sigma*t(gamma13)).
compute tsd={se1;se2;se3;se4;se5;se6;se7;se8;se9;se10;se11;se13}.
compute teffects={effect1;effect2;effect3;effect4;effect5;effect6;effect7;effect8;effect9;effect10;effect11;effect13}.
compute tcu=make(12,1,0).
compute tcl=make(12,1,0).
compute tpval=make(12,1,0).
compute logcu=make(12,1,0).
compute logcl=make(12,1,0).
Loop j=1 To 12.
compute log=ln(teffects).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"cpnde";"cpnie";"ctnde";"ctnie";"cde";"mpnde";"mpnie";"mtnde";"mtnie";"mte";"cte"}.
!IFEND.
!If ( !Output~=FULL ) !Then.
compute se6=SQRT(gamma6*sigma*t(gamma6)*ABS(!a1-!a0)).
compute se7=SQRT(gamma7*sigma*t(gamma7)).
compute se10=SQRT(gamma10*sigma*t(gamma10)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
compute tsd={se6;se7;se10;se11}.
compute teffects={effect6;effect7;effect10;effect11}.
compute tcu=make(4,1,0).
compute tcl=make(4,1,0).
compute tpval=make(4,1,0).
compute log=make(4,1,0).
compute logcu=make(4,1,0).
compute logcl=make(4,1,0).
Loop j=1 To 4.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"nde";"tnie";"mte"}.
!IFEND.
!IFEND.


!if (!yreg~=LINEAR & !interaction=TRUE & !NC=0) !THEN.
!If ( !mreg=LOGISTIC & !c~=NA) !Then.
!IF (!Output=FULL) !then.
compute se1=SQRT(gamma1*sigma*t(gamma1) *ABS(!a1-!a0)).
compute se2=SQRT(gamma2*sigma*t(gamma2)).
compute se3=SQRT(gamma3*sigma*t(gamma3)).
compute se4=SQRT(gamma4*sigma*t(gamma4)).
compute se5=SQRT(gamma5*sigma*t(gamma5)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
compute tsd={se1;se2;se3;se4;se5;se11}.
compute teffects={effect1;effect2;effect3;effect4;effect5;effect11}.
compute tcu=make(6,1,0).
compute tcl=make(6,1,0).
compute tpval=make(6,1,0).
compute log=make(6,1,0).
compute logcu=make(6,1,0).
compute logcl=make(6,1,0).
Loop j=1 To 6.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"pnde";"pnie";"tnde";"tnie";"te"}.
!IFEND.
!IF (!Output~=FULL) !then.
compute tsd={se1;se2;se5;se11}.
compute teffects={effect1;effect2;effect5;effect11}.
compute tcu=make(4,1,0).
compute tcl=make(4,1,0).
compute tpval=make(4,1,0).
compute log=make(4,1,0).
compute logcu=make(4,1,0).
compute logcl=make(4,1,0).
Loop j=1 To 4.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"nde";"nie";"te"}.
!IFEND.
!IFEND.
!If (!mreg=LINEAR) !Then.
!If (!Output=FULL) !Then.
compute se1=SQRT(gamma1*sigma*t(gamma1)*ABS(!a1-!a0)).
compute se2=SQRT(gamma2*sigma*t(gamma2)*ABS(!a1-!a0)).
compute se3=SQRT(gamma3*sigma*t(gamma3)*ABS(!a1-!a0)).
compute se4=SQRT(gamma4*sigma*t(gamma4)*ABS(!a1-!a0)).
compute se5=SQRT(gamma5*sigma*t(gamma5)*ABS(!a1-!a0)).
compute se11=SQRT(gamma11*sigma*t(gamma11) *ABS(!a1-!a0)).
compute tsd={se1;se2;se3;se4;se5;se11}.
compute teffects={effect1;effect2;effect3;effect4;effect5;effect11}.
compute tcu=make(6,1,0).
compute tcl=make(6,1,0).
compute tpval=make(6,1,0).
compute log=make(6,1,0).
compute logcu=make(6,1,0).
compute logcl=make(6,1,0).
Loop j=1 To 6.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"pnde";"pnie";"tnde";"tnie";"te"}.
!IFEND.
!IF (!Output~=FULL) !then.
compute tsd={se1;se2;se5;se11}.
compute teffects={effect1;effect2;effect5;effect11}.
compute tcu=make(4,1,0).
compute tcl=make(4,1,0).
compute tpval=make(4,1,0).
compute log=make(4,1,0).
compute logcu=make(4,1,0).
compute logcl=make(4,1,0).
Loop j=1 To 4.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
/title = "Direct and Indirect Effects"
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"nde";"nie";"te"}.
!IFEND.
!IFEND.
!IFEND.


!if (!yreg~=LINEAR & !mreg=LOGISTIC & !interaction=FALSE & !NC=0 ) !THEN. 
compute se1=SQRT(gamma1*sigma*t(gamma1))*ABS(!a1-!a0)).
compute se2=SQRT(gamma2*sigma*t(gamma2)).
compute se3=SQRT(gamma3*sigma*t(gamma3)).
compute se4=SQRT(gamma4*sigma*t(gamma4)).
compute se5=SQRT(gamma5*sigma*t(gamma5)).
compute se11=SQRT(gamma11*sigma*t(gamma11)).
!IF (!Output=FULL) !then.
compute tsd={se1;se2;se3;se4;se5;se11}.
compute teffects={effect1;effect2;effect3;effect4;effect5;effect11}.
compute tcu=make(6,1,0).
compute tcl=make(6,1,0).
compute tpval=make(6,1,0).
compute log=make(6,1,0).
compute logcu=make(6,1,0).
compute logcl=make(6,1,0).
Loop j=1 To 6.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"pnde";"pnie";"tnde";"tnie";"te"}.
!IFEND.
!IF (!Output~=FULL) !then.
compute tsd={se1;se2;se5;se11}.
compute teffects={effect1;effect2;effect5;effect11}.
compute tcu=make(4,1,0).
compute tcl=make(4,1,0).
compute tpval=make(4,1,0).
compute log=make(4,1,0).
compute logcu=make(4,1,0).
compute logcl=make(4,1,0).
Loop j=1 To 4.
compute log(j,1)=ln(teffects(j,1)).
compute logcu(j,1)=log(j,1)+((1.96)*(tsd(j,1))).
compute logcl(j,1)=log(j,1)-((1.96)*(tsd(j,1))).
compute tcu(j,1)=exp(logcu(j,1)).
compute tcl(j,1)=exp(logcl(j,1)).
compute tpval(j,1)=2*(1-cdfnorm(ABS(log(j,1)/tsd(j,1)))).
End Loop.
compute table={teffects,tpval,tcl,tcu}.
Print table
 /title = "Direct and Indirect Effects" 
 /clabels="estimates","p-value"," 95CIlow","95CIup"
 /rnames={"cde";"nde";"nie";"te"}.
!IFEND.
!IFEND.
 
End matrix.

!ifend.

!If (!interaction=TRUE) !Then.
save outfile !data
  /DROp =Int
!IFEND.

*dataset close *.


!ENDDEFINE.




$TITLE MODEL TNMI: A MODEL TO ANALYZE VULNERABILITIES IN A TRANSSHIPMENT NETWORK
************* AUTHOR: FATIH KASIMOGLU*******************************************
sets
*indices and sets
* Manual Entry: Define supply, stage1, stage2 and demand nodes below
        i  supply points  /i1*i25/
        j  stage1 stations /j1*j25/
        k  Stage2 stations /k1*k25/
        l  demand points  /l1*l25/
        SA1(i,j) set of existing arcs between i and j
        SA2(j,k) set of existing arcs between j and k
        SA3(k,l) set of existing arcs between k and l
;
***************************************************************
parameters
        S(i) capacity of supply station i
        D(l) demand at arrival station l
        C1(i,j) Cost coefficient between nodes i and j
        C2(j,k) Cost coefficient between nodes j and k
        C3(k,l) Cost coefficient between nodes k and l
        r1(i,j) resource to interdict arc between i and j
        r2(j,k) resource to interdict arc  between j and k
        r3(k,l) resource to interdict arc  between k and l
        TIR     total interdiction resources available

;

****** READ THE INDICES AND SETS FROM EXCEL FILE *******************
*Below part between $ontext and $offtext can be put into $onecho-$offecho section to specify associated domains for indices
$ontext
  dset=i   rng=indiceSheet!B2          cdim=1
  dset=j   rng=indiceSheet!B3          cdim=1
  dset=k   rng=indiceSheet!B4          cdim=1
  dset=l   rng=indiceSheet!B5          cdim=1
$offtext

$Onecho > tasksTNMI.txt
  Set=SA1  rng=setOfArcsSheetSA1!A1    rdim=1 cdim=1
  Set=SA2  rng=setOfArcsSheetSA2!A1    rdim=1 cdim=1
  Set=SA3  rng=setOfArcsSheetSA3!A1    rdim=1 cdim=1
$Offecho

$call GDXXRW ModelTNMI_Data_Experiment.xls trace=3 @tasksTNMI.txt
$GDXIN ModelTNMI_Data_Experiment.gdx
$Load SA1,SA2,SA3
$GDXIN

*** Values assigned to the parameters as follows ***
S(i)= round(uniform(10,15));
D(l)= round(uniform(5,10));
C1(i,j)= round (uniform(50,400));
C2(j,k)= round (uniform(20,80));
C3(k,l)= round (uniform(150,700));
r1(i,j)= round (uniform(1,2));
r2(j,k)= round (uniform(2,4));
r3(k,l)= round (uniform(1,2));
*** Manual Entry: Define total interdiction resources available below
TIR=5;
scalar
      M an arbitrarily large positive number
         /10000/
;
variables
     Z      Objective function value
     w2(j)   Dual variable of the original model
     w3(k)   Dual variable of the original model
     w4(l)   Dual variable of the original model
;
negative variables
     w1(i)   Dual variable of the original model
     v1(i,j) Dual variable associated with attack constraints
     v2(j,k) Dual variable associated with attack constraints
     v3(k,l) Dual variable associated with attack constraints
     q1(i,j) Variable used for linearization
     q2(j,k) Variable used for linearization
     q3(k,l) Variable used for linearization
;
binary variables
     y1(i,j) variable indicating whether road (i-j) is attacked
     y2(j,k) variable indicating whether railway (j-k) is attacked
     y3(k,l) variable indicating whether road (k-l) is attacked
;
equations
   ObjF             obj. func. value (total risk)

   ConstraintD1     constraint 1 after duality
   ConstraintD2     constraint 2 after duality
   ConstraintD3     constraint 3 after duality
   ConstraintTIR    constraint for total interdiction resource
   ConstraintTRSF1  constraint 1 for transformation
   ConstraintTRSF2  constraint 2 for transformation
   ConstraintTRSF3  constraint 3 for transformation
   ConstraintTRSF4  constraint 4 for transformation
   ConstraintTRSF5  constraint 5 for transformation
   ConstraintTRSF6  constraint 6 for transformation
;

ObjF..
      Z=E=sum((i),S(i)*w1(i))-sum((l),D(l)*w4(l))+sum((i,j)$SA1(i,j),M*q1(i,j))+
          sum((j,k)$SA2(j,k),M*q2(j,k))+ sum((k,l)$SA3(k,l),M*q3(k,l));
;

ConstraintD1(i,j) $ (SA1(i,j))..
                  w1(i)-w2(j)+v1(i,j)=L= C1(i,j);
ConstraintD2(j,k) $ (SA2(j,k))..
                  w2(j)-w3(k)+v2(j,k)=L= C2(j,k);
ConstraintD3(k,l) $ (SA3(k,l))..
                  w3(k)-w4(l)+v3(k,l)=L= C3(k,l);
ConstraintTIR..
                  sum((i,j)$SA1(i,j), r1(i,j)*y1(i,j))+
                  sum((j,k)$SA2(j,k), r2(j,k)*y2(j,k))+
                  sum((k,l)$SA3(k,l), r3(k,l)*y3(k,l))=L= TIR;
ConstraintTRSF1(i,j)$SA1(i,j)..
                  q1(i,j) =G= -M*(1-y1(i,j));
ConstraintTRSF2(i,j)$SA1(i,j)..
                  q1(i,j) =L= v1(i,j)+ M*y1(i,j);
ConstraintTRSF3(j,k)$SA2(j,k)..
                  q2(j,k) =G= -M*(1-y2(j,k));
ConstraintTRSF4(j,k)$SA2(j,k)..
                  q2(j,k) =L= v2(j,k)+ M*y2(j,k);
ConstraintTRSF5(k,l)$SA3(k,l)..
                  q3(k,l) =G= -M*(1-y3(k,l));
ConstraintTRSF6(k,l)$SA3(k,l)..
                  q3(k,l) =L= v3(k,l)+ M*y3(k,l);

model Model_TNMI /all/;
Model_TNMI.optcr=0.00;
Model_TNMI.iterlim=1000000;
Model_TNMI.reslim=1000;
solve Model_TNMI using MIP Maximizing Z ;
*Display i,j,k,l,SA1,SA2,SA3;
Display y1.l, y2.l, y3.l, SA1,SA2,SA3,S;

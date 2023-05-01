
$TITLE A MODEL TO ANALYZE VULNERABILITIES IN A TRANSSHIPMENT NETWORK
************* AUTHOR: FATIH KASIMOGLU********************************************
***    Dualization and Linearization Techniques Used  *********************
sets
*indices and sets
        i  supply points  /1*3/
        j  stage1 stations /1*3/
        k  Stage2 stations /1*3/
        l  demand points  /1*3/
        SA1(i,j) set of existing arcs between i and j
            /1.1,1.2,1.3,2.1,2.2,2.3,3.1,3.2,3.3/
        SA2(j,k) set of existing arcs between j and k
            /1.1,1.2,1.3,2.1,2.2,2.3,3.1,3.2,3.3/
        SA3(k,l) set of existing arcs between k and l
            /1.1,1.2,1.3,2.1,2.2,2.3,3.1,3.2,3.3/
;
***************************************************************
parameters
        S(i) capacity of supply station i /1 30, 2 15, 3 5/
        D(l) demand at arrival station l  /1 10, 2 25, 3 15/
        C1(i,j) Cost coefficient between nodes i and j
                /1.1 40, 1.2 80, 1.3 20
                 2.1 50, 2.2 30, 2.3 90
                 3.1 60, 3.2 40, 3.3 20 /
        C2(j,k) Cost coefficient between nodes j and k
                /1.1 20, 1.2 40, 1.3 80
                 2.1 30, 2.2 80, 2.3 60
                 3.1 80, 3.2 70, 3.3 20 /
        C3(k,l) Cost coefficient between nodes k and l
                /1.1 20, 1.2 10, 1.3 40
                 2.1 70, 2.2 30, 2.3 60
                 3.1 80, 3.2 20, 3.3 70 /
        r1(i,j) resource to interdict arc between i and j
        r2(j,k) resource to interdict arc between j and k
        r3(k,l) resource to interdict arc  between k and l
        TIR     total interdiction resources available
;
* Enter r values below
r1(i,j)= 1;
r2(j,k)= 1;
r3(k,l)= 1;
* Enter total interdiction resource available below
TIR=1;
scalar
      M an arbitrarily large positive number
         /1000/
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
       Z =E= sum((i),S(i)*w1(i))- sum((l),D(l)*w4(l))+ sum((i,j)$SA1(i,j),M*q1(i,j))+ sum((j,k)$SA2(j,k),M*q2(j,k))+ sum((k,l)$SA3(k,l),M*q3(k,l));
ConstraintD1(i,j) $ (SA1(i,j))..
                  w1(i)-w2(j)+v1(i,j)=L= C1(i,j);
ConstraintD2(j,k) $ (SA2(j,k))..
                  w2(j)-w3(k)+v2(j,k)=L= C2(j,k);
ConstraintD3(k,l) $ (SA3(k,l))..
                  w3(k)-w4(l)+v3(k,l)=L= C3(k,l);
ConstraintTIR..
                  sum((i,j)$SA1(i,j), r1(i,j)*y1(i,j)) + sum((j,k)$SA2(j,k), r2(j,k)*y2(j,k))+ sum((k,l)$SA3(k,l), r3(k,l)*y3(k,l)) =L= TIR;
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
*y.fx('f')=1;
Model_TNMI.optcr=0.00;
Model_TNMI.iterlim=1000000;
Model_TNMI.reslim=10000;
solve Model_TNMI using MIP Maximizing Z;
*Display w1.l, w2.l,w3.l,w4.l, y1.l,y2.l,y3.l, v1.l,v2.l,v3.l, q1.l,q2.l,q3.l;
*Display i,j,k,l,SA1,SA2,SA3,S;
Display y1.l, y2.l, y3.l, Z.l;

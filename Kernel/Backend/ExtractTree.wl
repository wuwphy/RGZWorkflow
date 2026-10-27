(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* ============================================================*)

(*s=p^2*)
(* ============================================================*)Clear[L2,terms,s,hasField,readMix,readXiMix,aMass,aDer,aCross,phK,phM,beK,beM,hPhi,hPhibar,hBeta,hBetabar,hA,hXi,bMass,xiMass,xiDer,bA,tauA,tauXi,aXi,kAA,kPhi,kBeta,KT,DT,KL,DL,qL,dAAT,dAAL,checkT,checkL];

(*------------------------------------------------------------*)

(*------------------------------------------------------------*)

L2=Expand[GetQuadraticTerms[LTotal(*/. rho->0*)]];

terms=If[Head[L2]===Plus,List@@L2,{L2}];


hasField[expr_,field_Symbol]:=!FreeQ[expr,x_[___]/;x===field];

(*------------------------------------------------------------*)

(*------------------------------------------------------------*)

aMass=Total@Cases[terms,HoldPattern[z_. A[u_,i_]^2]:>z];

aDer=Total@Cases[terms,HoldPattern[z_. del[A[u_,i_],v_]^2]:>z];

aCross=Total@Cases[terms,HoldPattern[z_. del[A[u_,i_],v_] del[A[v_,i_],u_]]:>z];

phK=Total@Cases[terms,HoldPattern[z_. del[ϕ[u_,i_,j_],v_]*del[ϕbar[u_,i_,j_],v_]]:>z];

phM=Total@Cases[terms,HoldPattern[z_. ϕ[u_,i_,j_] ϕbar[u_,i_,j_]]:>z];

beK=Total@Cases[terms,HoldPattern[z_. del[β[u_,i_,j_],v_]*del[βbar[u_,i_,j_],v_]]:>z];

beM=Total@Cases[terms,HoldPattern[z_. β[u_,i_,j_] βbar[u_,i_,j_]]:>z];


readMix[field_Symbol]:=Module[{selected,coefficients},selected=Select[terms,hasField[#,A]&&hasField[#,field]&];
coefficients=selected/.
x_[___]/;MemberQ[{A,f,field},x]:>1;
FullSimplify[Sqrt[Nc] Total[coefficients]]];

{hPhi,hPhibar,hBeta,hBetabar}=readMix/@{ϕ,ϕbar,β,βbar};

hA={hPhi,hPhibar,hBeta,hBetabar};

Print["Transverserequired Coefficient: "];
Print[{aMass,aDer,aCross,phK,phM,beK,beM,hPhi,hPhibar,hBeta,hBetabar}];

(*------------------------------------------------------------*)


(*------------------------------------------------------------*)


kAA=FullSimplify[2 aMass+2 s aDer];
kPhi=FullSimplify[s phK+phM];
kBeta=FullSimplify[s beK+beM];

KT=ConstantArray[0,{5,5}];

KT[[1,1]]=kAA;

Do[KT[[1,j+1]]=hA[[j]];
KT[[j+1,1]]=hA[[j]],{j,4}];

KT[[2,3]]=KT[[3,2]]=kPhi;
KT[[4,5]]=KT[[5,4]]=kBeta;

Print["TransverseKernel KT: "];
KT//MatrixForm

DT=FullSimplify[I Inverse[KT]];

dAAT=Factor[Together[DT[[1,1]]]];
checkT=FullSimplify[KT.DT-I IdentityMatrix[5]];

Print["Transverse gluon propagator scalar coefficient: "];
dAAT

Print["Transverse inverse check (expected zero matrix): "];
checkT//MatrixForm

(*------------------------------------------------------------*)

(*------------------------------------------------------------*)

bMass=Total@Cases[terms,HoldPattern[z_. b[i_]^2]:>z];

xiMass=Total@Cases[terms,HoldPattern[z_. ξ[i_]^2]:>z];

xiDer=Total@Cases[terms,HoldPattern[z_. del[ξ[i_],u_]^2]:>z];

bA=Total@Cases[terms,HoldPattern[z_. b[i_] del[A[u_,i_],u_]]:>z];

tauA=Total@Cases[terms,HoldPattern[z_. A[u_,i_] del[τ[i_],u_]]:>z];

tauXi=Total@Cases[terms,HoldPattern[z_. del[ξ[i_],u_] del[τ[i_],u_]]:>z];

aXi=Total@Cases[terms,HoldPattern[z_. A[u_,i_] del[ξ[i_],u_]]:>z];


readXiMix[field_Symbol]:=Module[{selected,coefficients},selected=Select[terms,hasField[#,ξ]&&hasField[#,field]&];
coefficients=selected/. HoldPattern[del[ξ[__],_]]->1;
coefficients=coefficients/. x_[___]/;MemberQ[{f,field},x]:>1;
FullSimplify[Sqrt[Nc] Total[coefficients]]];

hXi=readXiMix/@{ϕ,ϕbar,β,βbar};

Print["LongitudinalNewincrement Coefficient: "];
Print[{bMass,xiMass,xiDer,bA,tauA,tauXi,aXi}];

Print["Check hXi + hA (should IsfourItemsZero)
: "];
FullSimplify[hXi+hA]

(*------------------------------------------------------------*)


(*A_L^mu=p^mu A_L/Sqrt[s]*)
(*------------------------------------------------------------*)

qL=Sqrt[s];
KL=ConstantArray[0,{8,8}];


KL[[1,1]]=2 aMass;
KL[[2,2]]=2 bMass;
KL[[4,4]]=2 (s xiDer+xiMass);

(*b partial.A*)
KL[[1,2]]=I qL bA;
KL[[2,1]]=-I qL bA;

(*A partial.tau*)
KL[[1,3]]=-I qL tauA;
KL[[3,1]]=I qL tauA;

(*A partial.xi*)
KL[[1,4]]=-I qL aXi;
KL[[4,1]]=I qL aXi;

(*partial.xi partial.tau*)
KL[[3,4]]=s tauXi;
KL[[4,3]]=s tauXi;


Do[KL[[1,4+j]]=hA[[j]];
KL[[4+j,1]]=hA[[j]];
KL[[4,4+j]]=I qL hXi[[j]];
KL[[4+j,4]]=-I qL hXi[[j]],{j,4}];


KL[[5,6]]=KL[[6,5]]=kPhi;
KL[[7,8]]=KL[[8,7]]=kBeta;

Print["LongitudinalKernel KL: "];
KL//MatrixForm


dAAL=Factor@Together@FullSimplify[I LinearSolve[KL,UnitVector[8,1]][[1]]];

Print["Longitudinal gluon propagator scalar coefficient: "];
dAAL


DL=FullSimplify[I Inverse[KL]];

checkL=FullSimplify[KL.DL-I IdentityMatrix[8]];

Print["Longitudinal inverse check (expected zero matrix): "];
checkL//MatrixForm


rgzExtractedTree=<|"KT"->KT,"KL"->KL,"DT"->DT,"DL"->DL,"CheckT"->checkT,"CheckL"->checkL,"AA_T"->dAAT,"AA_L"->dAAL|>;

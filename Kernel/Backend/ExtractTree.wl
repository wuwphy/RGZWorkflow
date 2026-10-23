(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* ============================================================*)
(*RGZ \:4e8c\:6b21\:6838\:4e0e\:4f20\:64ad\:5b50\:ff1a\:5148\:8fd0\:884c LoadModel\:ff0c\:518d\:8fd0\:884c\:672c\:6bb5\:4ee3\:7801*)
(*s=p^2*)
(* ============================================================*)Clear[L2,terms,s,hasField,readMix,readXiMix,aMass,aDer,aCross,phK,phM,beK,beM,hPhi,hPhibar,hBeta,hBetabar,hA,hXi,bMass,xiMass,xiDer,bA,tauA,tauXi,aXi,kAA,kPhi,kBeta,KT,DT,KL,DL,qL,dAAT,dAAL,checkT,checkL];

(*------------------------------------------------------------*)
(*1. \:63d0\:53d6\:4e8c\:6b21\:9879*)
(*------------------------------------------------------------*)

L2=Expand[GetQuadraticTerms[LTotal(*/. rho->0*)]];

terms=If[Head[L2]===Plus,List@@L2,{L2}];

(*\:5224\:65ad\:67d0\:9879\:662f\:5426\:542b\:6307\:5b9a\:573a\:ff1b\:5bfc\:6570\:5185\:90e8\:7684\:573a\:4e5f\:80fd\:8bc6\:522b*)
hasField[expr_,field_Symbol]:=!FreeQ[expr,x_[___]/;x===field];

(*------------------------------------------------------------*)
(*2. \:4ece L2 \:8bfb\:53d6\:80f6\:5b50\:548c\:8f85\:52a9\:573a\:4e8c\:6b21\:9879*)
(*------------------------------------------------------------*)

aMass=Total@Cases[terms,HoldPattern[z_. A[u_,i_]^2]:>z];

aDer=Total@Cases[terms,HoldPattern[z_. del[A[u_,i_],v_]^2]:>z];

aCross=Total@Cases[terms,HoldPattern[z_. del[A[u_,i_],v_] del[A[v_,i_],u_]]:>z];

phK=Total@Cases[terms,HoldPattern[z_. del[\:03d5[u_,i_,j_],v_]*del[\:03d5bar[u_,i_,j_],v_]]:>z];

phM=Total@Cases[terms,HoldPattern[z_. \:03d5[u_,i_,j_] \:03d5bar[u_,i_,j_]]:>z];

beK=Total@Cases[terms,HoldPattern[z_. del[\:03b2[u_,i_,j_],v_]*del[\:03b2bar[u_,i_,j_],v_]]:>z];

beM=Total@Cases[terms,HoldPattern[z_. \:03b2[u_,i_,j_] \:03b2bar[u_,i_,j_]]:>z];

(*A \:4e0e\:8f85\:52a9\:573a\:7684\:6df7\:5408\:ff1bSqrt[Nc] \:6765\:81ea f^{abc} \:989c\:8272\:6295\:5f71*)
readMix[field_Symbol]:=Module[{selected,coefficients},selected=Select[terms,hasField[#,A]&&hasField[#,field]&];
coefficients=selected/.
x_[___]/;MemberQ[{A,f,field},x]:>1;
FullSimplify[Sqrt[Nc] Total[coefficients]]];

{hPhi,hPhibar,hBeta,hBetabar}=readMix/@{\:03d5,\:03d5bar,\:03b2,\:03b2bar};

hA={hPhi,hPhibar,hBeta,hBetabar};

Print["\:6a2a\:5411\:6240\:9700\:7cfb\:6570\:ff1a"];
Print[{aMass,aDer,aCross,phK,phM,beK,beM,hPhi,hPhibar,hBeta,hBetabar}];

(*------------------------------------------------------------*)
(*3. \:6a2a\:5411 5 x 5 \:6838*)
(*\:573a\:987a\:5e8f {A_T,phi_T,phibar_T,beta_T,betabar_T}*)
(*------------------------------------------------------------*)

(*aCross \:542b p.A_T\:ff0c\:6a2a\:5411\:6295\:5f71\:540e\:4e3a\:96f6*)
kAA=FullSimplify[2 aMass+2 s aDer];
kPhi=FullSimplify[s phK+phM];
kBeta=FullSimplify[s beK+beM];

KT=ConstantArray[0,{5,5}];

KT[[1,1]]=kAA;

Do[KT[[1,j+1]]=hA[[j]];
KT[[j+1,1]]=hA[[j]],{j,4}];

KT[[2,3]]=KT[[3,2]]=kPhi;
KT[[4,5]]=KT[[5,4]]=kBeta;

Print["\:6a2a\:5411\:6838 KT\:ff1a"];
KT//MatrixForm

DT=FullSimplify[I Inverse[KT]];

dAAT=Factor[Together[DT[[1,1]]]];
checkT=FullSimplify[KT.DT-I IdentityMatrix[5]];

Print["\:6a2a\:5411\:80f6\:5b50\:4f20\:64ad\:5b50\:7684\:6807\:91cf\:7cfb
\:6570\:ff1a"];
dAAT

Print["\:6a2a\:5411\:53cd\:6f14\:68c0\:67e5\:ff08\:5e94\:4e3a\:96f6\:77e9
\:9635\:ff09\:ff1a"];
checkT//MatrixForm

(*------------------------------------------------------------*)
(*4. \:4ece L2 \:8bfb\:53d6\:7eb5\:5411\:90e8\:95e8\:7684\:5176\:4f59\:7cfb\:6570*)
(*------------------------------------------------------------*)

bMass=Total@Cases[terms,HoldPattern[z_. b[i_]^2]:>z];

xiMass=Total@Cases[terms,HoldPattern[z_. \:03be[i_]^2]:>z];

xiDer=Total@Cases[terms,HoldPattern[z_. del[\:03be[i_],u_]^2]:>z];

bA=Total@Cases[terms,HoldPattern[z_. b[i_] del[A[u_,i_],u_]]:>z];

tauA=Total@Cases[terms,HoldPattern[z_. A[u_,i_] del[\:03c4[i_],u_]]:>z];

tauXi=Total@Cases[terms,HoldPattern[z_. del[\:03be[i_],u_] del[\:03c4[i_],u_]]:>z];

aXi=Total@Cases[terms,HoldPattern[z_. A[u_,i_] del[\:03be[i_],u_]]:>z];

(*partial.xi \:4e0e\:8f85\:52a9\:573a\:7684\:6df7\:5408*)
readXiMix[field_Symbol]:=Module[{selected,coefficients},selected=Select[terms,hasField[#,\:03be]&&hasField[#,field]&];
coefficients=selected/. HoldPattern[del[\:03be[__],_]]->1;
coefficients=coefficients/. x_[___]/;MemberQ[{f,field},x]:>1;
FullSimplify[Sqrt[Nc] Total[coefficients]]];

hXi=readXiMix/@{\:03d5,\:03d5bar,\:03b2,\:03b2bar};

Print["\:7eb5\:5411\:65b0\:589e\:7cfb\:6570\:ff1a"];
Print[{bMass,xiMass,xiDer,bA,tauA,tauXi,aXi}];

Print["\:68c0\:67e5 hXi + hA\:ff08\:5e94\:4e3a\:56db\:4e2a\:96f6\:ff09
\:ff1a"];
FullSimplify[hXi+hA]

(*------------------------------------------------------------*)
(*5. \:7eb5\:5411 8 x 8 \:6838*)
(*\:573a\:987a\:5e8f {A_L,b,tau,xi,phi_L,phibar_L,beta_L,betabar_L}*)
(*A_L^mu=p^mu A_L/Sqrt[s]*)
(*------------------------------------------------------------*)

qL=Sqrt[s];
KL=ConstantArray[0,{8,8}];

(*\:5bf9\:89d2\:9879*)
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

(*A\:3001xi \:4e0e\:8f85\:52a9\:573a*)
Do[KL[[1,4+j]]=hA[[j]];
KL[[4+j,1]]=hA[[j]];
KL[[4,4+j]]=I qL hXi[[j]];
KL[[4+j,4]]=-I qL hXi[[j]],{j,4}];

(*\:8f85\:52a9\:573a\:4e8c\:6b21\:9879*)
KL[[5,6]]=KL[[6,5]]=kPhi;
KL[[7,8]]=KL[[8,7]]=kBeta;

Print["\:7eb5\:5411\:6838 KL\:ff1a"];
KL//MatrixForm

(*\:5148\:5355\:72ec\:6c42 A_L A_L\:ff1b\:6bd4\:663e\:793a\:6574\:4e2a\:9006\:77e9\:9635\:66f4\:5bb9\:6613\:68c0
\:67e5*)
dAAL=Factor@Together@FullSimplify[I LinearSolve[KL,UnitVector[8,1]][[1]]];

Print["\:7eb5\:5411\:80f6\:5b50\:4f20\:64ad\:5b50\:7684\:6807\:91cf\:7cfb
\:6570\:ff1a"];
dAAL

(*\:6700\:540e\:6c42\:5b8c\:6574 8 x 8 \:4f20\:64ad\:5b50\:77e9\:9635*)
DL=FullSimplify[I Inverse[KL]];

checkL=FullSimplify[KL.DL-I IdentityMatrix[8]];

Print["\:7eb5\:5411\:53cd\:6f14\:68c0\:67e5\:ff08\:5e94\:4e3a\:96f6\:77e9
\:9635\:ff09\:ff1a"];
checkL//MatrixForm

(*\:5982\:9700\:67e5\:770b\:5168\:90e8\:7eb5\:5411\:4f20\:64ad\:5b50\:ff0c\:5355\:72ec\:6267\:884c\:ff1aDL//MatrixForm*)
rgzExtractedTree=<|"KT"->KT,"KL"->KL,"DT"->DT,"DL"->DL,"CheckT"->checkT,"CheckL"->checkL,"AA_T"->dAAT,"AA_L"->dAAL|>;

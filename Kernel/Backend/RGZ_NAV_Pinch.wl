(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* On-shell quark Ward decomposition. k=r-q, ell=p+q, equal external
   quark masses. gamma_sigma (slash(k)+mq) slash(q)/Dk -> -gamma_sigma;
   slash(ell) (slash(k)+mq) gamma_rho/Dk -> -gamma_rho.
   Right contraction is applied first, so double-longitudinal terms
   are counted once. The remainder retains the internal quark pole. *)
rgzNAVWardSplit[tensor_]:=Module[{poly,right,left,remainder,pinched,restore,colour},
 colour=tensor/.SUNF[rgzNAVA,rgzNAVB,rgzNAVC]->1;
 If[!FreeQ[colour,SUNF|SUNDelta|SUND|SUNT|SUNIndex],Return[Failure["NAVColourNotFactored",<||>]]];
 poly=colour/.{
  FVD[v_,rgzNAVRho]:>Coefficient[Expand[v],p] rgzNAVRP+Coefficient[Expand[v],q] rgzNAVRQ,
  FVD[v_,rgzNAVSigma]:>Coefficient[Expand[v],p] rgzNAVSP+Coefficient[Expand[v],q](rgzNAVSL-rgzNAVSP)};
 poly=Expand[poly,rgzNAVRQ|rgzNAVSL];
 If[!PolynomialQ[poly,{rgzNAVRP,rgzNAVRQ,rgzNAVSP,rgzNAVSL}]||Exponent[poly,rgzNAVRQ]>1||Exponent[poly,rgzNAVSL]>1,
  Return[Failure["NAVWardPolynomial",<||>]]];
 right=Coefficient[poly,rgzNAVRQ];
 remainder=poly/.rgzNAVRQ->0;
 left=Coefficient[remainder,rgzNAVSL];remainder=remainder/.rgzNAVSL->0;
 restore={rgzNAVRP->FVD[p,rgzNAVRho],rgzNAVRQ->FVD[q,rgzNAVRho],rgzNAVSP->FVD[p,rgzNAVSigma],rgzNAVSL->FVD[p+q,rgzNAVSigma]};
 pinched=(right/.restore/.rgzNAVSigma->rgzNAVPinchMu)+(left/.restore/.rgzNAVRho->rgzNAVPinchMu);
 <|"PinchTensor"->pinched,"GenuineVertexTensor"->(remainder/.restore),"RightCoefficient"->(right/.restore),"LeftCoefficient"->(left/.restore),"ExactPolynomialReconstruction"->TrueQ[Cancel[Together[poly-rgzNAVRQ right-rgzNAVSL left-remainder]]===0]|>
];

rgzNAVPinchBuild[nav_,dir_,seconds_]:=Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{rows={},record,split,kernel,file,packet,r},
 Do[file=FileNameJoin[{dir,"ward-"<>ToString[n]<>".wxf"}];
 If[FileExistsQ[file],record=Import[file,"WXF"],
 split=TimeConstrained[rgzNAVWardSplit[nav["Connections"][[n]]["TransverseTensor"]],seconds,$TimedOut];
 If[split===$TimedOut,Throw[Failure["NAVWardTimeout",<|"Connection"->n|>],rgzFunctionReturnTag]];
 If[FailureQ[split],Throw[split,rgzFunctionReturnTag]];
 If[!TrueQ[split["ExactPolynomialReconstruction"]],Throw[Failure["NAVWardReconstruction",<|"Connection"->n|>],rgzFunctionReturnTag]];
 kernel=TimeConstrained[FCE[Contract[FCI[I SMP["g_s"] SUNN (split["PinchTensor"]/.q->-q)] FCI[RGZLoopPT[p,\[Mu],rgzNAVPinchMu]]]/.rgzNAVNu->\[Nu]],seconds,$TimedOut];
 If[kernel===$TimedOut,Throw[Failure["NAVPinchProjectionTimeout",<|"Connection"->n|>],rgzFunctionReturnTag]];
 record=<|"Connection"->n,"Fields"->nav["Connections"][[n]]["Fields"],"Kernel"->kernel,"ExactPolynomialReconstruction"->True|>;
 r=RGZWorkflow`Private`atomicWXF[file,record];If[FailureQ[r],Throw[r,rgzFunctionReturnTag]]];
 If[!TrueQ[RGZWorkflow`Private`navPinchConnectionQ[record,n,nav["Connections"][[n]]["Fields"]]],Throw[Failure["NAVWardCacheInvalid",<|"File"->file,"Connection"->n|>],rgzFunctionReturnTag]];
 AppendTo[rows,record];Print["NAV Ward ",n," / ",nav["ConnectionCount"]],{n,nav["ConnectionCount"]}];
 packet=<|"Schema"->"RGZNAVPinch/1","ConnectionCount"->Length[rows],"Connections"->rows,"Kernel"->Total[Lookup[rows,"Kernel"]],
 "Representation"->"Rational","Meaning"->"Two-sided on-shell quark Ward pinch of the complete NAV field matrix; replaces the AA NAV contribution."|>;
 r=RGZWorkflow`Private`atomicWXF[FileNameJoin[{dir,"result.wxf"}],packet];If[FailureQ[r],r,packet]
],rgzFunctionReturnTag]];
(* Many matrix connections share identical scalar pole factors. Memoize
   exact partial fractions within one split; never bypass reconstruction. *)
rgzNAVSplit[input_]:=Module[{memo=<||>},
 Block[{RGZMXSplitFactor},
  RGZMXSplitFactor[den_,var_,route_]:=Module[{key=HoldComplete[den,var,route]/.var->rgzNAVPoleVariable,z},
   If[KeyExistsQ[memo,key],Return[memo[key]]];
   z=rgzNAVExactPoleFactor[den,var,route];If[!FailureQ[z],AssociateTo[memo,key->z]];z];
  RGZMXSplit[input]]];
(* Exact polynomial arithmetic suffices for these known quadratic poles;
   avoid repeated assumption-driven simplification of unrelated roots. *)
rgzNAVExactPoleFactor[den_,var_,route_]:=Module[{roots,r,mass,degree,order,quot,coefficient,back=0,out=0},
 If[!PolynomialQ[den,var],Return[Failure["NAVNonPolynomialPole",<||>]]];
 roots={{0,0},{\[Mu]^2,\[Mu]},{-I Sqrt[\[Alpha] M^4],rgzLoopM1},{I Sqrt[\[Alpha] M^4],rgzLoopM2},
 {(m^2+\[Mu]^2)/2+I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],rgzLoopM3},
 {(m^2+\[Mu]^2)/2-I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],rgzLoopM4}};
 degree=Exponent[den,var];
 Do[r=pole[[1]];mass=pole[[2]];
 If[TrueQ[Cancel[Expand[den/.var->r]]===0],
 order=1;While[order<degree&&TrueQ[Cancel[Expand[D[den,{var,order}]/.var->r]]===0],order++];
 quot=PolynomialQuotient[den,(var-r)^order,var];
 Do[coefficient=Cancel[Together[(D[1/quot,{var,order-j}]/.var->r)/Factorial[order-j]]];
 back+=coefficient/(var-r)^j;out+=coefficient ChangeDimension[FCI[FAD@@ConstantArray[{route,mass},j]],D],{j,1,order}]],{pole,roots}];
 If[!TrueQ[Cancel[Together[1/den-back]]===0],Return[Failure["NAVPoleReconstruction",<|"Denominator"->den|>]]];out];

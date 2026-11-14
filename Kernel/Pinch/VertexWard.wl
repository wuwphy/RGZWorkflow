(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Extract candidate NAV pinch terms using the explicit vertex Ward decomposition.

   Inputs:
   Explicitly requested candidate kernels, mixed matrices and (where required) the full NAV/QCD reference.

   Dependencies:
   Shared Algebra/ tools and OneLoop/ results. The conventional QQ route does not call this branch.
*)

(* On-shell quark Ward decomposition. k=r-q, ell=p+q, equal external
   quark masses. gamma_sigma (slash(k)+mq) slash(q)/Dk -> -gamma_sigma;
   slash(ell) (slash(k)+mq) gamma_rho/Dk -> -gamma_rho.
   Right contraction is applied first, so double-longitudinal terms
   are counted once. The remainder retains the internal quark pole. *)
rgzNAVWardSplit[tensor_]:=Module[{poly,right,left,remainder,pinched,restore,colour,routes,coeffs,restored,answer,colourProbe=Unique["rgzNAVColour$"]},
 If[!RGZMXValidValueQ[tensor],Return[Failure["InvalidNAVWardInput",<||>]]];
 If[Or@@{OwnValues[p]=!={},OwnValues[q]=!={},OwnValues[rgzNAVRP]=!={},
   OwnValues[rgzNAVRQ]=!={},OwnValues[rgzNAVSP]=!={},OwnValues[rgzNAVSL]=!={}},
  Return[Failure["AssignedNAVWardCoordinate",<||>]]];
 If[!FreeQ[tensor,rgzNAVRP|rgzNAVRQ|rgzNAVSP|rgzNAVSL],Return[Failure["NAVWardCoordinateCollision",<||>]]];
 colour=FCE[FCI[tensor]];
 routes=Cases[colour,FVD[v_,rgzNAVRho|rgzNAVSigma]:>v,{0,Infinity}];
 If[!AllTrue[routes,Function[v,
   coeffs={Coefficient[Expand[v],p],Coefficient[Expand[v],q]};
   FreeQ[coeffs,p|q] && TrueQ[Expand[v-coeffs[[1]] p-coeffs[[2]] q]===0]]],
  Return[Failure["UnsupportedNAVWardMomentum",<||>]]];
 (* A colour-free tensor is an explicitly factored compatibility input.
    Otherwise require exactly one common external structure constant. *)
 If[!FreeQ[colour,SUNF[rgzNAVA,rgzNAVB,rgzNAVC]],
  colour=colour/.SUNF[rgzNAVA,rgzNAVB,rgzNAVC]->colourProbe;
  If[!PolynomialQ[colour,colourProbe] || Exponent[colour,colourProbe]=!=1 ||
     !TrueQ[Expand[Coefficient[colour,colourProbe,0]]===0],
   Return[Failure["NAVColourStructure",<||>]]];
  colour=Coefficient[colour,colourProbe]];
 If[!FreeQ[FCI[colour],_SUNF|_SUNDelta|_SUND|_SUNT|_SUNIndex],Return[Failure["NAVColourNotFactored",<||>]]];
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
 restored=Expand[ExpandScalarProduct[FCI[colour-(poly/.restore)]]];
 If[!RGZMXValidValueQ[restored] || !TrueQ[Cancel[Together[restored]]===0],
  Return[Failure["NAVWardCoordinateReconstruction",<||>]]];
 pinched=(right/.restore/.rgzNAVSigma->rgzNAVPinchMu)+(left/.restore/.rgzNAVRho->rgzNAVPinchMu);
 answer=<|"PinchTensor"->pinched,"GenuineVertexTensor"->(remainder/.restore),"RightCoefficient"->(right/.restore),"LeftCoefficient"->(left/.restore),"ExactPolynomialReconstruction"->TrueQ[Cancel[Together[poly-rgzNAVRQ right-rgzNAVSL left-remainder]]===0],"OriginalTensorReconstruction"->True|>;
 If[RGZMXValidValueQ[answer],answer,Failure["InvalidNAVWardResult",<||>]]
];

rgzNAVPinchBuild[nav_,dir_,seconds_]:=Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{rows={},record,split,kernel,file,packet,r,connection,source},
 If[!NumericQ[seconds] || !TrueQ[seconds>0] || !StringQ[dir] ||
   !AssociationQ[nav] || !IntegerQ[Lookup[nav,"ConnectionCount",None]] || nav["ConnectionCount"]<1 ||
   !ListQ[Lookup[nav,"Connections",None]] || Length[nav["Connections"]]=!=nav["ConnectionCount"] ||
   !AllTrue[nav["Connections"],AssociationQ[#] && MatchQ[Lookup[#,"Fields",None],{_String,_String,_String}] &&
     AllTrue[#["Fields"],MemberQ[RGZWorkflow`Private`navMatrixFields,#]&] &&
     KeyExistsQ[#,"TransverseTensor"] && RGZMXValidValueQ[#["TransverseTensor"]]&],
  Throw[Failure["InvalidNAVPinchInput",<||>],rgzFunctionReturnTag]];
 If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
 Do[connection=nav["Connections"][[n]];
 source=RGZWorkflow`Private`navPinchInputIdentity[connection];
 file=FileNameJoin[{dir,"ward-"<>ToString[n]<>".wxf"}];
 record=If[FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],Missing["NoCache"]];
 If[!TrueQ[RGZWorkflow`Private`navPinchConnectionQ[record,n,connection["Fields"],source]],
 split=TimeConstrained[rgzNAVWardSplit[connection["TransverseTensor"]],seconds,$TimedOut];
 If[split===$TimedOut,Throw[Failure["NAVWardTimeout",<|"Connection"->n|>],rgzFunctionReturnTag]];
 If[FailureQ[split],Throw[split,rgzFunctionReturnTag]];
 If[!AssociationQ[split] || !RGZMXValidValueQ[split] ||
   !TrueQ[split["ExactPolynomialReconstruction"]] || !TrueQ[split["OriginalTensorReconstruction"]],
  Throw[Failure["NAVWardReconstruction",<|"Connection"->n|>],rgzFunctionReturnTag]];
 kernel=TimeConstrained[FCE[Contract[FCI[I SMP["g_s"] SUNN (split["PinchTensor"]/.q->-q)] FCI[RGZLoopPT[p,\[Mu],rgzNAVPinchMu]]]/.rgzNAVNu->\[Nu]],seconds,$TimedOut];
 If[!RGZMXValidValueQ[kernel],Throw[Failure["NAVPinchProjectionStopped",<|"Connection"->n|>],rgzFunctionReturnTag]];
 record=<|"Connection"->n,"Fields"->connection["Fields"],"Kernel"->kernel,
  "ExactPolynomialReconstruction"->True,"OriginalTensorReconstruction"->True,
  "ValidationPolicy"->"NAVWard/3","InputIdentity"->source,"KernelHash"->Hash[kernel,"SHA256"]|>;
 r=RGZWorkflow`Private`atomicWXF[file,record];If[FailureQ[r],Throw[r,rgzFunctionReturnTag]]];
 If[!TrueQ[RGZWorkflow`Private`navPinchConnectionQ[record,n,connection["Fields"],source]],
  Throw[Failure["NAVWardCacheInvalid",<|"File"->file,"Connection"->n|>],rgzFunctionReturnTag]];
 AppendTo[rows,record];Print["NAV Ward ",n," / ",nav["ConnectionCount"]],{n,nav["ConnectionCount"]}];
 packet=<|"Schema"->"RGZNAVPinch/1","ConnectionCount"->Length[rows],"Connections"->rows,"Kernel"->Total[Lookup[rows,"Kernel"]],
 "Representation"->"Rational","Meaning"->"Two-sided on-shell quark Ward pinch of the complete NAV field matrix; replaces the AA NAV contribution."|>;
 If[!RGZWorkflow`Private`navPacketQ[packet,"RGZNAVPinch/1",{"Kernel"}],Throw[Failure["InvalidNAVPinchResult",<||>],rgzFunctionReturnTag]];
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
rgzNAVExactPoleFactor[den_,var_,route_]:=Block[{$Assumptions=True},Module[
 {roots,r,mass,degree,order,quot,coefficient,back=0,out=0,emitted,probe=Unique["rgzNAVPoleProbe$"]},
 If[Or@@{OwnValues[rgzLoopM1]=!={},OwnValues[rgzLoopM2]=!={},OwnValues[rgzLoopM3]=!={},OwnValues[rgzLoopM4]=!={}},
  Return[Failure["AssignedNAVPoleLabel",<||>]]];
 If[!RGZMXValidValueQ[{den,var,route}],Return[Failure["InvalidNAVPoleInput",<||>]]];
 If[!PolynomialQ[den,var],Return[Failure["NAVNonPolynomialPole",<||>]]];
 roots={{0,0},{\[Mu]^2,\[Mu]},{-I Sqrt[\[Alpha] M^4],rgzLoopM1},{I Sqrt[\[Alpha] M^4],rgzLoopM2},
 {(m^2+\[Mu]^2)/2+I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],rgzLoopM3},
 {(m^2+\[Mu]^2)/2-I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4],rgzLoopM4}};
 (* Coalescing candidates are one root; multiplicity comes from den. *)
 roots=DeleteDuplicates[roots,TrueQ[Cancel[Together[#1[[1]]-#2[[1]]]]===0]&];
 degree=Exponent[den,var];
 Do[r=pole[[1]];mass=pole[[2]];
 If[TrueQ[Cancel[Expand[den/.var->r]]===0],
 order=1;While[order<degree&&TrueQ[Cancel[Expand[D[den,{var,order}]/.var->r]]===0],order++];
 quot=PolynomialQuotient[den,(var-r)^order,var];
 Do[coefficient=Cancel[Together[(D[1/quot,{var,order-j}]/.var->r)/Factorial[order-j]]];
 back+=coefficient/(var-r)^j;out+=coefficient ChangeDimension[FCI[FAD@@ConstantArray[{route,mass},j]],D],{j,1,order}]],{pole,roots}];
 If[!RGZMXValidValueQ[back] || !TrueQ[Cancel[Together[1/den-back]]===0],
  Return[Failure["NAVPoleReconstruction",<|"Denominator"->den|>]]];
 (* Verify the actual output, not only the abstract partial fractions. *)
 With[{momentum=MomentumExpand[FCI[Momentum[route,D]]]},
  emitted=out/.HoldPattern[PropagatorDenominator[momentum,label_]]:>
   PropagatorDenominator[Momentum[probe,D],label]];
 emitted=FeynAmpDenominatorExplicit[RGZLoopPoleMasses[emitted]]/.
  Pair[Momentum[probe,D],Momentum[probe,D]]->var;
 If[!RGZMXValidValueQ[{out,emitted}] || !TrueQ[Cancel[Together[1/den-emitted]]===0],
  Return[Failure["NAVEmittedPoleReconstruction",<|"Denominator"->den|>]]];out]];

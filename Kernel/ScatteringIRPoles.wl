(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Endpoint IR poles without a real-mass-only integral backend.
   This is a certificate on a stated domain, not an evaluation of finite
   complex-mass integrals or an arbitrary analytic continuation. *)
qqIRLinearMoment[a_,b_,A_,B_] := Module[{y,poly,quot,rem},
 poly=Expand[y^a (1-y)^b];
 If[TrueQ[B===0],Return[Factorial[a] Factorial[b]/(Factorial[a+b+1] A)]];
 quot=PolynomialQuotient[poly,A+B y,y];rem=PolynomialRemainder[poly,A+B y,y];
 Total[Table[Coefficient[quot,y,k]/(k+1),{k,0,Max[0,Exponent[quot,y]]}]]+
 rem (Log[A+B]-Log[A])/B];
qqIREndpointCertificate[m_] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{v=qqPVParse[m],n,ind,j,counts,rank,dist,zeros,mass,conditions={},candidates={},edge,comp,A,B,value,method,k,a,b},
 If[v===$Failed,Return[$Failed]];{ind,edge,mass}=v;n=Length[mass];
 If[!MemberQ[{2,3,4},n]||Length[edge]=!=n(n-1)/2,Return[$Failed]];
 If[ind==={0},ind={}];
 If[!AllTrue[ind,IntegerQ[#]&&0<=#<n&]||OddQ[Count[ind,0]],Return[$Failed]];
 j=Count[ind,0]/2;counts=Prepend[Table[Count[ind,k],{k,1,n-1}],0];rank=Total[counts];
 zeros=Flatten[Position[mass,0,{1},Heads->False]];
 (* More than two massless lines can have overlapping soft/collinear poles.
    Leave them to the established real-mass backend or a future certificate. *)
 If[Length[zeros]>2||Length[zeros]===n,Return[$Failed]];
 dist=ConstantArray[0,{n,n}];
 Do[{a,b}=qqPVEdges[n][[k]]+1;dist[[a,b]]=dist[[b,a]]=edge[[k]],{k,Length[edge]}];
 (* Positive real parts of each massive parameter and its slopes away from
    a massless vertex exclude massive soft thresholds in this domain. *)
 Do[If[!MemberQ[zeros,k],AppendTo[conditions,Re[mass[[k]]]>0]],{k,n}];
 Do[If[!MemberQ[zeros,k],A=mass[[k]]-dist[[z,k]];
   If[TrueQ[A===0],Throw[$Failed,rgzFunctionReturnTag]];AppendTo[conditions,Re[A]>0]],{z,zeros},{k,n}];
 If[Length[zeros]===2,
  {a,b}=zeros;
  If[TrueQ[dist[[a,b]]===0],AppendTo[candidates,zeros],AppendTo[conditions,Unequal[dist[[a,b]],0]]]];
 conditions=DeleteDuplicates[DeleteCases[conditions,True]];
 If[MemberQ[conditions,False],Return[$Failed]];
 If[candidates==={},Return[<|"IR"->0,"IRMethod"->"NoSoftOrCollinearEndpoint","IRConditions"->conditions|>]];
 {a,b}=First[candidates];comp=Complement[Range[n],{a,b}];
 (* On a two-parameter massless face, the radial integrand is
    lambda^(-1+j+Sum[counts on complement]-epsilon). Positive powers
    suppress the collinear pole. PaVe {0} is scalar, NOT one metric. *)
 If[j+Total[counts[[comp]]]>0,Return[<|"IR"->0,"IRMethod"->"CollinearEndpointSuppressedByTensorMoment","IRConditions"->conditions|>]];
 If[n=!=3,Return[$Failed]];k=First[comp];
 A=mass[[k]]-dist[[b,k]];B=dist[[b,k]]-dist[[a,k]];
 If[TrueQ[A===0]||TrueQ[A+B===0],Return[$Failed]];
 If[!TrueQ[B===0],conditions=DeleteDuplicates[Append[conditions,Unequal[B,0]]]];
 value=(-1)^rank qqIRLinearMoment[counts[[a]],counts[[b]],A,B]/FeynCalc`EpsilonIR;
 <|"IR"->value,"IRMethod"->"TriangleMasslessFaceMoment","IRConditions"->conditions,
 "IRDerivation"->"(-1)^R/epsilon_IR times integral_0^1 y^a(1-y)^b/(A+B y) dy; principal log difference on Re(A),Re(A+B)>0"|>],rgzFunctionReturnTag]];

(* Keep logarithms as independent atoms. Rational cancellation between
   masters must happen before interpreting a large pole as a residual. *)
qqUVIRCompactPole[input_] := Module[{v=qqCanonicalSpectralRadicals[input],logs,slots,poly,cs,out},
 If[TrueQ[v===0],Return[0]];
 logs=DeleteDuplicates[Cases[v,_Log,Infinity]];
 slots=Table[Unique["RGZWorkflow`Private`irPoleLog"],{Length[logs]}];
 poly=Expand[v/.Thread[logs->slots]];
 If[slots=!={}&&!PolynomialQ[poly,slots],Return[input]];
 cs=If[slots==={},{{{},poly}},CoefficientRules[poly,slots]];
 out=Total[Map[Function[pair,Module[{co=Cancel[Together[Last[pair]]]},
  co=Cancel[Expand[Numerator[co]]/Expand[Denominator[co]]];
  co Times@@MapThread[Power,{slots,First[pair]}]]],cs]]/.Thread[slots->logs];
 If[qqFFInvalidQ[out],input,qqCoefficientCompact[input,out]]];

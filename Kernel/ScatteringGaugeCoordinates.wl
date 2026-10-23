(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Cancel rational loop-coordinate factors BEFORE partial fractions/TID.
   Coordinate identities use the same massless physical momentum convention
   as the existing box Ward preconditioner. *)
qqGaugeEarlyCoordinateNormalize[input_,name_,alpha_,seconds_] := Module[
 {q=RGZWorkflow`Engine`q,k1=RGZWorkflow`Engine`qqK1,k2=RGZWorkflow`Engine`qqK2,
  k3=RGZWorkflow`Engine`qqK3,k4=RGZWorkflow`Engine`qqK4,s=RGZWorkflow`Engine`qqS,t=RGZWorkflow`Engine`qqT,
  w0,w1,w2,w3,vars,qs,rhs,back,pair,parts,co,result={},terms={},cc,r,dd,degree,dimDegree,dimPoly,check,restored,stop=Unique["earlyCoordinates"]},Catch[
 If[!MemberQ[{"NAV1","NAV2","Box1","Box2"},name],Return[input]];
 pair[a_,b_]:=FeynCalc`FCI[FeynCalc`SPD[a,b]];
 Switch[name,
 "NAV1"|"NAV2",
  {qs,rhs}=If[name==="NAV1",{{pair[q,q],pair[q,k1],pair[q,k3]},
    {w0,(w0-w2)/2,(w1-t-w2)/2}},{{pair[q,q],pair[q,k2],pair[q,k4]},
    {w0,(w0-w2)/2,(w1-t-w2)/2}}];vars={w0,w1,w2};
  back=If[name==="NAV1",{w0->pair[q,q],w1->pair[q-k1+k3,q-k1+k3],w2->pair[q-k1,q-k1]},
    {w0->pair[q,q],w1->pair[q-k2+k4,q-k2+k4],w2->pair[q-k2,q-k2]}],
 "Box1"|"Box2",
  qs={pair[q,q],pair[q,k1],pair[q,k2],pair[q,k3]};vars={w0,w1,w2,w3};
  rhs=If[name==="Box1",{w0,(w0-w1)/2,(w2-w0)/2,(w3-w1-t)/2},
    {w0,(w0-w1)/2,(w2-w3+s+t)/2,(w0-w3)/2}];
  back=If[name==="Box1",{w0->pair[q,q],w1->pair[q-k1,q-k1],w2->pair[q+k2,q+k2],w3->pair[q-k1+k3,q-k1+k3]},
    {w0->pair[q,q],w1->pair[q-k1,q-k1],w2->pair[q+k2-k3,q+k2-k3],w3->pair[q-k3,q-k3]}]];
 check=RGZWorkflow`Private`qqFFScalarKinematics[rhs/.back]-qs;
 If[!AllTrue[check,TrueQ[Cancel[Expand[#]]===0]&],Throw[Failure["QQEarlyCoordinateIdentity",<|"Part"->name|>],stop]];
 parts=qqFFStructuralParts[input];If[FailureQ[parts],Throw[parts,stop]];
 Do[qqProgressOperation["Cancel loop-coordinate coefficients: "<>name<>" "<>ToString[i]<>" / "<>ToString[Length[parts]]];
 co=qqFFScalarKinematics[FeynCalc`FeynAmpDenominatorExplicit[parts[[i,3]]]];
 If[MemberQ[{"Box1","Box2"},name],co=co/.{pair[q,k4]->pair[q,k1]+pair[q,k2]-pair[q,k3],pair[k4,q]->pair[q,k1]+pair[q,k2]-pair[q,k3]}];
 co=co/.Dispatch[Join[Thread[qs->rhs],Thread[(Reverse /@qs)->rhs]]];
 If[!FreeQ[co,q]||!PolynomialQ[co,alpha],Throw[Failure["QQEarlyUnmappedCoordinate",<|"Part"->name,"Structure"->i|>],stop]];
 degree=Exponent[co,alpha];terms={};
 Do[cc=Coefficient[co,alpha,a];dimPoly=PolynomialQ[cc,D];dimDegree=If[dimPoly,Max[0,Exponent[cc,D]],0];
 Do[dd=If[dimPoly,Coefficient[cc,D,d],cc];
 r=TimeConstrained[Cancel[Together[dd]],seconds,$TimedOut];
 If[qqFFInvalidQ[r],Throw[Failure["QQEarlyCoordinateStopped",<|"Part"->name,"Structure"->i,"GaugeDegree"->a,"Seconds"->seconds|>],stop]];
 If[r=!=0,r=TimeConstrained[Numerator[r]/Factor[Denominator[r]],seconds,$TimedOut];
  If[qqFFInvalidQ[r],Throw[Failure["QQEarlyCoordinateStopped",<|"Part"->name,"Structure"->i,"Operation"->"Factor denominator","Seconds"->seconds|>],stop]];
  AppendTo[terms,r alpha^a If[dimPoly,D^d,1]]],{d,0,dimDegree}],{a,0,Max[0,degree]}];
 restored=Total[terms]/.back;
 If[!FreeQ[restored,Alternatives@@vars],Throw[Failure["QQEarlyCoordinateLeak",<|"Part"->name|>],stop]];
 AppendTo[result,parts[[i,1]] parts[[i,2]] restored],{i,Length[parts]}];
 Total[result],stop]];

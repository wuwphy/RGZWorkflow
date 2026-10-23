(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Rational numerator collection with explicit denominator factors. Work in
   the spectral coordinate ring, including the original mG^2 relation. No
   full multivariate factorization of the large numerator is required. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringCoefficientNumerator.wl"}]];
qqCoefficientSpectralGrouped[input_,u_,v_,mu_,mg_,mode_:"Reduce"] := Module[
 {terms,normalized={},n,d,r,relation=mg^4-(u^2-mu^2)(v^2-mu^2),facts,
  powers=<||>,known,mult,polys,common,numerator,splitFactors,stop=Unique["spectralGrouped"]},Catch[
 terms=If[Head[input]===Plus,List@@input,{input}];
 Do[qqProgressOperation["Normalize spectral denominator "<>ToString[i]<>" / "<>ToString[Length[terms]]];
  r=terms[[i]];n=Numerator[r];d=Denominator[r];
  If[!FreeQ[n,Power[_,k_Integer?Negative]],
   r=Cancel[Together[r]];n=Numerator[r];d=Denominator[r]];
  If[!PolynomialQ[n,mg]||!PolynomialQ[d,mg],Throw[$Failed,stop]];
  If[!FreeQ[n,mg],n=PolynomialRemainder[n,relation,mg]];
  d=Factor[PolynomialRemainder[d,relation,mg]];
  If[d===0||qqFFInvalidQ[n]||qqFFInvalidQ[d],Throw[$Failed,stop]];
  If[n===0,Continue[]];
  AppendTo[normalized,{n,d}],{i,Length[terms]}];
 If[normalized==={},Return[0]];
 normalized=({Total[#[[All,1]]],#[[1,2]]}& /@ GatherBy[normalized,Last]);
 splitFactors[ff_] := Module[{unit=1,factors={}},
  If[!ListQ[ff]||!AllTrue[ff,MatchQ[#,{_,_Integer}]&],Throw[$Failed,stop]];
  Do[If[NumericQ[fac[[1]]],unit*=fac[[1]]^fac[[2]],
   If[fac[[2]]<0,Throw[$Failed,stop]];
   If[fac[[2]]>0,AppendTo[factors,fac]]],{fac,ff}];{unit,factors}];
 facts=splitFactors[FactorList[#[[2]]]]& /@ normalized;
 Do[Do[AssociateTo[powers,fac[[1]]->Max[Lookup[powers,fac[[1]],0],fac[[2]]]],{fac,ff[[2]]}],{ff,facts}];
 common=Times@@KeyValueMap[#1^#2&,powers];
 polys=Table[known=Association[Rule@@@facts[[i,2]]];
  mult=Times@@KeyValueMap[#1^(#2-Lookup[known,#1,0])&,powers];
  normalized[[i,1]]/facts[[i,1]] mult,{i,Length[normalized]}];
 If[mode==="Numerator",Return[<|"Polynomials"->polys,"Denominator"->common,"Relation"->relation|>]];
 qqProgressOperation["Collect spectral polynomial numerator (no full Factor)"];
 numerator=qqNumeratorBlocked[polys,u,v,mu,mg];
 If[qqFFInvalidQ[numerator],Return[numerator]];
 If[numerator===0,0,numerator/common],stop]];

(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Principal powers obey (c z)^p = c^p z^p for strictly positive real c.
   Only exact positive rational factors of the known spectral radicands are
   extracted. In particular Sqrt[z^2] is NEVER replaced by z. *)
qqCanonicalSpectralRadicals[input_] := Module[
 {delta=RGZWorkflow`Engine`mG^4-(RGZWorkflow`Engine`m^2-RGZWorkflow`Engine`\[Mu]^2)^2/4,powers,rules={},b,p,ratio},
 powers=DeleteDuplicates[Cases[input,Power[_,e_Rational]/;Denominator[e]===2,{0,Infinity}]];
 Do[{b,p}=List@@power;If[LeafCount[b]>2000,Continue[]];
 ratio=TimeConstrained[Cancel[Together[Expand[b]/delta]],1/4,$TimedOut];
 If[MatchQ[ratio,_Integer|_Rational]&&TrueQ[ratio>0],AppendTo[rules,power->ratio^p delta^p]],{power,powers}];
 input/.Dispatch[rules]];
qqBranchCanonicalCoefficient[input_] := Module[{normalized=qqCanonicalSpectralRadicals[input]},
 If[normalized===input,input,Cancel[Together[normalized]]]];
qqBranchCanonicalComparison[packet_] := Module[{rows,zero},
 If[!AssociationQ[packet]||!ListQ[Lookup[packet,"Rows",None]]||
 !AllTrue[packet["Rows"],AssociationQ[#]&&KeyExistsQ[#,"Difference"]&],Return[packet]];
 rows=Map[Function[row,Module[{d=qqBranchCanonicalCoefficient[row["Difference"]]},
  Join[row,<|"Difference"->d,"Status"->If[TrueQ[d===0],"Zero","Uncancelled"]|>]]],packet["Rows"]];
 zero=AllTrue[rows,TrueQ[#["Difference"]===0]&];
 Join[packet,<|"Rows"->rows,"Status"->If[zero,"VerifiedSymbolicZero","UncancelledCoefficients"],
 "AllCoefficientsZero"->zero,"Statistics"->Counts[Lookup[rows,"Status"]],
 "SpectralBranchNormalization"->"PositiveRationalRadicandScaling/1"|>]];
(* Normalize final exact differences, including valid older coefficient
   checkpoints. Physical diagram fingerprints and their caches do not change. *)
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[If[UnsameQ[value_Symbol,0],Increment[count_Symbol]]] :>
  CompoundExpression[value=qqBranchCanonicalCoefficient[value],If[UnsameQ[value,0],Increment[count]]];

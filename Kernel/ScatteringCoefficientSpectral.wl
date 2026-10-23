(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Exact spectral coordinates for scalar coefficients. The principal square
   roots defining the two pole masses are retained in the inverse map. *)
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringCoefficientGrouped.wl"}]];
$qqCoefficientSpectralVersion=StringTake[IntegerString[Hash[{FileHash[$InputFileName,"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringCoefficientGrouped.wl"}],"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringCoefficientNumerator.wl"}],"SHA256"]},"SHA256"],16,64],12];
qqCoefficientSpectralNativeFactor[input_] := Factor[input];
qqCoefficientSpectral[input_,method_:Automatic] := Module[{m=RGZWorkflow`Engine`m,
 mu=RGZWorkflow`Engine`\[Mu],mg=RGZWorkflow`Engine`mG,x,y,z,base,minus,plus,
 roots,rules={},ratio,u,v,back,trans,result,chosen,matchedBase=False,matchedPole=False},
 x=m^2;y=mu^2;z=mg^4;base=z-(x-y)^2/4;
 minus=(x+y)/2-I Sqrt[base];plus=(x+y)/2+I Sqrt[base];
 roots=DeleteDuplicates[Cases[input,Power[_,1/2],Infinity]];
 If[roots==={},Return[$Failed]];
 u=Unique["RGZWorkflow`Private`spectralMinus"];v=Unique["RGZWorkflow`Private`spectralPlus"];
 Do[Which[
   TrueQ[Cancel[Together[root[[1]]-minus]]===0],AppendTo[rules,root->u];matchedPole=True,
   TrueQ[Cancel[Together[root[[1]]-plus]]===0],AppendTo[rules,root->v];matchedPole=True,
   True,ratio=Cancel[Together[root[[1]]/base]];
    If[MatchQ[ratio,_Integer|_Rational]&&TrueQ[ratio>0],
     AppendTo[rules,root->Sqrt[ratio] (v^2-u^2)/(2 I)];matchedBase=True]],{root,roots}];
 (* Polynomial pole-mass coefficients often contain only the discriminant,
    with no square root of a pole mass left. The same inverse identities
    remain exact in that sector; do not send it to the opaque-root fallback. *)
 If[!matchedBase,Return[$Failed]];
 trans=input/.Dispatch[rules];
 trans=With[{ms=m,mgs=mg},trans/.{
  HoldPattern[Power[ms,n_Integer]]/;EvenQ[n]:>(u^2+v^2-y)^(n/2),
  HoldPattern[Power[mgs,n_Integer]]/;Divisible[n,4]:>((u^2-y)(v^2-y))^(n/4)}];
 back={u->Sqrt[minus],v->Sqrt[plus]};
 If[method==="Coordinates",Return[<|"Input"->trans,"Back"->back|>]];
 chosen=If[method===Automatic,If[Max[LeafCount[input],LeafCount[trans]]>=250000,"Grouped","Factor"],method];
 qqProgressUpdate[<|"SpectralMethod"->chosen,"SpectralCoordinateLeaves"->LeafCount[trans],"SpectralImplementation"->$qqCoefficientSpectralVersion|>];
 qqProgressOperation[If[chosen==="Grouped","Collect rational coefficient in spectral coordinates","Factor coefficient in exact spectral mass coordinates"]];
 result=Switch[chosen,
  "Grouped",qqCoefficientSpectralGrouped[trans,u,v,mu,mg],
  "Factor",If[method===Automatic,
   With[{probe=TimeConstrained[qqCoefficientSpectralNativeFactor[trans],5,$TimedOut]},
    If[probe===$TimedOut,
     qqProgressUpdate[<|"SpectralMethod"->"GroupedAfterFactorProbe","SpectralFactorProbeSeconds"->5|>];
     qqProgressOperation["Factor probe timed out; collect rational coefficient in spectral coordinates"];
     qqCoefficientSpectralGrouped[trans,u,v,mu,mg],probe]],
   qqCoefficientSpectralNativeFactor[trans]],
  "Cancel",Cancel[Together[trans]],_,Return[$Failed]];
 If[qqFFInvalidQ[result],Return[result]];
 If[result=!=0&&LeafCount[result]>LeafCount[trans],result=trans];
 qqProgressOperation["Restore exact pole-mass square roots"];
 result=result/.back;
 If[!FreeQ[result,u|v],Return[$Failed]];result];

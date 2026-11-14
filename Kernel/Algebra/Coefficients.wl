(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Reduce collected numerator and spectral coefficients with principal-root branch conventions.

   Inputs:
   Symbolic D-dimensional tensors, scalar masters and their collected coefficients.

   Dependencies:
   FeynCalc for tensor/PV expressions; Internal/ supplies progress and persistence.
*)


(* Exact spectral coefficient algebra *)
(* Polynomial numerators *)
(* Sparse polynomial collection in selected variables; scalar blocks stay opaque. *)
qqNumeratorSparseRules[input_,vars_] := Module[{parse,multiply,add,result,degree,zero,cache=<||>,pat=Alternatives@@vars,stop=Unique["sparseStop"]},zero=degree[ConstantArray[0,Length[vars]]];Internal`WithLocalSettings[Null,Catch[
 multiply[a_,b_] := Module[{out=<||>,ak,bk,key},
 If[a===<||>||b===<||>,Return[<||>]];
 If[Length[a]===1,ak=First[Keys[a]];Return[Association[KeyValueMap[degree[ak[[1]]+#1[[1]]]->(a[ak] #2)&,b]]]];
 If[Length[b]===1,Return[multiply[b,a]]];
 Do[key=degree[ak[[1]]+bk[[1]]];AssociateTo[out,key->(Lookup[out,key,0]+a[ak] b[bk])],{ak,Keys[a]},{bk,Keys[b]}];out];
 parse[e_] := Module[{key,z,parts,n,result,base,exponent},
 If[FreeQ[e,pat],Return[If[e===0,<||>,<|zero->e|>]]];
 key=HoldComplete[e];If[KeyExistsQ[cache,key],Return[cache[key]]];
 result=Which[MemberQ[vars,e],<|degree[UnitVector[Length[vars],First[FirstPosition[vars,e]]]]->1|>,
 Head[e]===Plus,Merge[parse/@List@@e,Total],
 Head[e]===Times,parts=SortBy[parse/@List@@e,Length];Fold[multiply,<|zero->1|>,parts],
 Head[e]===Power&&IntegerQ[e[[2]]]&&e[[2]]>=0,
  n=e[[2]];base=parse[e[[1]]];z=<|zero->1|>;
  While[n>0,If[OddQ[n],z=multiply[z,base]];n=Quotient[n,2];If[n>0,base=multiply[base,base]]];z,
 True,Throw[$Failed,stop]];
 If[LeafCount[e]<500,AssociateTo[cache,key->result]];result];
 result=parse[input];If[result===$Failed,Throw[$Failed,stop]];
 KeyValueMap[#1[[1]]->#2&,result],stop],Clear[parse,multiply];cache=<||>]];
qqNumeratorBlocked[polys_,u_,v_,mu_,mg_,mode_:Automatic] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[
 {sum=Total[polys],vars,alpha,pattern,mask,memo=<||>,values={},slots=0,slot,
  masked,rules,back,relation=mg^4-(u^2-mu^2)(v^2-mu^2),root=None,checkpoint,
  canonical,toCanonical,fromCanonical,key,file,p,c,out={},plan,planKey,probe,reduceMass,reused=0,saved=0},Internal`WithLocalSettings[Null,
 reduceMass[x_] := If[qqFFInvalidQ[x]||FreeQ[x,mg],x,PolynomialRemainder[x,relation,mg]];
 If[mode=!= "Blocked"&&LeafCount[sum]<100000,Return[reduceMass[Expand[sum]]]];
 alpha=DeleteDuplicates[Cases[sum,s_Symbol/;StringStartsQ[Context[s],"RGZWorkflow`Gauge`"]&&SymbolName[s]==="alpha",Infinity]];
 vars=Select[Join[alpha,{D}],!FreeQ[sum,#]&];
 If[vars==={},Return[reduceMass[Expand[sum]]]];
 checkpoint=Lookup[$qqProgress,"CheckpointFile",None];
 If[StringQ[checkpoint]&&DirectoryQ[DirectoryName[checkpoint]],
  root=FileNameJoin[{DirectoryName[checkpoint],"numerator-coefficients"}];
  If[!DirectoryQ[root],CreateDirectory[root,CreateIntermediateDirectories->True]]];
 toCanonical={u->RGZWorkflow`Private`qqNumeratorU,v->RGZWorkflow`Private`qqNumeratorV};
 fromCanonical=Reverse/@toCanonical;
 planKey=Hash[{"QQNumeratorPlan/1",sum/.toCanonical,relation/.toCanonical,vars},"SHA256"];
 plan=If[StringQ[root],FileNameJoin[{root,"plan-"<>IntegerString[planKey,16,64]<>".wxf"}],None];
 qqProgressUpdate[<|"NumeratorCoefficientIndex"->0,"NumeratorCoefficientTotal"->0,"NumeratorCacheReused"->0,"NumeratorCacheSaved"->0|>];
 If[mode=!= "Blocked"&&(!StringQ[plan]||!FileExistsQ[plan]),
  qqProgressOperation["Try direct numerator expansion (15 s probe)"];
  probe=TimeConstrained[Expand[sum],15,$TimedOut];
  If[probe=!=$TimedOut,Return[reduceMass[probe]]];
  If[StringQ[plan],p=atomicWXF[plan,<|"Key"->planKey,"NativeExpandTimedOut"->True|>];If[FailureQ[p],Return[p]]]];
 pattern=Alternatives@@vars;
 mask[x_] := If[FreeQ[x,pattern],If[NumberQ[x],x,With[{k=HoldComplete[x]},
  If[KeyExistsQ[memo,k],memo[k],slots++;AppendTo[values,slot[slots]->x];AssociateTo[memo,k->slot[slots]];slot[slots]]]],Map[mask,x]];
 qqProgressOperation["Collect numerator in gauge parameter and D; protect mass polynomials"];
 masked=mask[sum];rules=qqNumeratorSparseRules[masked,vars];
 If[rules===$Failed,Return[reduceMass[Expand[sum]]]];back=Dispatch[values];
 qqProgressUpdate[<|"NumeratorCoefficientTotal"->Length[rules],"NumeratorCacheReused"->0,"NumeratorCacheSaved"->0|>];
 Do[qqProgressOperation["Expand numerator coefficient "<>ToString[i]<>" / "<>ToString[Length[rules]]];
  qqProgressUpdate[<|"NumeratorCoefficientIndex"->i|>];
  c=Last[rules[[i]]]/.back;canonical=c/.toCanonical;
  key=Hash[{"QQNumeratorCoefficient/1",canonical,relation/.toCanonical},"SHA256"];
  file=If[StringQ[root],FileNameJoin[{root,IntegerString[key,16,64]<>".wxf"}],None];
  qqProgressUpdate[<|"NumeratorCoefficientLeaves"->LeafCount[c],"NumeratorCheckpointFile"->file|>];
  p=If[StringQ[file]&&FileExistsQ[file],Quiet[Check[Import[file,"WXF"],$Failed]],$Failed];
  If[AssociationQ[p]&&Lookup[p,"Key",None]===key&&!qqFFInvalidQ[Lookup[p,"Value",$Failed]],
   c=p["Value"]/.fromCanonical;reused++,
   c=Expand[c];If[!FreeQ[c,mg],c=PolynomialRemainder[c,relation,mg]];
   If[qqFFInvalidQ[c],Throw[c,rgzFunctionReturnTag]];
   If[StringQ[file],p=atomicWXF[file,<|"Key"->key,"Value"->(c/.toCanonical)|>];If[FailureQ[p],Throw[p,rgzFunctionReturnTag]];saved++]];
  qqProgressUpdate[<|"NumeratorCacheReused"->reused,"NumeratorCacheSaved"->saved|>];
  If[c=!=0,AppendTo[out,c Times@@MapThread[Power,{vars,First[rules[[i]]]}]]],{i,Length[rules]}];
 Total[out],Clear[mask,reduceMass];memo=<||>;values={}]],rgzFunctionReturnTag]];

(* Grouping rational terms *)
(* Rational numerator collection with explicit denominator factors. Work in
   the spectral coordinate ring, including the original mG^2 relation. No
   full multivariate factorization of the large numerator is required. *)
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

(* Spectral pole coordinates *)
(* Exact spectral coordinates for scalar coefficients. The principal square
   roots defining the two pole masses are retained in the inverse map. *)
$qqCoefficientSpectralVersion=StringTake[IntegerString[Hash[{FileHash[$InputFileName,"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","Algebra/Coefficients.wl"}],"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","Algebra/Coefficients.wl"}],"SHA256"]},"SHA256"],16,64],12];
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

(* Exact coefficient reduction *)
(* Exact balanced rational merges, with restartable algebra checkpoints.
   This file is excluded from physical diagram fingerprints. *)
$qqCoefficientBlockLeaves=5000;
qqCoefficientCompact[input_,candidate_] := If[candidate===0,0,
 If[LeafCount[candidate]<=LeafCount[input],candidate,input]];
(* Treat repeated radicals as opaque algebraic variables during rational
   arithmetic. Substitute the original expressions back exactly; do not use
   PowerExpand, select branches, or impose a mass point. *)
qqCoefficientRational[original_] := Module[{input=qqCanonicalSpectralRadicals[original],roots,vars,forward,back,spectral},
 If[LeafCount[input]<1000,
  qqProgressOperation["Reduce small rational coefficient"];
  qqProgressUpdate[<|"SpectralMethod"->"NativeSmall"|>];Return[Cancel[Together[input]]]];
 spectral=qqCoefficientSpectral[input];
 If[spectral=!=$Failed,Return[spectral]];
 qqProgressOperation["Reduce coefficient with independent radical placeholders (fallback)"];
 qqProgressUpdate[<|"SpectralMethod"->"IndependentRadicalsFallback"|>];
 roots=DeleteDuplicates[Cases[input,Power[_,n_Rational]/;!IntegerQ[n],Infinity]];
 If[roots==={}||Length[roots]>8,Return[Cancel[Together[input]]]];
 vars=Table[Unique["RGZWorkflow`Private`coefficientRoot"],{Length[roots]}];
 forward=Thread[roots->vars];back=Thread[vars->roots];
 Cancel[Together[input/.forward]]/.back];
(* Content-addressed children let an interrupted large denominator group
   retain work inside that group. Only integer powers are traversed: never
   distribute noninteger powers or change a radical branch. *)
qqCoefficientSubReduce[input_,dir_] := Module[{key,file,p,result,children,n,snapshot,fast,head=Head[input]},
 If[!AssociationQ[$qqProgress],qqProgressUpdate[<||>]];
 (* Small leaves can still be algebraically hard. They need both an operation
    label and a checkpoint; previously this early return bypassed both. *)
 key=Hash[{"QQCoefficientChildren/1",input},"SHA256"];
 file=FileNameJoin[{dir,"sub-"<>IntegerString[key,16,64]<>".wxf"}];
 If[FileExistsQ[file],p=Import[file,"WXF"];
  If[AssociationQ[p]&&Lookup[p,"Key",None]===key&&!qqFFInvalidQ[Lookup[p,"Value",$Failed]],
   qqProgressUpdate[<|"CoefficientSubReused"->(Lookup[$qqProgress,"CoefficientSubReused",0]+1)|>];Return[qqCoefficientCompact[input,p["Value"]]]]];
 (* A whole block can be easy in spectral coordinates even if traversing
    hundreds of its children exhausts the coefficient-wide budget. Try the
    exact direct path first, with a bounded probe, before recursive descent. *)
 If[LeafCount[input]>=5000,
  snapshot=file<>".input.wxf";
  p=atomicWXF[snapshot,<|"Schema"->"QQCoefficientPending/1","Key"->key,"Input"->input|>];
  If[FailureQ[p],Return[p]];
  qqProgressOperation["Try direct spectral block before recursive subdivision"];
  qqProgressUpdate[<|"InputLeaves"->LeafCount[input],"CheckpointFile"->file,"PendingInputFile"->snapshot,
   "NumeratorCoefficientIndex"->0,"NumeratorCoefficientTotal"->0,"NumeratorCacheReused"->0,"NumeratorCacheSaved"->0|>];
  fast=TimeConstrained[qqCoefficientSpectral[input],If[LeafCount[input]>=500000,30,15],$TimedOut];
  If[fast===$TimedOut&&Lookup[$qqProgress,"NumeratorCoefficientTotal",0]>0,
   qqProgressOperation["Continue checkpointed numerator within the existing coefficient budget"];
   fast=qqCoefficientSpectral[input]];
  If[fast=!=$Failed&&!qqFFInvalidQ[fast],
   result=qqCoefficientCompact[input,fast];
   p=atomicWXF[file,<|"Key"->key,"Value"->result|>];If[FailureQ[p],Return[p]];
   qqProgressUpdate[<|"CoefficientSubSaved"->(Lookup[$qqProgress,"CoefficientSubSaved",0]+1)|>];Return[result]]];
 Which[LeafCount[input]<$qqCoefficientBlockLeaves,result=input,
  head===Plus&&Length[input]>1,
   children=List@@input;n=Quotient[Length[children],2];
   result=qqCoefficientSubReduce[Total[Take[children,n]],dir]+qqCoefficientSubReduce[Total[Drop[children,n]],dir],
  head===Times,
   result=Times@@(qqCoefficientSubReduce[#,dir]& /@ List@@input),
  head===Power&&IntegerQ[input[[2]]],
   result=qqCoefficientSubReduce[input[[1]],dir]^input[[2]],
  True,result=input];
 If[qqFFInvalidQ[result],Return[result]];
 result=qqCoefficientCompact[input,result];
 qqProgressOperation["Reduce coefficient subgroup ("<>ToString[LeafCount[result]]<>" leaves)"];
 qqProgressUpdate[<|"InputLeaves"->LeafCount[result],"CheckpointFile"->file|>];
 (* Save the exact pending algebra problem, not a completion marker. A
    timeout must leave enough information to reproduce the hard merge in
    an isolated kernel without replaying the scattering calculation. *)
 If[LeafCount[result]>=5000,
  snapshot=file<>".input.wxf";
  p=atomicWXF[snapshot,<|"Schema"->"QQCoefficientPending/1","Key"->key,"Input"->result|>];
  If[FailureQ[p],Return[p]];
  qqProgressUpdate[<|"PendingInputFile"->snapshot|>]];
 children=result;
 result=qqCoefficientRational[result];
 If[qqFFInvalidQ[result],Return[result]];
 result=qqCoefficientCompact[children,result];
 p=atomicWXF[file,<|"Key"->key,"Value"->result|>];If[FailureQ[p],Return[p]];
 qqProgressUpdate[<|"CoefficientSubSaved"->(Lookup[$qqProgress,"CoefficientSubSaved",0]+1)|>];result];
qqCoefficientReduce[input_] := Module[{terms,groups,level=0,nodes,dir,key,node,
 read=0,written=0,merge,stop=Unique["qqCoefficientStop"]},Catch[
 key=IntegerString[Hash[{"QQCoefficient/1",input},"SHA256"],16,64];
 qqProgressUpdate[<|"CoefficientSubReused"->0,"CoefficientSubSaved"->0|>];
 dir=FileNameJoin[{cacheDirectory[],"qq-coefficients",key}];
 If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
 merge[value_,level_,index_] := Module[{file,p,result},
  file=FileNameJoin[{dir,ToString[level]<>"-"<>ToString[index]<>".wxf"}];
  If[FileExistsQ[file],p=Import[file,"WXF"];
   If[AssociationQ[p]&&Lookup[p,"Key",None]===key&&
     Lookup[p,"InputHash",None]===Hash[value,"SHA256"]&&
     !qqFFInvalidQ[Lookup[p,"Value",$Failed]],read++;Return[qqCoefficientCompact[value,p["Value"]]]]];
  qqProgressOperation["Rational merge level "<>ToString[level]<>" block "<>ToString[index]];
  qqProgressUpdate[<|"CoefficientCacheReused"->read,"CoefficientCacheSaved"->written,
    "InputLeaves"->LeafCount[value],"CheckpointFile"->file,
    "AlgebraGroupIndex"->index,"AlgebraGroupTotal"->If[level===0,Length[groups],Ceiling[Length[nodes]/2]]|>];
  result=qqCoefficientSubReduce[value,dir];
  If[qqFFInvalidQ[result],Throw[result,stop]];
  p=atomicWXF[file,<|"Key"->key,"InputHash"->Hash[value,"SHA256"],"Value"->result|>];
  If[FailureQ[p],Throw[p,stop]];written++;result];
 (* Expanding every scalar numerator before grouping destroys the factored
    blocks retained by PV collection. Together accepts unexpanded products
    and sums; only split the existing outer sum. Old node checkpoints remain
    valid because their complete input hash is checked before reuse. *)
 qqProgressOperation["Group factored rational coefficient terms"];
 node=input;terms=If[Head[node]===Plus,List@@node,{node}];
 groups=GatherBy[terms,Denominator];
 nodes=Table[merge[Total[groups[[i]]],0,i],{i,Length[groups]}];
 While[Length[nodes]>1,level++;
  (* Preserve deterministic grouping across retries. *)
  nodes=Table[If[i===Length[nodes],nodes[[i]],
    merge[nodes[[i]]+nodes[[i+1]],level,(i+1)/2]],{i,1,Length[nodes],2}]];
 qqProgressUpdate[<|"CoefficientCacheReused"->read,"CoefficientCacheSaved"->written|>];
 If[nodes==={},0,First[nodes]],stop]];

(* Preserve the comparison cache key while making a stopped coefficient
   diagnosable. The final coefficient file is written only after success. *)
qqCoefficientStopDetails[value_,stage_,file_,seconds_] := Failure["QQGaugeCoefficientStopped",
 <|"Stage"->stage,"File"->file,"Seconds"->seconds,
 "Reason"->Which[value===$TimedOut,"CoefficientTimeout",value===$Aborted,"CoefficientAborted",
   FailureQ[value],"CoefficientFailure",True,"InvalidCoefficientExpression"],
 "Progress"->KeyTake[$qqProgress,{"Operation","InputLeaves","AlgebraGroupIndex","AlgebraGroupTotal",
   "CoefficientCacheReused","CoefficientCacheSaved","CoefficientSubReused","CoefficientSubSaved","CheckpointFile","PendingInputFile","Master","Index","Total",
   "GaugeDifferenceInputLeaves","GaugeDifferenceOutputLeaves","GaugeDifferencePVBefore","GaugeDifferencePVAfter","ScalarBasisReusedAcrossTimeLimits",
   "SpectralMethod","SpectralCoordinateLeaves","SpectralImplementation","SpectralFactorProbeSeconds",
   "NumeratorCoefficientIndex","NumeratorCoefficientTotal","NumeratorCacheReused","NumeratorCacheSaved","NumeratorCoefficientLeaves","NumeratorCheckpointFile"}],
 "Action"->"Repeat with the desired CoefficientSeconds; completed checkpoints are reused."|>];

(* Principal-root branch conventions *)
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

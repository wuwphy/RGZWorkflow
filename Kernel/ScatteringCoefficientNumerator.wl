(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
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
qqNumeratorBlocked[polys_,u_,v_,mu_,mg_,mode_:Automatic] := Module[
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
   If[qqFFInvalidQ[c],Return[c]];
   If[StringQ[file],p=atomicWXF[file,<|"Key"->key,"Value"->(c/.toCanonical)|>];If[FailureQ[p],Return[p]];saved++]];
  qqProgressUpdate[<|"NumeratorCacheReused"->reused,"NumeratorCacheSaved"->saved|>];
  If[c=!=0,AppendTo[out,c Times@@MapThread[Power,{vars,First[rules[[i]]]}]]],{i,Length[rules]}];
 Total[out],Clear[mask,reduceMass];memo=<||>;values={}]];

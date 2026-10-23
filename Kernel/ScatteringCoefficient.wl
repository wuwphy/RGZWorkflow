(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Exact balanced rational merges, with restartable algebra checkpoints.
   This file is excluded from physical diagram fingerprints. *)
$qqCoefficientBlockLeaves=5000;
Get[FileNameJoin[{$packageRoot,"Kernel","ScatteringCoefficientSpectral.wl"}]];
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

(* Upgrade comparison algebra without changing physical job or alpha IDs.
   Loading ScatteringGauge.wl first makes repeated package loads idempotent. *)
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[Cancel[Together[value_]]]:>qqCoefficientReduce[value];
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[FileHash[path_,"SHA256"]]:>Hash[{FileHash[path,"SHA256"],"QQCoefficient/1"},"SHA256"];
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
DownValues[qqGaugeCompare]=DownValues[qqGaugeCompare]/.
 HoldPattern[Failure["QQGaugeCoefficientStopped",_Association]]:>
  qqCoefficientStopDetails[d,tag,file,coSeconds];

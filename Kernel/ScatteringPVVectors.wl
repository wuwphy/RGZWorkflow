(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Apply integral identities to a coefficient vector, not by substituting
   expanded identities throughout the amplitude. Keep scalar products factored. *)
qqFFVectorVersion=Hash[{"QQPVVector/1",FileHash[$InputFileName,"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringFormFactors.wl"}],"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVRelations.wl"}],"SHA256"],
 FileHash[FileNameJoin[{$packageRoot,"Kernel","ScatteringPVGram.wl"}],"SHA256"]},"SHA256"];
qqFFVectorContext[] := Hash[{qqFFVectorVersion,$Version,FeynCalc`$FeynCalcVersion,
 DownValues[FeynCalc`PaVeReduce],DownValues[FeynCalc`PaVeOrder],
 Options[FeynCalc`PaVeReduce],Options[FeynCalc`PaVeOrder]},"SHA256"];
qqFFVectorPairsQ[v_] := ListQ[v]&&AllTrue[v,MatchQ[#,{_,_}]&]&&!qqFFInvalidQ[v];
qqFFIdentityVector[master_,reduce_,seconds_,context_] := Module[{key,file,p,r,v},
 If[master===1,Return[{{1,1}}]];
 key=Hash[{context,master,reduce},"SHA256"];
 file=FileNameJoin[{cacheDirectory[],"qq-pv-vectors",IntegerString[key,16,64]<>".wxf"}];
 If[FileExistsQ[file],qqProgressOperation["Read PV coefficient-vector checkpoint"];
  p=Quiet[Check[Import[file,"WXF"],$Failed]];
  If[AssociationQ[p]&&Lookup[p,"Key",None]===key&&
    qqFFVectorPairsQ[Lookup[p,"Pairs",None]],
   qqProgressUpdate[<|"CacheState"->"Reused","ReusedCheckpoints"->(Lookup[$qqProgress,"ReusedCheckpoints",0]+1)|>];Return[p["Pairs"]]]];
 r=qqFFPVNormalize[master,reduce,seconds];
 If[qqFFInvalidQ[r],Return[Failure["QQPVVectorIncomplete",<|"Master"->master|>]]];
 qqProgressOperation["Collect this PV identity into scalar coefficients"];
 v=qqFFCollectMasters[r,False,seconds];If[FailureQ[v],Return[v]];
 If[!qqFFVectorPairsQ[v],Return[Failure["QQPVVectorIncomplete",<|"Master"->master|>]]];
 (* A timed-out normalization retains its input; it must remain retryable. *)
 If[KeyExistsQ[qqFFPVCache,{master,reduce}],
  If[!DirectoryQ[DirectoryName[file]],CreateDirectory[DirectoryName[file],CreateIntermediateDirectories->True]];
  qqProgressOperation["Save PV coefficient-vector checkpoint"];
  p=atomicWXF[file,<|"Key"->key,"Pairs"->v|>];If[FailureQ[p],Return[p]];
  qqProgressUpdate[<|"CacheState"->"Saved","ComputedCheckpoints"->(Lookup[$qqProgress,"ComputedCheckpoints",0]+1)|>]];v];
qqFFReduceMasterPairs[pairs_,reduce_,simplify_,seconds_,record_:False] := Module[{acc=<||>,identities={},context=qqFFVectorContext[],
 v,co,key,keys,rows={},candidate,stop=Unique["RGZWorkflow`Private`pvVectorStop"]},Catch[
 qqProgressUpdate[<|"PVIndex"->0,"PVTotal"->Length[pairs],"ScalarMasterIndex"->0,"ScalarMasterTotal"->0|>];
 Do[qqProgressOperation["Apply PV coefficient vector "<>ToString[i]<>" / "<>ToString[Length[pairs]]];
  qqProgressUpdate[<|"Master"->pairs[[i,1]],"PVIndex"->i,"PVTotal"->Length[pairs],"ItemStarted"->AbsoluteTime[],"InputLeaves"->LeafCount[pairs[[i,1]]]|>];
  v=qqFFIdentityVector[pairs[[i,1]],reduce,seconds,context];If[FailureQ[v],Throw[v,stop]];
  If[TrueQ[record],AppendTo[identities,{pairs[[i,1]],v}]];
  qqProgressUpdate[<|"OutputLeaves"->LeafCount[v]|>];
  co=pairs[[i,2]];
  Do[key=With[{m=entry[[1]]},HoldComplete[m]];
   AssociateTo[acc,key->Append[Lookup[acc,key,{}],co entry[[2]]]],{entry,v}],{i,Length[pairs]}];
 keys=Keys[acc];
 Do[qqProgressOperation["Merge scalar master coefficient "<>ToString[i]<>" / "<>ToString[Length[keys]]];
  key=keys[[i]];qqProgressUpdate[<|"ScalarMasterIndex"->i,"ScalarMasterTotal"->Length[keys],"Master"->ReleaseHold[key]|>];co=Total[acc[key]];
  If[TrueQ[simplify],candidate=TimeConstrained[qqFFRationalReduce[co],seconds,$TimedOut];
   If[!qqFFInvalidQ[candidate]&&LeafCount[candidate]<=LeafCount[co],co=candidate]];
  If[co=!=0,AppendTo[rows,{ReleaseHold[key],co}]],{i,Length[keys]}];If[TrueQ[record],<|"Pairs"->rows,"Identities"->identities|>,rows],stop]];

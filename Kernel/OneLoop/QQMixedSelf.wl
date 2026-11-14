(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Prepare the mixed-field self-energy contribution between the two quark currents.

   Inputs:
   The current QQ task and its mixed transverse matrix.

   Dependencies:
   Algebra/MixedProjection.wl attaches external legs; SpectralCoordinates.wl simplifies coefficients; Renormalization/QuarkMassLimit.wl and MassRestoration.wl handle the declared limit.
*)


(* A prepared insertion belongs to this matrix and this physical limit. *)
qqSelfProof[j_Association,value_] := Hash[{"QQMixedSelf/1",
 Lookup[j,{"Fingerprint","MixedSourceHash","Gauge","RegulatorMass"}],
 Lookup[j,"MixedTransverse",Missing["Absent"]],qqCalculationContext[],value},"SHA256"];
qqPreparedSelfQ[j_Association] := TrueQ[Lookup[j,"SelfPrepared",False]]&&
 KeyExistsQ[j,"MixedSelfEnergy"]&&!qqFFInvalidQ[j["MixedSelfEnergy"]]&&
 Hash[Lookup[j,"MixedTransverse",Missing["Absent"]],"SHA256"]===Lookup[j,"MixedSourceHash",None]&&
 Lookup[j,"MixedSelfProof",None]===qqSelfProof[j,j["MixedSelfEnergy"]];

Options[RGZQQPrepareSelf]={"TotalSeconds"->3600};
RGZQQPrepareSelf[seconds_:600,OptionsPattern[]] := engineEvaluation@Module[
 {job=RGZResult["QQ"],dir,key,work,batches={},pairs,group,terms={},assemblyRows,assemblyValues,scalar,gaugePairs,values={},answer,r,saved,
 stop=Unique["qqSelfStop"],require,total=OptionValue["TotalSeconds"]},
 If[!NumericQ[seconds]||!TrueQ[seconds>0]||!NumericQ[total]||!TrueQ[total>0],Return[Failure["TimeLimit",<||>]]];
 If[!AssociationQ[job],Return[Failure["QQStartFirst",<||>]]];
 If[Lookup[job,"Fingerprint",None]=!=qqJobFingerprint[job["Gauge"],job["RegulatorMass"]],Return[Failure["QQRestartRequired",<|"Action"->"RGZQQStart[]"|>]]];
 If[!KeyExistsQ[job,"MixedTransverse"]||Hash[job["MixedTransverse"],"SHA256"]=!=Lookup[job,"MixedSourceHash",None],Return[Failure["QQSelfSourceMismatch",<|"Action"->"RGZQQStart[]"|>]]];
 If[qqPreparedSelfQ[job],Return[<|"Status"->"Complete","Name"->"QQMixedSelf","Reused"->True|>]];
 r=loadQuarkEngine[];If[FailureQ[r],Return[r]];
 dir=FileNameJoin[{job["Directory"],"self"}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 key=Hash[{job["Fingerprint"],job["MixedSourceHash"]},"SHA256"];
 require[x_]:=If[FailureQ[x],Throw[x,stop],x];
 qqSelfProgress["Matrix",0,25];
 answer=qqSelfMonitor[CheckAbort[TimeConstrained[Catch[
 (* Only the transverse matrix is used; no duplicated longitudinal matrices. *)
 work=RGZWorkflow`Engine`RGZGaugeStart[<|"T"->job["MixedTransverse"],"L"->ConstantArray[0,{8,8}],
 "TFieldOrder"->RGZWorkflow`Engine`RGZMixBasis["T"],"LFieldOrder"->RGZWorkflow`Engine`RGZMixBasis["L"]|>];require[work];
 Do[qqSelfProgress["Matrix",i,25];
 r=require[qqSelfCheckpoint[dir,"matrix-"<>ToString[i],key,
 RGZWorkflow`Engine`RGZGaugePVPairs[RGZWorkflow`Engine`RGZLoopPoleMasses[
 RGZWorkflow`Engine`RGZLoopColour[-work["TreeTransverseAA"]^2 work["SelfEnergyTParts"][[i]]]]],seconds]];
 AppendTo[batches,r],{i,25}];
 pairs=require[qqSelfCheckpoint[dir,"pairs",key,
 group=GatherBy[Flatten[batches,1],First];({#[[1,1]],Total[#[[All,2]]]}& /@ group),seconds]];
 If[TrueQ[job["RegulatorMass"]===0],
 Do[qqSelfProgress["M0Coefficient",i,Length[pairs]];
 AppendTo[terms,require[qqSelfCheckpoint[dir,"m0-"<>ToString[i],key,
 RGZWorkflow`Engine`rgzQQM0Term[pairs[[i]],seconds],seconds]]],{i,Length[pairs]}];
 Do[qqSelfProgress["M0Assemble",i,3];
 assemblyRows=require[qqSelfCheckpoint[dir,"m0-assemble-pairs-"<>ToString[i],key,
 RGZWorkflow`Engine`RGZGaugePVPairs[RGZResidual39Diagnostic`Private`publicMaster[Total[If[terms==={}, {},terms[[All,i]]]]]],seconds]];
 assemblyValues={};
 Do[qqSelfProgress["M0Assemble:"<>ToString[2 i-6],j,Length[assemblyRows]];
 r=require[qqSelfCheckpoint[dir,"m0-assemble-"<>ToString[i]<>"-"<>ToString[j],key,
 qqSimplifyCoefficient[assemblyRows[[j,2]]],seconds]];
 If[i<3&&r=!=0,Throw[Failure["QQSelfM0Divergent",<|"Power"->(2 i-6),"Master"->assemblyRows[[j,1]],"Coefficient"->r|>],stop]];
 AppendTo[assemblyValues,assemblyRows[[j,1]] r],{j,Length[assemblyRows]}];
 r=require[qqSelfCheckpoint[dir,"m0-sum-"<>ToString[i],key,Total[assemblyValues],seconds]];
 If[i===3,scalar=qqRestoreProgress[r]],{i,3}],
 scalar=Total[Times@@@pairs]];
 qqSelfProgress["GaugePairs",0,0];
 gaugePairs=require[qqSelfCheckpoint[dir,"gauge-pairs",key,RGZWorkflow`Engine`RGZGaugePVPairs[scalar],seconds]];
 Do[qqSelfProgress["GaugeCoefficient",i,Length[gaugePairs]];
 AppendTo[values,require[qqSelfCheckpoint[dir,"gauge-"<>ToString[i],key,
 RGZWorkflow`Engine`rgzQQGaugeValue[Times@@gaugePairs[[i]],job["Gauge"],seconds],seconds]]],{i,Length[gaugePairs]}];
 qqSelfProgress["Attach",1,1];
 r=require[qqSelfCheckpoint[dir,"connected",key,
 (-FeynCalc`SMP["g_s"]^2 RGZWorkflow`Engine`rgzQQBornCurrent[RGZWorkflow`Engine`qqMu,RGZWorkflow`Engine`qqNu]
 RGZWorkflow`Engine`RGZLoopPT[RGZWorkflow`Engine`p,RGZWorkflow`Engine`qqMu,RGZWorkflow`Engine`qqNu] Total[values]) /.
 {RGZWorkflow`Engine`p->RGZWorkflow`Engine`qqK3-RGZWorkflow`Engine`qqK1,RGZWorkflow`Engine`M->job["RegulatorMass"]},seconds]];
 AssociateTo[job,{"MixedSelfEnergy"->r,"SelfPrepared"->True,"MixedSelfProof"->qqSelfProof[job,r]}];
 saved=atomicWXF[FileNameJoin[{job["Directory"],"job.wxf"}],job];require[saved];invalidateQQDerived[];storeResult["QQ",job];
 <|"Status"->"Complete","Name"->"QQMixedSelf","File"->FileNameJoin[{dir,"connected.wxf"}]|>,stop],
 total,Failure["QQSelfTotalTimeout",<|"Seconds"->total|>]],Failure["QQSelfAborted",<||>]],
 Panel[Row[{Lookup[$qqProgress,"Stage",""],"  ",Lookup[$qqProgress,"Index",0]," / ",Lookup[$qqProgress,"Total",0]}]]];
 qqSelfProgress[Lookup[$qqProgress,"Stage",""],Lookup[$qqProgress,"Index",0],Lookup[$qqProgress,"Total",0],If[FailureQ[answer],"Stopped","Complete"]];
 If[FailureQ[answer],Failure[answer[[1]],Join[answer[[2]],<|"Progress"->$qqProgress,"CheckpointDirectory"->dir,"Action"->"Repeat RGZQQPrepareSelf with the desired limits; completed checkpoints are reused."|>]],answer]
];



(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Full conventional amplitudes: no pinch contribution is added twice. *)
qqJobFingerprint[gauge_,reg_]:=If[!FileExistsQ[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}]],Missing["VerticesRequired"],Hash[{"RGZQQ/3",FileHash[FileNameJoin[{$packageRoot,"Kernel","Scattering.wl"}],"SHA256"],$Version,modelKey[],navFormulaFingerprint[],FileHash[FileNameJoin[{backendDirectory[],"RGZ_Quark_Amplitude.wl"}],"SHA256"],gauge,reg},"SHA256"]];
Options[RGZQQStart]={"Gauge"->0,"RegulatorMass"->0};
RGZQQStart[seconds_:300,OptionsPattern[]]:=engineEvaluation@Module[{r,mixed,nav,dir,key,probe,gauge=OptionValue["Gauge"],reg=OptionValue["RegulatorMass"],parts,packet,sigma,cached,sourceHash},
 r=masslessQQReady[];If[FailureQ[r],Return[r]];
 r=loadPTEngine[];If[FailureQ[r],Return[r]];
 If[!FreeQ[{gauge,reg},_Failure|_Missing]||!FreeQ[gauge,RGZWorkflow`Engine`q],Return[Failure["QuarkAmplitudeParameters",<||>]]];
 r=engineGet["RGZ_Residual39_M0_Diagnostic.wl"];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
 sigma=RGZWorkflow`Engine`rgzQQSigmaV0[gauge,reg];If[FailureQ[sigma],Return[sigma]];
 mixed=RGZResult["Mixed"];
 If[!AssociationQ[mixed]||!KeyExistsQ[mixed,"T"]||Dimensions[mixed["T"]]=!={5,5}||
 Lookup[mixed,"T\:573a\:987a\:5e8f",None]=!=RGZWorkflow`Engine`RGZMixBasis["T"]||
 !FreeQ[mixed["T"],_Failure|_Missing|$Failed|$Aborted],Return[Failure["MixedMatricesRequired",<|"Action"->"RGZMixedResult[] or RGZRestore[]"|>]]];
 If[Lookup[mixed,"ModelFingerprint",None]=!=modelKey[]||Lookup[mixed,"ModelContent",None]=!=modelContent[],Return[Failure["MixedModelContentMismatch",<|"Action"->"Recompute the mixed matrices with the current model, including eta and both quark loops."|>]]];
 sourceHash=Hash[mixed["T"],"SHA256"];
 r=timed["QQNAVSource",RGZNAVSum[seconds,"QuarkMass"->0],seconds];If[FailureQ[r],Return[r]];nav=RGZResult["NAV"];
 key=qqJobFingerprint[gauge,reg];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_qq_"<>IntegerString[key,16]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 If[FileExistsQ[FileNameJoin[{dir,"job.wxf"}]],
 cached=Quiet[Check[Import[FileNameJoin[{dir,"job.wxf"}],"WXF"],$Failed]];
 If[AssociationQ[cached]&&Lookup[cached,"Fingerprint",None]===key&&Lookup[cached,"MixedSourceHash",None]===sourceHash,
 storeResult["QQ",cached];Return[<|"Status"->"IntegrandsReady","Name"->"FullQQAmplitude","Reused"->True,"SelfPrepared"->TrueQ[Lookup[cached,"SelfPrepared",False]]|>]]];
 probe=timed["FullProbeGraphs",RGZWorkflow`Engine`rgzQQProbeGraphs[dir,seconds],seconds];If[FailureQ[probe],Return[probe]];
 probe=MapAt[RGZWorkflow`Engine`RGZLoopNormalize,probe,{{Key["Tree"]},{Key["AVAndBoxes"]},{Key["QuarkSelfEnergy"]}}];
 probe=RGZWorkflow`Engine`rgzQQNormalizeColours[probe];
 parts=RGZWorkflow`Engine`rgzQQClassify[probe["AVAndBoxes"]];If[FailureQ[parts],Return[parts]];
 parts=Join[parts,<|"NAV1"->Missing["DeferredNAV",1],"NAV2"->Missing["DeferredNAV",2],"QuarkSelfEnergy"->probe["QuarkSelfEnergy"]|>];
 packet=<|"Schema"->"RGZQQ/3","ExternalOrderingSign"->-1,"Fingerprint"->key,"Directory"->dir,"ModelFingerprint"->modelKey[],"ModelContent"->modelContent[],"JacobianGhostIncluded"->True,"ClosedQuarkFlavors"->2,"Process"->"q qprime -> q qprime; two dynamical massless flavors", "Gauge"->gauge,"RegulatorMass"->reg,
 "Kinematics"-><|"k1"->RGZWorkflow`Engine`qqK1,"k2"->RGZWorkflow`Engine`qqK2,"k3"->RGZWorkflow`Engine`qqK3,"k4"->RGZWorkflow`Engine`qqK4,"s"->RGZWorkflow`Engine`qqS,"t"->RGZWorkflow`Engine`qqT|>,
 "Tree"->probe["Tree"],"Integrands"->parts,"MixedTransverse"->mixed["T"],"MixedSourceHash"->sourceHash,
 "NAVSource"-><|"TransverseIntegrand"->nav["TransverseIntegrand"]|>,"MixedSelfEnergy"->Missing["PrepareSelfFirst"],"SelfPrepared"->False,"Reduced"-><||>,"Status"->"IntegrandsReady",
 "LoopMeasureConversion"->1/(2 Pi)^4,"OnShellSigmaV"->sigma,"GaugeIndependenceVerified"->False,"Renormalization"->"MSbar coupling and on-shell quark residues; not yet evaluated"|>;
 packet=MapAt[(#/.{RGZWorkflow`Engine`\[Alpha]->gauge,RGZWorkflow`Engine`M->reg})&,packet,{{Key["Tree"]},{Key["Integrands"]}}];
 r=atomicWXF[FileNameJoin[{dir,"job.wxf"}],packet];If[FailureQ[r],Return[r]];
 storeResult["QQ",packet];<|"Status"->"IntegrandsReady","Name"->"FullQQAmplitude","Rows"->7,"File"->FileNameJoin[{dir,"job.wxf"}]|>
];
RGZQQRunPart[name_String,seconds_:300]:=engineEvaluation@Module[{job=RGZResult["QQ"],r,input,file,saved,cached,raw},
 If[!AssociationQ[job],Return[Failure["QQStartFirst",<||>]]];
 If[Lookup[job,"Fingerprint",None]=!=qqJobFingerprint[job["Gauge"],job["RegulatorMass"]],Return[Failure["QQRestartRequired",<|"Action"->"RGZQQStart[]"|>]]];
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
 If[!KeyExistsQ[job["Integrands"],name],Return[Failure["QQPart",<|"Names"->Keys[job["Integrands"]]|>]]];
 file=FileNameJoin[{job["Directory"],name<>"-reduced.wxf"}];
 If[FileExistsQ[file],cached=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!AssociationQ[cached]||Lookup[cached,"Fingerprint",None]=!=job["Fingerprint"]||Lookup[cached,"Part",None]=!=name,Return[Failure["QQCacheInvalid",<|"File"->file|>]]];r=cached["Expression"],
 raw=job["Integrands"][name];
 If[MatchQ[raw,Missing["DeferredNAV",_Integer]],
 raw=timed["QQAttach:"<>name,-RGZWorkflow`Engine`rgzQQNAV[job["NAVSource"],raw[[2]]],seconds];If[FailureQ[raw],Return[raw]];
 raw=raw/.{RGZWorkflow`Engine`\[Alpha]->job["Gauge"],RGZWorkflow`Engine`M->job["RegulatorMass"]}];
 input=timed["QQPoleSplit:"<>name,RGZWorkflow`Engine`rgzQQSplit[raw,job["Gauge"],job["RegulatorMass"]],seconds];If[FailureQ[input],Return[input]];
 r=timed["QQTID:"<>name,Check[RGZWorkflow`Engine`rgzQQReduce[input],Failure["QQTensorReductionMessages",<|"Part"->name|>]],seconds];If[FailureQ[r],Return[r]];
 If[!FreeQ[r,FeynCalc`TID|FeynCalc`FeynAmpDenominator|$Failed|$Aborted|$TimedOut],Return[Failure["QQReductionIncomplete",<|"Part"->name|>]]];
 r=RGZWorkflow`Engine`rgzQQRestorePoleMasses[r,job["Gauge"],job["RegulatorMass"]] job["LoopMeasureConversion"];saved=atomicWXF[file,<|"Fingerprint"->job["Fingerprint"],"Part"->name,"Expression"->r|>];If[FailureQ[saved],Return[saved]]];
 If[!FreeQ[r,_Failure|_Missing|FeynCalc`TID|FeynCalc`FeynAmpDenominator|$Failed|$Aborted|$TimedOut],Return[Failure["QQReductionIncomplete",<|"Part"->name|>]]];
 AssociateTo[job,"Reduced"->Append[job["Reduced"],name->r]];saved=atomicWXF[FileNameJoin[{job["Directory"],"job.wxf"}],job];If[FailureQ[saved],Return[saved]];storeResult["QQ",job];
 <|"Status"->"Complete","Name"->name,"File"->file|>
];

(* Full conventional diagrams replace, rather than supplement, pinch kernels.
   The final state explicitly distinguishes a bare amplitude from a proved S matrix. *)
RGZQQAmplitude[]:=engineEvaluation@Module[{job=RGZResult["QQ"],required,missing,loop,packet,saved},
 If[!AssociationQ[job],Return[Failure["QQStartFirst",<||>]]];
 If[Lookup[job,"Fingerprint",None]=!=qqJobFingerprint[job["Gauge"],job["RegulatorMass"]],Return[Failure["QQRestartRequired",<|"Action"->"RGZQQStart[]"|>]]];
 If[!TrueQ[Lookup[job,"SelfPrepared",False]],Return[Failure["QQPrepareSelfFirst",<|"Action"->"RGZQQPrepareSelf[600]"|>]]];
 required={"AV1","AV2","Box1","Box2","NAV1","NAV2"};
 missing=Select[required,!KeyExistsQ[job["Reduced"],#]&];
 If[missing=!={},Return[Failure["QQPartsRequired",<|"Parts"->missing,"Action"->"RGZQQRunPart[name,seconds]"|>]]];
 If[!FreeQ[job["OnShellSigmaV"],_Failure|_Missing],Return[Failure["QQOnShellResidueRequired",<||>]]];
 loop=job["MixedSelfEnergy"]+Total[Lookup[job["Reduced"],required]]+2 job["OnShellSigmaV"] job["Tree"];
 packet=<|"Status"->"BareAmplitudeAssembled","Process"->job["Process"],"Fingerprint"->job["Fingerprint"],
 "ModelFingerprint"->Lookup[job,"ModelFingerprint",None],"ModelContent"->Lookup[job,"ModelContent",None],"JacobianGhostIncluded"->Lookup[job,"JacobianGhostIncluded",False],"ClosedQuarkFlavors"->Lookup[job,"ClosedQuarkFlavors",0],
 "Gauge"->job["Gauge"],"RegulatorMass"->job["RegulatorMass"],"Tree"->job["Tree"],"OneLoop"->loop,
 "iAmplitude"->job["Tree"]+loop,"Amplitude"->(job["Tree"]+loop)/I,
 "ExternalResidues"->2 job["OnShellSigmaV"] job["Tree"],"PinchDoubleCounted"->False,
 "Renormalized"->False,"GaugeIndependenceVerified"->False,
 "OutstandingChecks"->{"Full amplitude Ward identities including vertex and box remainders","Local MSbar counterterm closure","UV and IR pole separation"}|>;
 saved=atomicWXF[FileNameJoin[{job["Directory"],"bare-amplitude.wxf"}],packet];If[FailureQ[saved],Return[saved]];
 storeResult["QQAmplitude",packet];
 KeyTake[packet,{"Status","Process","Renormalized","GaugeIndependenceVerified","OutstandingChecks"}]
];
RGZQQSMatrix[]:=Module[{amp=RGZResult["QQAmplitude"]},
 If[!AssociationQ[amp],Return[Failure["QQAmplitudeFirst",<|"Action"->"RGZQQAmplitude[]"|>]]];
 (* These checks are deliberately fail-closed. Residual39 alone is not this proof. *)
 If[!TrueQ[amp["Renormalized"]]||!TrueQ[amp["GaugeIndependenceVerified"]],
 Return[Failure["FullSMatrixNotVerified",<|"Renormalized"->amp["Renormalized"],
 "GaugeIndependenceVerified"->amp["GaugeIndependenceVerified"],"OutstandingChecks"->amp["OutstandingChecks"]|>]]];
 <|"Status"->"Verified","Amplitude"->amp["Amplitude"],
 "Convention"->"Sfi = deltafi + i (2 Pi)^4 delta4(Pf-Pi) Amplitude"|>
];

qqSelfProgress[stage_,index_,total_,state_:"Computing"] := (
 $qqProgress=<|"Stage"->stage,"Index"->index,"Total"->total,"Status"->state|>;
 storeResult["QQProgress",$qqProgress]);
SetAttributes[qqSelfCheckpoint,HoldAll];
qqSelfCheckpoint[dir_,name_,key_,expression_,seconds_] := Module[{file,r,saved},
 file=FileNameJoin[{dir,name<>".wxf"}];
 If[FileExistsQ[file],
 r=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!AssociationQ[r]||Lookup[r,"Key",None]=!=key||!KeyExistsQ[r,"Value"],Return[Failure["QQSelfCacheInvalid",<|"File"->file|>]]];
 If[!FreeQ[r["Value"],$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity|_Failure|_Missing],Return[Failure["QQSelfCacheInvalid",<|"File"->file|>]]];
 Return[r["Value"]]];
 r=timed["QQSelf:"<>name,expression,seconds];If[FailureQ[r],Return[r]];
 If[!FreeQ[r,$Failed|$Aborted|$TimedOut|Indeterminate|ComplexInfinity|_DirectedInfinity|_Failure|_Missing],Return[Failure["QQSelfCheckpointIncomplete",<|"Checkpoint"->name|>]]];
 saved=atomicWXF[file,<|"Key"->key,"Value"->r|>];If[FailureQ[saved],saved,r]
];
Options[RGZQQPrepareSelf]={"TotalSeconds"->3600};
RGZQQPrepareSelf[seconds_:600,OptionsPattern[]] := engineEvaluation@Module[
 {job=RGZResult["QQ"],dir,key,work,batches={},pairs,group,terms={},assemblyRows,assemblyValues,scalar,gaugePairs,values={},answer,r,saved,
 stop=Unique["qqSelfStop"],require,total=OptionValue["TotalSeconds"]},
 If[!NumericQ[seconds]||!TrueQ[seconds>0]||!NumericQ[total]||!TrueQ[total>0],Return[Failure["TimeLimit",<||>]]];
 If[!AssociationQ[job],Return[Failure["QQStartFirst",<||>]]];
 If[Lookup[job,"Fingerprint",None]=!=qqJobFingerprint[job["Gauge"],job["RegulatorMass"]],Return[Failure["QQRestartRequired",<|"Action"->"RGZQQStart[]"|>]]];
 If[!KeyExistsQ[job,"MixedTransverse"]||Hash[job["MixedTransverse"],"SHA256"]=!=Lookup[job,"MixedSourceHash",None],Return[Failure["QQSelfSourceMismatch",<|"Action"->"RGZQQStart[]"|>]]];
 If[TrueQ[Lookup[job,"SelfPrepared",False]],Return[<|"Status"->"Complete","Name"->"QQMixedSelf","Reused"->True|>]];
 r=loadPTEngine[];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_Residual39_M0_Diagnostic.wl"];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_Quark_Amplitude.wl",False];If[FailureQ[r],Return[r]];
 dir=FileNameJoin[{job["Directory"],"self"}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 key=Hash[{job["Fingerprint"],job["MixedSourceHash"]},"SHA256"];
 require[x_]:=If[FailureQ[x],Throw[x,stop],x];
 qqSelfProgress["Matrix",0,25];
 answer=qqSelfMonitor[CheckAbort[TimeConstrained[Catch[
 (* Only the transverse matrix is used; no duplicated longitudinal matrices. *)
 work=RGZWorkflow`Engine`RGZGaugeStart[<|"T"->job["MixedTransverse"],"L"->ConstantArray[0,{8,8}],
 "T\:573a\:987a\:5e8f"->RGZWorkflow`Engine`RGZMixBasis["T"],"L\:573a\:987a\:5e8f"->RGZWorkflow`Engine`RGZMixBasis["L"]|>];require[work];
 Do[qqSelfProgress["Matrix",i,25];
 r=require[qqSelfCheckpoint[dir,"matrix-"<>ToString[i],key,
 RGZWorkflow`Engine`RGZGaugePVPairs[RGZWorkflow`Engine`RGZLoopPoleMasses[
 RGZWorkflow`Engine`RGZPTColour[-work["\:6811\:7ea7\:6a2a\:5411AA"]^2 work["\:81ea\:80fdT\:5206\:9879"][[i]]]]],seconds]];
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
 Factor[Cancel[Together[assemblyRows[[j,2]]]]],seconds]];
 If[i<3&&r=!=0,Throw[Failure["QQSelfM0Divergent",<|"Power"->(2 i-6),"Master"->assemblyRows[[j,1]],"Coefficient"->r|>],stop]];
 AppendTo[assemblyValues,assemblyRows[[j,1]] r],{j,Length[assemblyRows]}];
 r=require[qqSelfCheckpoint[dir,"m0-sum-"<>ToString[i],key,Total[assemblyValues],seconds]];
 If[i===3,scalar=RGZWorkflow`Engine`rgzQQM0Restore[r]],{i,3}],
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
 AssociateTo[job,{"MixedSelfEnergy"->r,"SelfPrepared"->True}];
 saved=atomicWXF[FileNameJoin[{job["Directory"],"job.wxf"}],job];require[saved];storeResult["QQ",job];
 <|"Status"->"Complete","Name"->"QQMixedSelf","File"->FileNameJoin[{dir,"connected.wxf"}]|>,stop],
 total,Failure["QQSelfTotalTimeout",<|"Seconds"->total|>]],Failure["QQSelfAborted",<||>]],
 Panel[Row[{Lookup[$qqProgress,"Stage",""],"  ",Lookup[$qqProgress,"Index",0]," / ",Lookup[$qqProgress,"Total",0]}]]];
 qqSelfProgress[Lookup[$qqProgress,"Stage",""],Lookup[$qqProgress,"Index",0],Lookup[$qqProgress,"Total",0],If[FailureQ[answer],"Stopped","Complete"]];
 If[FailureQ[answer],Failure[answer[[1]],Join[answer[[2]],<|"Progress"->$qqProgress,"CheckpointDirectory"->dir,"Action"->"Repeat RGZQQPrepareSelf with the desired limits; completed checkpoints are reused."|>]],answer]
];

SetAttributes[qqSelfMonitor,HoldAll];
qqSelfMonitor[expression_,display_]:=If[TrueQ[$Notebooks],Monitor[expression,display],expression];

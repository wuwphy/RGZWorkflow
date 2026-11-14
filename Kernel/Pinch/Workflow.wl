(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Expose candidate PT initialization, staged evaluation and evidence through separate public commands.

   Inputs:
   An explicitly requested PT mode, mixed matrix and required QCD/NAV data.

   Dependencies:
   OneLoop/MixedMatrices.wl provides the common loader; Kernels.wl, Continuation.wl and VertexWard.wl are loaded only by candidate commands.
*)


(* Completed physical fields are validated separately from stopped diagnostics. *)
(* Pure packet provenance also works before the FeynCalc backend is loaded,
   so session restore cannot accept a disconnected chain of stage values. *)
ptPartSource[p_] := Hash[{p["OriginalExpression"],Lookup[p,"Representation","FAD"]},"SHA256"];
ptStageProvenance[p_,stage_,environment_] := Module[{order,n,input},
 order={"Project","Decompose","Integrate","Scalar"};
 n=FirstPosition[order,stage,Missing[]];If[MissingQ[n],Return[Missing["PTStage"]]];n=First[n];
 If[!KeyExistsQ[p["Result"],stage],Return[Missing["PTStageResult"]]];
 input=If[n===1,p["OriginalExpression"],Lookup[p["Result"],order[[n-1]],Missing["PTStageInput"]]];
 If[MissingQ[input],Return[input]];
 <|"Schema"->"RGZPTStage/1","Environment"->environment,"Source"->ptPartSource[p],
   "Input"->Hash[input,"SHA256"],"Result"->Hash[p["Result"][stage],"SHA256"]|>];
ptPartPacketQ[p_] := AssociationQ[p]&&KeyExistsQ[p,"OriginalExpression"]&&
 !qqFFInvalidQ[p["OriginalExpression"]]&&AssociationQ[Lookup[p,"Result",None]]&&
 AllTrue[Keys[p["Result"]],MemberQ[{"Project","Decompose","Integrate","Scalar"},#]&]&&
 !qqFFInvalidQ[Values[p["Result"]]];
ptPartProvenanceQ[p_,environment_] := ptPartPacketQ[p]&&AssociationQ[Lookup[p,"StageProvenance",None]]&&
 Sort[Keys[p["StageProvenance"]]]===Sort[Keys[p["Result"]]]&&
 AllTrue[Keys[p["Result"]],With[{proof=ptStageProvenance[p,#,environment]},
   AssociationQ[proof]&&Lookup[p["StageProvenance"],#,None]===proof]&];
ptComparisonIdentity[j_] := Hash[{"RGZPTComparison/1",j["Environment"],j["Mode"],
 j["NAVMode"],j["NAVFingerprint"],j["SelfEnergyParts"],
 ({#,ptPartSource[j["Parts"][#]],Lookup[j["Parts"][#]["Result"],"Scalar",Missing[]],
    Lookup[Lookup[j["Parts"][#],"StageProvenance",<||>],"Scalar",Missing[]]}&/@j["PartsOrder"])},"SHA256"];
ptJobPacketQ[j_] := AssociationQ[j]&&StringQ[Lookup[j,"Directory",None]]&&StringLength[j["Directory"]]>0&&
 IntegerQ[Lookup[j,"Environment",None]]&&j["Environment"]>=0&&IntegerQ[Lookup[j,"KernelSignature",None]]&&
 Lookup[j,"PartsOrder",None]==={"box","navAll","avAll","qseAll"}&&AssociationQ[Lookup[j,"Parts",None]]&&
 Sort[Keys[j["Parts"]]]===Sort[j["PartsOrder"]]&&AllTrue[Values[j["Parts"]],ptPartPacketQ]&&
 AllTrue[Values[j["Parts"]],ptPartProvenanceQ[#,j["Environment"]]&]&&
 ListQ[Lookup[j,"SelfEnergyParts",None]]&&Length[j["SelfEnergyParts"]]===25&&!qqFFInvalidQ[j["SelfEnergyParts"]]&&
 MemberQ[{"FourPinch","GaugeDifference"},Lookup[j,"Mode",None]]&&KeyExistsQ[j,"ReferenceGauge"]&&
 !qqFFInvalidQ[j["ReferenceGauge"]]&&MemberQ[{"FullMatrix","AAReplacement"},Lookup[j,"NAVMode",None]]&&
 If[j["NAVMode"]==="FullMatrix",j["Mode"]==="FourPinch"&&IntegerQ[Lookup[j,"NAVFingerprint",None]]&&
   qqPVValidNAVProof[Lookup[j,"NAVPacket",None],j["NAVFingerprint"]]&&
   j["Parts"]["navAll"]["OriginalExpression"]===j["NAVPacket"]["Kernel"]&&
   Lookup[j["Parts"]["navAll"],"Representation",None]==="Rational",Lookup[j,"NAVFingerprint",None]===None]&&
 AssociationQ[Lookup[j,"QCDConvention",None]]&&Lookup[j["QCDConvention"],"Status",None]==="Passed"&&
 Lookup[j["QCDConvention"],"SharedFactor",None]===1&&KeyExistsQ[j,"Compare"];
loadPTEngine[] := engineEvaluation@Module[{r=loadSharedLoopTools[],name},If[FailureQ[r],Return[r]];
 r=Fold[If[FailureQ[#1],#1,engineGet[#2]]&,Null,{"Pinch/Kernels.wl","Internal/GaugeRuntime.wl","Algebra/GaugeCoefficients.wl","Algebra/CommonDenominator.wl","Pinch/Continuation.wl","Pinch/VertexWard.wl"}];r];
Options[RGZPinchKernels]={"Mode"->"FourPinch","ReferenceGauge"->0,"Representation"->"Rational"};
RGZPinchKernels[OptionsPattern[]] := engineEvaluation@Module[{r=ready[]},If[FailureQ[r],Return[r]];
 r=loadLoopEngine[];If[FailureQ[r],Return[r]];r=engineGet["Pinch/Kernels.wl"];If[FailureQ[r],Return[r]];
 RGZWorkflow`Engine`RGZBuildPinches["Mode"->OptionValue["Mode"],"ReferenceGauge"->OptionValue["ReferenceGauge"],"Representation"->OptionValue["Representation"]]
];
Options[RGZPTStart]={"Mode"->"FourPinch","ReferenceGauge"->0,"NAVMode"->"FullMatrix","NAVSeconds"->300};
RGZPTStart[OptionsPattern[]] := engineEvaluation@Module[{r=loadPTEngine[],mixed,qcd,nav=None},If[FailureQ[r],Return[r]];
 mixed=RGZResult["Mixed"];If[!validMixed[mixed],Return[Failure["MixedMatricesRequired",<|"Action"->"RGZMixedResult[] or RGZRestore[]"|>]]];
 qcd=RGZResult["QCD"];If[!AssociationQ[qcd],Return[Failure["QCDBenchmarkRequired",<|"Action"->"RGZQCDBenchmark[]"|>]]];
 If[!MemberQ[{"FullMatrix","AAReplacement"},OptionValue["NAVMode"]],Return[Failure["NAVMode",<||>]]];
 If[OptionValue["NAVMode"]==="FullMatrix",nav=prepareNAVPinch[OptionValue["NAVSeconds"]];If[FailureQ[nav],Return[nav]]];
 r=RGZWorkflow`Engine`RGZPTLegacyStart[mixed,"CacheDirectory"->cacheDirectory[],"QCDReport"->qcd,
 "Mode"->OptionValue["Mode"],"ReferenceGauge"->OptionValue["ReferenceGauge"],"NAVPacket"->nav];
 If[!FailureQ[r],$results=KeyDrop[$results,Select[Keys[$results],MemberQ[{"Gauge","GaugePartial","GaugeStopped","Pinch"},#]||StringStartsQ[#,"MassLimit:"]&]];$stageStatus=KeyDrop[$stageStatus,{"Gauge","Pinch"}]];r
];
RGZPinchResult[] := engineEvaluation@Module[{r=loadPTEngine[]},If[FailureQ[r],Return[r]];
 r=RGZWorkflow`Engine`RGZPTResult[];If[!FailureQ[r],storeResult["Pinch",r]];r
];
candidateName[name_String] := Lookup[<|"nav"->"navAll","av"->"avAll","qse"->"qseAll"|>,name,name];
RGZRunPart[name_String,stage_String,seconds_:300] := engineEvaluation@Module[{r,part=candidateName[name]},
 If[!MemberQ[{"box","navAll","avAll","qseAll"},part]||!MemberQ[{"Project","Decompose","Integrate","Scalar"},stage],Return[Failure["CandidateStage",<|"Explanation"->"Names: box, nav/navAll, av/avAll, qse/qseAll; stages: Project, Decompose, Integrate, Scalar."|>]]];
 r=loadPTEngine[];If[FailureQ[r],Return[r]];
 RGZWorkflow`Engine`RGZPTRun[part,stage,seconds]
];
Options[RGZGaugeCheck]={"ReferenceGauge"->1,"Coefficients"->{1},"PreparationSeconds"->600,"TotalSeconds"->1200,"NAVMode"->"FullMatrix"};
RGZGaugeCheck[seconds_:60,OptionsPattern[]] := engineEvaluation@Module[{r=loadPTEngine[]},If[FailureQ[r],Return[r]];
 If[OptionValue["NAVMode"]==="FullMatrix" && AssociationQ[RGZWorkflow`Engine`rgzPTJob] && !fullNAVIncluded[],Return[Failure["FullNAVRestartRequired",<|"Action"->"RGZPTStart[]; rerun navAll: Project, Decompose, Integrate, Scalar","Explanation"->"The restored job contains the old AA-only NAV."|>]]];
 (* Reset attempt diagnostics before preprocessing can fail. The last successful
    Gauge report remains available separately from this attempt. *)
 $results=KeyDrop[$results,{"GaugeStopped","GaugePartial"}];
 RGZWorkflow`Engine`rgzGaugeCompletedRows={};
 r=RGZWorkflow`Engine`RGZPTFixedCompare[OptionValue["ReferenceGauge"],seconds,
 "CoefficientID"->OptionValue["Coefficients"],"CacheDirectory"->cacheDirectory[],
 "PreprocessTimeLimit"->OptionValue["PreparationSeconds"],"PrepareTimeLimit"->OptionValue["PreparationSeconds"],"TotalTimeLimit"->OptionValue["TotalSeconds"]];
 If[FailureQ[r],
  storeResult["GaugeStopped",r[[2]]];
  If[ListQ[RGZWorkflow`Engine`rgzGaugeCompletedRows],storeResult["GaugePartial",<|"ReferenceGauge"->OptionValue["ReferenceGauge"],"Rows"->RGZWorkflow`Engine`rgzGaugeCompletedRows,"CalculationScope"->gaugeScope[],"FullNAVIncluded"->fullNAVIncluded[]|>]];
  Return[r]];
 r=Join[r,<|"CalculationScope"->gaugeScope[],"FullNAVIncluded"->fullNAVIncluded[]|>];storeResult["Gauge",r];
 <|"Status"->Lookup[r,"Status","Unknown"],"Rows"->Lookup[r,"CheckedCount",0],"Statistics"->Lookup[r,"Statistics",<||>],"CalculationScope"->gaugeScope[],"FullNAVIncluded"->fullNAVIncluded[]|>
];
RGZExportResidual[] := engineEvaluation@Module[{r=loadPTEngine[],file,report,rows},If[FailureQ[r],Return[r]];report=RGZResult["Gauge"];
 If[!AssociationQ[report],Return[Failure["GaugeReportRequired",<||>]]];rows=Lookup[report,"ResidualParts",{}];
 If[!residual39RowsQ[rows],Return[Failure["Complete39RowsRequired",<||>]]];
 If[!NumericQ[Lookup[report,"ReferenceGauge",None]]||!TrueQ[report["ReferenceGauge"]>0]||qqFFInvalidQ[report["ReferenceGauge"]],Return[Failure["InvalidResidualReport",<||>]]];
 file=FileNameJoin[{dataDirectory[],"RGZ_Gauge_Residual_39.wxf"}];
 r=atomicWXF[file,<|"Schema"->"RGZResidual39/1","WorkflowFingerprint"->workflowFingerprint[],"ModelFingerprint"->modelKey[],"PinchFormulaFingerprint"->pinchFormulaFingerprint[],"NAVMode"->If[TrueQ[Lookup[report,"FullNAVIncluded",False]],"FullMatrix","AAReplacement"],"NAVFingerprint"->If[TrueQ[Lookup[report,"FullNAVIncluded",False]],navFormulaFingerprint[],None],"ReferenceGauge"->report["ReferenceGauge"],"ResidualParts"->rows|>];r
];
RGZStatus["PT"] := engineEvaluation@Module[{r=loadPTEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZPTStatus[]]];

navMatrixFields={"A","b","tau","xi","phi","phibar","beta","betabar"};
navMatrixVectors={"A","phi","phibar","beta","betabar"};
navPinchInputIdentity[c_] := Hash[{"NAVWardInput/3",workflowFingerprint[],c["Fields"],c["TransverseTensor"]},"SHA256"];
navPinchConnectionQ[r_,n_,fields_,source_:Automatic] := AssociationQ[r]&&Lookup[r,"Connection",None]===n&&
 MatchQ[Lookup[r,"Fields",None],{_String,_String,_String}]&&AllTrue[r["Fields"],MemberQ[navMatrixFields,#]&]&&r["Fields"]===fields&&
 Lookup[r,"ValidationPolicy",None]==="NAVWard/3"&&IntegerQ[Lookup[r,"InputIdentity",None]]&&
 (source===Automatic || r["InputIdentity"]===source)&&
 KeyExistsQ[r,"Kernel"]&&Lookup[r,"KernelHash",None]===Hash[r["Kernel"],"SHA256"]&&
 TrueQ[Lookup[r,"ExactPolynomialReconstruction",False]]&&TrueQ[Lookup[r,"OriginalTensorReconstruction",False]]&&!qqFFInvalidQ[r];
navPinchMatchesQ[r_,nav_] := navPacketQ[r,"RGZNAVPinch/1",{"Kernel"}] &&
 r["ConnectionCount"]===nav["ConnectionCount"] &&
 And@@MapIndexed[navPinchConnectionQ[#1,First[#2],nav["Connections"][[First[#2]]]["Fields"],
   navPinchInputIdentity[nav["Connections"][[First[#2]]]]]& ,r["Connections"]];
fullNAVIncluded[]:=AssociationQ[RGZWorkflow`Engine`rgzPTJob]&&Lookup[RGZWorkflow`Engine`rgzPTJob,"NAVMode",None]==="FullMatrix";
gaugeScope[]:=If[fullNAVIncluded[],"FullMatrixNAVPinch","AAReplacementCandidate"];
navFormulaFingerprint[]:=Hash[{workflowFingerprint[],modelKey[],FileHash[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}],"SHA256"],FileHash[FileNameJoin[{calculationDirectory[],"OneLoop/NonAbelianVertex.wl"}],"SHA256"],FileHash[FileNameJoin[{calculationDirectory[],"Pinch/VertexWard.wl"}],"SHA256"]},"SHA256"];
prepareNAVPinch[seconds_]:=Module[{r,key,dir},
 r=RGZNAVSum[seconds];If[FailureQ[r],Return[r]];
 r=engineGet["Pinch/VertexWard.wl"];If[FailureQ[r],Return[r]];
 key=navFormulaFingerprint[];dir=FileNameJoin[{cacheDirectory[],"RGZ_NAV_pinch_"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=If[FileExistsQ[FileNameJoin[{dir,"result.wxf"}]],Quiet[Check[Import[FileNameJoin[{dir,"result.wxf"}],"WXF"],$Failed]],Missing["NoCache"]];
 If[!navPinchMatchesQ[r,RGZResult["NAV"]],r=RGZWorkflow`Engine`rgzNAVPinchBuild[RGZResult["NAV"],dir,seconds]];
 If[FailureQ[r],Return[r]];If[!navPinchMatchesQ[r,RGZResult["NAV"]],Return[Failure["NAVPinchCacheInvalid",<|"Directory"->dir|>]]];r=Join[r,<|"Fingerprint"->key|>];storeResult["NAVPinch",r];r
];


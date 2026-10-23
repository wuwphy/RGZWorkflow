(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Explicit adapter to the compatible RGZWorkflow`Engine` computational engine. *)
loadLoopEngine[] := engineEvaluation@Module[{r},r=checksReady[];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_Mixed_OneLoop_Overall.wl"];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_Mixed_Scalar_Accelerator.wl"];If[FailureQ[r],Return[r]];
 RGZWorkflow`Engine`RGZMixSetAcceleration["\:5355\:6838\:65f6\:9650"->120,"\:9884\:5904\:7406\:65f6\:9650"->15];
 RGZWorkflow`Engine`RGZMixSetDisplay[Switch[$config["Display"],"Quiet","\:9759\:9ed8","Verbose","\:8be6\:7ec6",_,"\:7b80\:6d01"]];Null
];
RGZMixedStart[] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZMixStart[]]];
Options[RGZMixedStartPair]={"Sectors"->All};
RGZMixedStartPair[a_,b_,seconds_:60,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,
 RGZWorkflow`Engine`RGZMixStartPair[a,b,"\:7c7b\:522b"->OptionValue["Sectors"],"\:5355\:6b65\:65f6\:9650"->seconds,"\:590d\:7528AA\:7f13\:5b58"->False]]];
RGZRunGraph[n_Integer,stage_String,seconds_:120] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 If[!MemberQ[{"\:8f6c\:6362","\:6295\:5f71","\:5206\:89e3T","\:79ef\:5206T","\:6807\:91cfT","\:5206\:89e3L","\:79ef\:5206L","\:6807\:91cfL"},stage],Return[Failure["GraphStage",<||>]]];
 RGZWorkflow`Engine`RGZMXRun[n,stage,seconds]
];
Options[RGZMixedComputePair]={"Sectors"->All};
RGZMixedComputePair[a_,b_,seconds_:300,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,
 RGZWorkflow`Engine`RGZMixComputePair[a,b,seconds,"\:7c7b\:522b"->OptionValue["Sectors"],"\:590d\:7528AA\:7f13\:5b58"->False]]];
Options[RGZMixedComputeAll]={"Sectors"->All,"Pairs"->All};
RGZMixedComputeAll[seconds_:300,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,
 RGZWorkflow`Engine`RGZMixComputeAll[seconds,"\:573a\:5bf9"->OptionValue["Pairs"],"\:7c7b\:522b"->OptionValue["Sectors"],"\:590d\:7528AA\:7f13\:5b58"->False]]];
RGZMixedResult[seconds_:120] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 r=RGZWorkflow`Engine`RGZMixResult[seconds];If[FailureQ[r],Return[r]];
 If[!validMixed[r],Return[Failure["InvalidMixedMatrices",<||>]]];
 RGZWorkflow`Engine`rgzMixed=r;storeResult["Mixed",r];<|"Status"->"Complete","Name"->"Mixed","Dimensions"->{{5,5},{8,8},{8,8}}|>
];
RGZQCDBenchmark[seconds_:600] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 r=timed["QCDBenchmark",engineGet["RGZ_QCD_Check_Clean.wl",False],seconds];If[FailureQ[r],Return[r]];
 r=RGZWorkflow`Engine`rgzQCDReport;If[FailureQ[r],Return[r]];If[!AssociationQ[r],Return[Failure["InvalidQCDReport",<||>]]];
 storeResult["QCD",r];<|"Status"->"Complete","Name"->"QCD","Statistics"->Lookup[r,"\:603b\:548c\:68c0\:67e5",Missing["NotChecked"]]|>
];
loadPTEngine[] := engineEvaluation@Module[{r=loadLoopEngine[],name},If[FailureQ[r],Return[r]];
 r=Fold[If[FailureQ[#1],#1,engineGet[#2]]&,Null,{"RGZ_Mixed_Gauge_Check_v2.wl","RGZ_Mixed_PT_Continuation.wl","RGZ_Gauge_Fixed_Denominator.wl","RGZ_NAV_Pinch.wl"}];r];
Options[RGZPinchKernels]={"Mode"->"FourPinch","ReferenceGauge"->0,"Representation"->"Rational"};
RGZPinchKernels[OptionsPattern[]] := engineEvaluation@Module[{r=ready[]},If[FailureQ[r],Return[r]];
 r=fcReady[];If[FailureQ[r],Return[r]];r=engineGet["RGZ_OneLoop_AA_PT_Check.wl"];If[FailureQ[r],Return[r]];
 RGZWorkflow`Engine`RGZBuildPinches["Mode"->OptionValue["Mode"],"ReferenceGauge"->OptionValue["ReferenceGauge"],"Representation"->OptionValue["Representation"]]
];
Options[RGZPTStart]={"Mode"->"FourPinch","ReferenceGauge"->0,"NAVMode"->"FullMatrix","NAVSeconds"->300};
RGZPTStart[OptionsPattern[]] := engineEvaluation@Module[{r=loadPTEngine[],mixed,qcd,nav=None},If[FailureQ[r],Return[r]];
 mixed=RGZResult["Mixed"];If[!validMixed[mixed],Return[Failure["MixedMatricesRequired",<|"Action"->"RGZMixedResult[] or RGZRestore[]"|>]]];
 qcd=RGZResult["QCD"];If[!AssociationQ[qcd],Return[Failure["QCDBenchmarkRequired",<|"Action"->"RGZQCDBenchmark[]"|>]]];
 If[!MemberQ[{"FullMatrix","AAReplacement"},OptionValue["NAVMode"]],Return[Failure["NAVMode",<||>]]];
 If[OptionValue["NAVMode"]==="FullMatrix",nav=prepareNAVPinch[OptionValue["NAVSeconds"]];If[FailureQ[nav],Return[nav]]];
 r=RGZWorkflow`Engine`RGZPTLegacyStart[mixed,"\:7f13\:5b58\:76ee\:5f55"->cacheDirectory[],"QCD\:62a5\:544a"->qcd,
 "Mode"->OptionValue["Mode"],"ReferenceGauge"->OptionValue["ReferenceGauge"],"NAVPacket"->nav];
 If[!FailureQ[r],$results=KeyDrop[$results,Select[Keys[$results],MemberQ[{"Gauge","GaugePartial","GaugeStopped","Pinch"},#]||StringStartsQ[#,"MassLimit:"]&]];$stageStatus=KeyDrop[$stageStatus,{"Gauge","Pinch"}]];r
];
RGZPinchResult[] := engineEvaluation@Module[{r=loadPTEngine[]},If[FailureQ[r],Return[r]];
 r=RGZWorkflow`Engine`RGZPTResult[];If[!FailureQ[r],storeResult["Pinch",r]];r
];
candidateName[name_String] := Lookup[<|"nav"->"nav\:5168\:90e8","av"->"av\:5168\:90e8","qse"->"qse\:5168\:90e8"|>,name,name];
RGZRunPart[name_String,stage_String,seconds_:300] := engineEvaluation@Module[{r,part=candidateName[name]},
 If[!MemberQ[{"box","nav\:5168\:90e8","av\:5168\:90e8","qse\:5168\:90e8"},part]||!MemberQ[{"\:6295\:5f71","\:5206\:89e3","\:79ef\:5206","\:6807\:91cf"},stage],Return[Failure["CandidateStage",<|"Explanation"->"Names: box, nav/nav全部, av/av全部, qse/qse全部; stages: 投影, 分解, 积分, 标量."|>]]];
 r=loadPTEngine[];If[FailureQ[r],Return[r]];
 RGZWorkflow`Engine`RGZPTRun[part,stage,seconds]
];
Options[RGZGaugeCheck]={"ReferenceGauge"->1,"Coefficients"->{1},"PreparationSeconds"->600,"TotalSeconds"->1200,"NAVMode"->"FullMatrix"};
RGZGaugeCheck[seconds_:60,OptionsPattern[]] := engineEvaluation@Module[{r=loadPTEngine[]},If[FailureQ[r],Return[r]];
 If[OptionValue["NAVMode"]==="FullMatrix" && AssociationQ[RGZWorkflow`Engine`rgzPTJob] && !fullNAVIncluded[],Return[Failure["FullNAVRestartRequired",<|"Action"->"RGZPTStart[]; rerun nav全部: 投影, 分解, 积分, 标量","Explanation"->"The restored job contains the old AA-only NAV."|>]]];
 r=RGZWorkflow`Engine`RGZPTFixedCompare[OptionValue["ReferenceGauge"],seconds,
 "\:7cfb\:6570\:7f16\:53f7"->OptionValue["Coefficients"],"\:5df2\:9a8c\:8bc1\:8f93\:5165"->None,"\:7f13\:5b58\:76ee\:5f55"->cacheDirectory[],
 "\:9884\:5904\:7406\:65f6\:9650"->OptionValue["PreparationSeconds"],"\:51c6\:5907\:65f6\:9650"->OptionValue["PreparationSeconds"],"\:603b\:65f6\:9650"->OptionValue["TotalSeconds"]];
 If[FailureQ[r],
  storeResult["GaugeStopped",r[[2]]];
  If[ListQ[RGZWorkflow`Engine`rgzGaugeCompletedRows],storeResult["GaugePartial",<|"ReferenceGauge"->OptionValue["ReferenceGauge"],"Rows"->RGZWorkflow`Engine`rgzGaugeCompletedRows,"CalculationScope"->gaugeScope[],"FullNAVIncluded"->fullNAVIncluded[]|>]];
  Return[r]];
 r=Join[r,<|"CalculationScope"->gaugeScope[],"FullNAVIncluded"->fullNAVIncluded[]|>];storeResult["Gauge",r];
 <|"Status"->Lookup[r,"\:72b6\:6001","Unknown"],"Rows"->Lookup[r,"\:672c\:6b21\:68c0\:67e5\:9879\:6570",0],"Statistics"->Lookup[r,"\:7edf\:8ba1",<||>],"CalculationScope"->gaugeScope[],"FullNAVIncluded"->fullNAVIncluded[]|>
];
RGZExportResidual[] := engineEvaluation@Module[{r=loadPTEngine[],file,report,rows},If[FailureQ[r],Return[r]];report=RGZResult["Gauge"];
 If[!AssociationQ[report],Return[Failure["GaugeReportRequired",<||>]]];rows=Lookup[report,"\:6b8b\:5dee\:5206\:9879",{}];
 If[Length[rows]=!=39||Length[DeleteDuplicates[Lookup[rows,"\:7f16\:53f7"]]]=!=39,Return[Failure["Complete39RowsRequired",<||>]]];
 file=FileNameJoin[{dataDirectory[],"RGZ_Gauge_Residual_39.wxf"}];
 r=atomicWXF[file,<|"Schema"->"RGZResidual39/1","ModelFingerprint"->modelKey[],"PinchFormulaFingerprint"->pinchFormulaFingerprint[],"NAVMode"->If[TrueQ[Lookup[report,"FullNAVIncluded",False]],"FullMatrix","AAReplacement"],"NAVFingerprint"->If[TrueQ[Lookup[report,"FullNAVIncluded",False]],navFormulaFingerprint[],None],"\:53c2\:8003\:89c4\:8303"->report["\:53c2\:8003\:89c4\:8303"],"\:6b8b\:5dee\:5206\:9879"->rows|>];r
];
Options[RGZMassLimit]={"ParameterPoint"->Automatic};
RGZMassLimit[mode_String,seconds_:60,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[],file,packet},If[FailureQ[r],Return[r]];
 If[mode==="QCD" && OptionValue["ParameterPoint"]=!=Automatic,Return[Failure["ParameterPointM0Only",<|"Explanation"->"ParameterPoint applies only to M0; QCD uses its specified common zero-mass path."|>]]];
 file=FileNameJoin[{dataDirectory[],"RGZ_Gauge_Residual_39.wxf"}];If[!FileExistsQ[file],Return[Failure["ExportResidualFirst",<||>]]];
 packet=Check[Import[file,"WXF"],$Failed];
 If[!AssociationQ[packet] || Lookup[packet,"ModelFingerprint",None]=!=modelKey[],Return[Failure["ResidualDataModelMismatch",<||>]]];
 If[Lookup[packet,"PinchFormulaFingerprint",None]=!=pinchFormulaFingerprint[],Return[Failure["ResidualKernelMismatch",<|"Action"->"RGZGaugeCheck[...] then RGZExportResidual[]","Explanation"->"The saved residual uses an older pinch formula."|>]]];
 If[Lookup[packet,"NAVMode",None]==="FullMatrix" && Lookup[packet,"NAVFingerprint",None]=!=navFormulaFingerprint[],Return[Failure["ResidualNAVMismatch",<|"Action"->"RGZPTStart[] then RGZGaugeCheck[...] and RGZExportResidual[]"|>]]];
 Switch[mode,"QCD",r=engineGet["RGZ_Residual39_QCD_Zero_Check.wl"];If[FailureQ[r],Return[r]];r=RGZResidual39QCD`RGZ39QCDCheck[file,seconds,"\:663e\:793a\:8fdb\:5ea6"->($config["Display"]=!="Quiet")],
 "M0",r=engineGet["RGZ_Residual39_M0_Diagnostic.wl"];If[FailureQ[r],Return[r]];r=RGZResidual39Diagnostic`RGZ39M0Check[file,seconds,"ParameterPoint"->OptionValue["ParameterPoint"],"\:663e\:793a\:8fdb\:5ea6"->($config["Display"]=!="Quiet")],
 _,Return[Failure["MassLimitMode",<||>]]];
 storeResult["MassLimit:"<>mode,r];r
];
RGZStatus["Mixed"] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZMixStatus[]]];
RGZStatus["PT"] := engineEvaluation@Module[{r=loadPTEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZPTStatus[]]];

Options[RGZNAVSum]={"QuarkMass"->0};
RGZNAVSum[seconds_:300,OptionsPattern[]]:=engineEvaluation@Module[{r,key,dir,mass=OptionValue["QuarkMass"]},
 r=loadVertexEngine[seconds];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_NAV_Matrix.wl",False];If[FailureQ[r],Return[r]];
 key=IntegerString[Hash[{"RGZNAVMatrix/1",modelKey[],FileHash[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}],"SHA256"],FileHash[FileNameJoin[{backendDirectory[],"RGZ_NAV_Matrix.wl"}],"SHA256"],mass},"SHA256"],16,64];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_NAV_matrix_"<>key}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=If[FileExistsQ[FileNameJoin[{dir,"result.wxf"}]],Import[FileNameJoin[{dir,"result.wxf"}],"WXF"],RGZWorkflow`Engine`rgzNAVBuild[dir,mass,seconds]];If[FailureQ[r],Return[r]];storeResult["NAV",r];
 <|"Status"->"Complete","Name"->"NAVFullMatrix","Rows"->r["ConnectionCount"],"Statistics"-><|"ConservedCurrentConnections"->r["ConservedCurrentConnectionCount"],"Stage"->"Raw vertex integrand"|>,"File"->FileNameJoin[{dir,"result.wxf"}]|>
];

fullNAVIncluded[]:=AssociationQ[RGZWorkflow`Engine`rgzPTJob]&&Lookup[RGZWorkflow`Engine`rgzPTJob,"NAVMode",None]==="FullMatrix";
gaugeScope[]:=If[fullNAVIncluded[],"FullMatrixNAVPinch","AAReplacementCandidate"];
navFormulaFingerprint[]:=Hash[{modelKey[],FileHash[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}],"SHA256"],FileHash[FileNameJoin[{backendDirectory[],"RGZ_NAV_Matrix.wl"}],"SHA256"],FileHash[FileNameJoin[{backendDirectory[],"RGZ_NAV_Pinch.wl"}],"SHA256"]},"SHA256"];
prepareNAVPinch[seconds_]:=Module[{r,key,dir},
 r=RGZNAVSum[seconds];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_NAV_Pinch.wl"];If[FailureQ[r],Return[r]];
 key=navFormulaFingerprint[];dir=FileNameJoin[{cacheDirectory[],"RGZ_NAV_pinch_"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=If[FileExistsQ[FileNameJoin[{dir,"result.wxf"}]],Import[FileNameJoin[{dir,"result.wxf"}],"WXF"],RGZWorkflow`Engine`rgzNAVPinchBuild[RGZResult["NAV"],dir,seconds]];
 If[FailureQ[r],Return[r]];r=Join[r,<|"Fingerprint"->key|>];storeResult["NAVPinch",r];r
];

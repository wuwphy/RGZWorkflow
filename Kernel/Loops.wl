(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Explicit adapter to the compatible RGZWorkflow`Engine` computational engine. *)
loadLoopEngine[] := engineEvaluation@Module[{r},r=checksReady[];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_Mixed_OneLoop_Overall.wl"];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_Mixed_Scalar_Accelerator.wl"];If[FailureQ[r],Return[r]];
 RGZWorkflow`Engine`RGZMixSetAcceleration["SingleKernelTimeLimit"->120,"PreprocessTimeLimit"->15];
 RGZWorkflow`Engine`RGZMixSetDisplay[Switch[$config["Display"],"Quiet","Quiet","Verbose","Verbose",_,"Compact"]];Null
];
RGZMixedStart[] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZMixStart[]]];
Options[RGZMixedStartPair]={"Sectors"->All};
RGZMixedStartPair[a_,b_,seconds_:60,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,
 RGZWorkflow`Engine`RGZMixStartPair[a,b,"Category"->OptionValue["Sectors"],"SingleStepTimeLimit"->seconds,"reuseAACache"->False]]];
RGZRunGraph[n_Integer,stage_String,seconds_:120] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 If[!MemberQ[{"Convert","Project","DecomposeT","IntegrateT","ScalarT","DecomposeL","IntegrateL","ScalarL"},stage],Return[Failure["GraphStage",<||>]]];
 RGZWorkflow`Engine`RGZMXRun[n,stage,seconds]
];
Options[RGZMixedComputePair]={"Sectors"->All};
RGZMixedComputePair[a_,b_,seconds_:300,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,
 RGZWorkflow`Engine`RGZMixComputePair[a,b,seconds,"Category"->OptionValue["Sectors"],"reuseAACache"->False]]];
Options[RGZMixedComputeAll]={"Sectors"->All,"Pairs"->All};
RGZMixedComputeAll[seconds_:300,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,
 RGZWorkflow`Engine`RGZMixComputeAll[seconds,"FieldPair"->OptionValue["Pairs"],"Category"->OptionValue["Sectors"],"reuseAACache"->False]]];
RGZMixedResult[seconds_:120] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 r=RGZWorkflow`Engine`RGZMixResult[seconds];If[FailureQ[r],Return[r]];
 If[!validMixed[r],Return[Failure["InvalidMixedMatrices",<||>]]];
 RGZWorkflow`Engine`rgzMixed=r;storeResult["Mixed",r];<|"Status"->"Complete","Name"->"Mixed","Dimensions"->{{5,5},{8,8},{8,8}}|>
];
RGZQCDBenchmark[seconds_:600] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 r=timed["QCDBenchmark",engineGet["RGZ_QCD_Check_Clean.wl",False],seconds];If[FailureQ[r],Return[r]];
 r=RGZWorkflow`Engine`rgzQCDReport;If[FailureQ[r],Return[r]];If[!AssociationQ[r],Return[Failure["InvalidQCDReport",<||>]]];
 storeResult["QCD",r];<|"Status"->"Complete","Name"->"QCD","Statistics"->Lookup[r,"SumCheck",Missing["NotChecked"]]|>
];
(* Completed physical fields are validated separately from stopped diagnostics. *)
ptPartPacketQ[p_] := AssociationQ[p]&&KeyExistsQ[p,"OriginalExpression"]&&
 !qqFFInvalidQ[p["OriginalExpression"]]&&AssociationQ[Lookup[p,"Result",None]]&&
 AllTrue[Keys[p["Result"]],MemberQ[{"Project","Decompose","Integrate","Scalar"},#]&]&&
 !qqFFInvalidQ[Values[p["Result"]]];
ptJobPacketQ[j_] := AssociationQ[j]&&StringQ[Lookup[j,"Directory",None]]&&StringLength[j["Directory"]]>0&&
 IntegerQ[Lookup[j,"Environment",None]]&&j["Environment"]>=0&&IntegerQ[Lookup[j,"KernelSignature",None]]&&
 Lookup[j,"PartsOrder",None]==={"box","navAll","avAll","qseAll"}&&AssociationQ[Lookup[j,"Parts",None]]&&
 Sort[Keys[j["Parts"]]]===Sort[j["PartsOrder"]]&&AllTrue[Values[j["Parts"]],ptPartPacketQ]&&
 ListQ[Lookup[j,"SelfEnergyParts",None]]&&Length[j["SelfEnergyParts"]]===25&&!qqFFInvalidQ[j["SelfEnergyParts"]]&&
 MemberQ[{"FourPinch","GaugeDifference"},Lookup[j,"Mode",None]]&&KeyExistsQ[j,"ReferenceGauge"]&&
 !qqFFInvalidQ[j["ReferenceGauge"]]&&MemberQ[{"FullMatrix","AAReplacement"},Lookup[j,"NAVMode",None]]&&
 If[j["NAVMode"]==="FullMatrix",j["Mode"]==="FourPinch"&&IntegerQ[Lookup[j,"NAVFingerprint",None]],Lookup[j,"NAVFingerprint",None]===None]&&
 AssociationQ[Lookup[j,"QCDConvention",None]]&&Lookup[j["QCDConvention"],"Status",None]==="Passed"&&
 Lookup[j["QCDConvention"],"SharedFactor",None]===1&&KeyExistsQ[j,"Compare"];
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
 "CoefficientID"->OptionValue["Coefficients"],"VerifiedInput"->None,"CacheDirectory"->cacheDirectory[],
 "PreprocessTimeLimit"->OptionValue["PreparationSeconds"],"PrepareTimeLimit"->OptionValue["PreparationSeconds"],"TotalTimeLimit"->OptionValue["TotalSeconds"]];
 If[FailureQ[r],
  storeResult["GaugeStopped",r[[2]]];
  If[ListQ[RGZWorkflow`Engine`rgzGaugeCompletedRows],storeResult["GaugePartial",<|"ReferenceGauge"->OptionValue["ReferenceGauge"],"Rows"->RGZWorkflow`Engine`rgzGaugeCompletedRows,"CalculationScope"->gaugeScope[],"FullNAVIncluded"->fullNAVIncluded[]|>]];
  Return[r]];
 r=Join[r,<|"CalculationScope"->gaugeScope[],"FullNAVIncluded"->fullNAVIncluded[]|>];storeResult["Gauge",r];
 <|"Status"->Lookup[r,"Status","Unknown"],"Rows"->Lookup[r,"CheckedCount",0],"Statistics"->Lookup[r,"Statistics",<||>],"CalculationScope"->gaugeScope[],"FullNAVIncluded"->fullNAVIncluded[]|>
];
residual39RowsQ[rows_] := ListQ[rows]&&Length[rows]===39&&AllTrue[rows,AssociationQ]&&
 AllTrue[rows,Function[row,And@@(KeyExistsQ[row,#]&/@{"ID","MasterIntegral","ResidualCoefficient"})]]&&
 AllTrue[Lookup[rows,"ID"],IntegerQ]&&Sort[Lookup[rows,"ID"]]===Range[39]&&
 !qqFFInvalidQ[KeyTake[#,{"MasterIntegral","ResidualCoefficient"}]&/@rows];
residual39PacketQ[p_] := AssociationQ[p]&&Lookup[p,"Schema",None]==="RGZResidual39/1"&&
 MemberQ[{"FullMatrix","AAReplacement"},Lookup[p,"NAVMode",None]]&&
 NumericQ[Lookup[p,"ReferenceGauge",None]]&&TrueQ[p["ReferenceGauge"]>0]&&
 !qqFFInvalidQ[p["ReferenceGauge"]]&&residual39RowsQ[Lookup[p,"ResidualParts",None]];
RGZExportResidual[] := engineEvaluation@Module[{r=loadPTEngine[],file,report,rows},If[FailureQ[r],Return[r]];report=RGZResult["Gauge"];
 If[!AssociationQ[report],Return[Failure["GaugeReportRequired",<||>]]];rows=Lookup[report,"ResidualParts",{}];
 If[!residual39RowsQ[rows],Return[Failure["Complete39RowsRequired",<||>]]];
 If[!NumericQ[Lookup[report,"ReferenceGauge",None]]||!TrueQ[report["ReferenceGauge"]>0]||qqFFInvalidQ[report["ReferenceGauge"]],Return[Failure["InvalidResidualReport",<||>]]];
 file=FileNameJoin[{dataDirectory[],"RGZ_Gauge_Residual_39.wxf"}];
 r=atomicWXF[file,<|"Schema"->"RGZResidual39/1","WorkflowFingerprint"->workflowFingerprint[],"ModelFingerprint"->modelKey[],"PinchFormulaFingerprint"->pinchFormulaFingerprint[],"NAVMode"->If[TrueQ[Lookup[report,"FullNAVIncluded",False]],"FullMatrix","AAReplacement"],"NAVFingerprint"->If[TrueQ[Lookup[report,"FullNAVIncluded",False]],navFormulaFingerprint[],None],"ReferenceGauge"->report["ReferenceGauge"],"ResidualParts"->rows|>];r
];
Options[RGZMassLimit]={"ParameterPoint"->Automatic};
RGZMassLimit[mode_String,seconds_:60,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[],file,packet},If[FailureQ[r],Return[r]];
 If[!MemberQ[{"M0","QCD"},mode],Return[Failure["MassLimitMode",<||>]]];
 $results=KeyDrop[$results,{"MassLimit:"<>mode}];
 If[mode==="QCD" && OptionValue["ParameterPoint"]=!=Automatic,Return[Failure["ParameterPointM0Only",<|"Explanation"->"ParameterPoint applies only to M0; QCD uses its specified common zero-mass path."|>]]];
 file=FileNameJoin[{dataDirectory[],"RGZ_Gauge_Residual_39.wxf"}];If[!FileExistsQ[file],Return[Failure["ExportResidualFirst",<||>]]];
 packet=Quiet[Check[Import[file,"WXF"],$Failed]];
 If[!residual39PacketQ[packet],Return[Failure["InvalidResidualPacket",<|"Action"->"Regenerate a complete 39-row residual export."|>]]];
 If[Lookup[packet,"WorkflowFingerprint",None]=!=workflowFingerprint[],Return[Failure["ResidualSourceMismatch",<|"Action"->"Recompute the gauge comparison and export residuals with the current sources."|>]]];
 If[!AssociationQ[packet] || Lookup[packet,"ModelFingerprint",None]=!=modelKey[],Return[Failure["ResidualDataModelMismatch",<||>]]];
 If[Lookup[packet,"PinchFormulaFingerprint",None]=!=pinchFormulaFingerprint[],Return[Failure["ResidualKernelMismatch",<|"Action"->"RGZGaugeCheck[...] then RGZExportResidual[]","Explanation"->"The saved residual uses an older pinch formula."|>]]];
 If[Lookup[packet,"NAVMode",None]==="FullMatrix" && Lookup[packet,"NAVFingerprint",None]=!=navFormulaFingerprint[],Return[Failure["ResidualNAVMismatch",<|"Action"->"RGZPTStart[] then RGZGaugeCheck[...] and RGZExportResidual[]"|>]]];
 Switch[mode,"QCD",r=engineGet["RGZ_Residual39_QCD_Zero_Check.wl"];If[FailureQ[r],Return[r]];r=RGZResidual39QCD`RGZ39QCDCheck[file,seconds,"ShowProgress"->($config["Display"]=!="Quiet")],
 "M0",r=engineGet["RGZ_Residual39_M0_Diagnostic.wl"];If[FailureQ[r],Return[r]];r=RGZResidual39Diagnostic`RGZ39M0Check[file,seconds,"ParameterPoint"->OptionValue["ParameterPoint"],"ShowProgress"->($config["Display"]=!="Quiet")],
 _,Return[Failure["MassLimitMode",<||>]]];
 storeResult["MassLimit:"<>mode,r];r
];
RGZStatus["Mixed"] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZMixStatus[]]];
RGZStatus["PT"] := engineEvaluation@Module[{r=loadPTEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZPTStatus[]]];

navMatrixConnectionQ[r_] := AssociationQ[r]&&IntegerQ[Lookup[r,"VertexID",None]]&&r["VertexID"]>0&&
 MatchQ[Lookup[r,"Fields",None],{_String,_String,_String}]&&Sort[Lookup[r,"LegOrder",{}]]==={1,2,3}&&
 And@@(KeyExistsQ[r,#]&/@{"Tensor","TransverseTensor"})&&!qqFFInvalidQ[r];
navPinchConnectionQ[r_,n_,fields_] := AssociationQ[r]&&Lookup[r,"Connection",None]===n&&
 MatchQ[Lookup[r,"Fields",None],{_String,_String,_String}]&&r["Fields"]===fields&&
 KeyExistsQ[r,"Kernel"]&&TrueQ[Lookup[r,"ExactPolynomialReconstruction",False]]&&!qqFFInvalidQ[r];
navPacketQ[p_,schema_,fields_] := AssociationQ[p]&&Lookup[p,"Schema",None]===schema&&
 IntegerQ[Lookup[p,"ConnectionCount",None]]&&p["ConnectionCount"]>0&&
 ListQ[Lookup[p,"Connections",None]]&&Length[p["Connections"]]===p["ConnectionCount"]&&
 AllTrue[p["Connections"],AssociationQ]&&And@@(KeyExistsQ[p,#]&/@fields)&&!qqFFInvalidQ[p]&&
 Switch[schema,
 "RGZNAVMatrix/1",AllTrue[p["Connections"],navMatrixConnectionQ],
 "RGZNAVPinch/1",And@@MapIndexed[navPinchConnectionQ[#1,First[#2],Lookup[#1,"Fields",None]]&,p["Connections"]]&&
  KeyExistsQ[p,"Kernel"]&&p["Kernel"]===Total[Lookup[p["Connections"],"Kernel"]],
 _,False];
Options[RGZNAVSum]={"QuarkMass"->0};
RGZNAVSum[seconds_:300,OptionsPattern[]]:=engineEvaluation@Module[{r,key,dir,mass=OptionValue["QuarkMass"]},
 r=loadVertexEngine[seconds];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_NAV_Matrix.wl",False];If[FailureQ[r],Return[r]];
 key=IntegerString[Hash[{"RGZNAVMatrix/1",workflowFingerprint[],modelKey[],FileHash[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}],"SHA256"],FileHash[FileNameJoin[{backendDirectory[],"RGZ_NAV_Matrix.wl"}],"SHA256"],mass},"SHA256"],16,64];
 dir=FileNameJoin[{cacheDirectory[],"RGZ_NAV_matrix_"<>key}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=If[FileExistsQ[FileNameJoin[{dir,"result.wxf"}]],Import[FileNameJoin[{dir,"result.wxf"}],"WXF"],RGZWorkflow`Engine`rgzNAVBuild[dir,mass,seconds]];If[FailureQ[r],Return[r]];If[!navPacketQ[r,"RGZNAVMatrix/1",{"Tensor","TransverseTensor","Integrand","TransverseIntegrand","ConservedCurrentConnectionCount"}],Return[Failure["NAVCacheInvalid",<|"Directory"->dir|>]]];storeResult["NAV",r];
 <|"Status"->"Complete","Name"->"NAVFullMatrix","Rows"->r["ConnectionCount"],"Statistics"-><|"ConservedCurrentConnections"->r["ConservedCurrentConnectionCount"],"Stage"->"Raw vertex integrand"|>,"File"->FileNameJoin[{dir,"result.wxf"}]|>
];

fullNAVIncluded[]:=AssociationQ[RGZWorkflow`Engine`rgzPTJob]&&Lookup[RGZWorkflow`Engine`rgzPTJob,"NAVMode",None]==="FullMatrix";
gaugeScope[]:=If[fullNAVIncluded[],"FullMatrixNAVPinch","AAReplacementCandidate"];
navFormulaFingerprint[]:=Hash[{workflowFingerprint[],modelKey[],FileHash[FileNameJoin[{dataDirectory[],"RGZ_all_raw.wxf"}],"SHA256"],FileHash[FileNameJoin[{backendDirectory[],"RGZ_NAV_Matrix.wl"}],"SHA256"],FileHash[FileNameJoin[{backendDirectory[],"RGZ_NAV_Pinch.wl"}],"SHA256"]},"SHA256"];
prepareNAVPinch[seconds_]:=Module[{r,key,dir},
 r=RGZNAVSum[seconds];If[FailureQ[r],Return[r]];
 r=engineGet["RGZ_NAV_Pinch.wl"];If[FailureQ[r],Return[r]];
 key=navFormulaFingerprint[];dir=FileNameJoin[{cacheDirectory[],"RGZ_NAV_pinch_"<>IntegerString[key,16,64]}];If[!DirectoryQ[dir],CreateDirectory[dir]];
 r=If[FileExistsQ[FileNameJoin[{dir,"result.wxf"}]],Import[FileNameJoin[{dir,"result.wxf"}],"WXF"],RGZWorkflow`Engine`rgzNAVPinchBuild[RGZResult["NAV"],dir,seconds]];
 If[FailureQ[r],Return[r]];If[!navPacketQ[r,"RGZNAVPinch/1",{"Kernel"}]||Lookup[r["Connections"],"Fields",None]=!=Lookup[RGZResult["NAV"]["Connections"],"Fields",None],Return[Failure["NAVPinchCacheInvalid",<|"Directory"->dir|>]]];r=Join[r,<|"Fingerprint"->key|>];storeResult["NAVPinch",r];r
];

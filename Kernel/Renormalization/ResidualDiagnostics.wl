(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Run the specialized saved 39-row residual diagnostics, separate from the conventional amplitude.

   Inputs:
   Computed QQ graphs or form factors, with the declared dimensional and mass-limit prescription.

   Dependencies:
   Algebra/ tools and already computed OneLoop/ results; no candidate PT task is required.
*)


residual39RowsQ[rows_] := ListQ[rows]&&Length[rows]===39&&AllTrue[rows,AssociationQ]&&
 AllTrue[rows,Function[row,And@@(KeyExistsQ[row,#]&/@{"ID","MasterIntegral","ResidualCoefficient"})]]&&
 AllTrue[Lookup[rows,"ID"],IntegerQ]&&Sort[Lookup[rows,"ID"]]===Range[39]&&
 !qqFFInvalidQ[KeyTake[#,{"MasterIntegral","ResidualCoefficient"}]&/@rows];
residual39PacketQ[p_] := AssociationQ[p]&&Lookup[p,"Schema",None]==="RGZResidual39/1"&&
 MemberQ[{"FullMatrix","AAReplacement"},Lookup[p,"NAVMode",None]]&&
 NumericQ[Lookup[p,"ReferenceGauge",None]]&&TrueQ[p["ReferenceGauge"]>0]&&
 !qqFFInvalidQ[p["ReferenceGauge"]]&&residual39RowsQ[Lookup[p,"ResidualParts",None]];
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
 Switch[mode,"QCD",r=engineGet["Identities/QCDLimit.wl"];If[FailureQ[r],Return[r]];r=RGZResidual39QCD`RGZ39QCDCheck[file,seconds,"ShowProgress"->($config["Display"]=!="Quiet")],
 "M0",r=engineGet["Renormalization/RegulatorLimit.wl"];If[FailureQ[r],Return[r]];r=RGZResidual39Diagnostic`RGZ39M0Check[file,seconds,"ParameterPoint"->OptionValue["ParameterPoint"],"ShowProgress"->($config["Display"]=!="Quiet")],
 _,Return[Failure["MassLimitMode",<||>]]];
 storeResult["MassLimit:"<>mode,r];r
];

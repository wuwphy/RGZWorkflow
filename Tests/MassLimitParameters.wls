(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
Get[FileNameJoin[{project,"Kernel","Backend","RGZ_Residual39_M0_Diagnostic.wl"}]];
point=<|"p^2"->-7,"m^2"->4,"mu^2"->2,"mG^4"->9,"N"->4|>;
file=FileNameJoin[{$TemporaryDirectory,"RGZ-point-"<>CreateUUID[]<>".wxf"}];
Export[file,<|"\:53c2\:8003\:89c4\:8303"->1,"\:6b8b\:5dee\:5206\:9879"->Table[<|"\:7f16\:53f7"->j,"\:4e3b\:79ef\:5206"->FeynCalc`A0[RGZWorkflow`Engine`m^2],"\:6b8b\:5dee\:7cfb\:6570"->If[j===1,Pi^2 (RGZWorkflow`Engine`m^2-4),0]|>,{j,39}]|>,"WXF"];
custom=RGZResidual39Diagnostic`RGZ39M0Check[file,60,"ParameterPoint"->point,"\:663e\:793a\:8fdb\:5ea6"->False];
default=RGZResidual39Diagnostic`RGZ39M0Check[file,60,"\:663e\:793a\:8fdb\:5ea6"->False];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"RGZ-point-api-"<>CreateUUID[]}],"Display"->"Quiet"];
apiFile=FileNameJoin[{RGZConfiguration[]["DataDirectory"],"RGZ_Gauge_Residual_39.wxf"}];
packet=Import[file,"WXF"];
Export[apiFile,Join[packet,<|"ModelFingerprint"->RGZWorkflow`Private`modelKey[],"PinchFormulaFingerprint"->RGZWorkflow`Private`pinchFormulaFingerprint[]|>],"WXF"];
api=Block[{RGZWorkflow`Private`loadLoopEngine,RGZWorkflow`Private`engineGet},
 RGZWorkflow`Private`loadLoopEngine[]:=True;
 RGZWorkflow`Private`engineGet[___]:=True;
 RGZMassLimit["M0",60,"ParameterPoint"->point]];
report=TestReport[{
 VerificationTest[api["M0\:7ed3\:679c"],0,TestID->"PublicParameterForwarding"],
 VerificationTest[Options[RGZMassLimit],{"ParameterPoint"->Automatic},TestID->"PublicOption"],
 VerificationTest[custom["M0\:7ed3\:679c"],0,TestID->"CustomPointAppliedToFullCoefficient"],
 VerificationTest[custom["\:53c2\:6570\:70b9"],<|"p^2"->-7,"m^2"->4,"mu^2"->2,"mG^4"->9,"N=SUNN"->4|>,TestID->"CustomPointRecorded"],
 VerificationTest[default["M0\:76841/epsilon\:7cfb\:6570"],45,TestID->"DefaultPointUnaffected"],
 VerificationTest[MissingQ[custom["\:72ec\:7acb\:7cbe\:786e\:7ed3\:679c\:5bf9\:7167"]],True,TestID->"HistoricalReferenceOnlyDefaultPoint"],
 VerificationTest[Block[{RGZResidual39Diagnostic`Private`$parameterPoint=point},RGZResidual39Diagnostic`Private`pointExpression[{FeynCalc`SPD[RGZWorkflow`Engine`p,RGZWorkflow`Engine`p],RGZWorkflow`Engine`m^2,RGZWorkflow`Engine`\:03bc^2,RGZWorkflow`Engine`mG^4,FeynCalc`SUNN}]],{-7,4,2,9,4},TestID->"AllPointCoordinates"],
 VerificationTest[FailureQ[RGZResidual39Diagnostic`Private`normalizePoint[<|"N"->1|>]],True,TestID->"RejectInvalidColour"],
 VerificationTest[FailureQ[RGZResidual39Diagnostic`Private`normalizePoint[<|"p^2"->-7.0|>]],True,TestID->"RejectApproximateInput"],
 VerificationTest[FailureQ[RGZResidual39Diagnostic`Private`normalizePoint[<|"alpha"->1|>]],True,TestID->"KeepGaugeParameterSymbolic"],
 VerificationTest[FailureQ[RGZResidual39Diagnostic`Private`normalizePoint[<|"mG^4"->16|>]],True,TestID->"RejectCoincidentPoles"],
 VerificationTest[RGZResidual39Diagnostic`Private`normalizePoint[<|"p^2"->-7|>]["m^2"],9,TestID->"PartialOverride"]
}];
DeleteFile[file];
Print[<|"Passed"->report["TestsSucceededCount"],"Failed"->report["TestsFailedCount"]|>];
If[report["TestsFailedCount"]>0,Print[report["TestsFailed"]]];
Exit[If[report["TestsFailedCount"]===0,0,1]];

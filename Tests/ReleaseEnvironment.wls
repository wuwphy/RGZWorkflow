(* SPDX-License-Identifier: GPL-3.0-or-later *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"rgz-environment-"<>CreateUUID[]}],"Display"->"Quiet"];
r=Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];If[FailureQ[r],Print[r];Exit[1]];
packet=<|"WolframVersion"->$Version,"SystemID"->$SystemID,"FeynCalcVersion"->FeynCalc`$FeynCalcVersion,"FeynArtsVersion"->FeynArts`$FeynArtsVersion,"PackageVersion"->RGZWorkflow`$RGZWorkflowVersion|>;
Print[packet];file=Environment["RGZ_TEST_ENVIRONMENT_RECORD"];
If[StringQ[file]&&file=!="",Export[file,packet,"RawJSON"]];Exit[0];

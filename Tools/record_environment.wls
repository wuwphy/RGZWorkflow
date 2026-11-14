(* SPDX-License-Identifier: GPL-3.0-or-later *)
Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],"RGZWorkflow.wl"}]];
RGZInitialize[Environment["RGZ_TEST_WORK"],"Display"->"Quiet"];
rgzEnvironmentLoad=RGZLoadFeynCalc[];
If[FailureQ[rgzEnvironmentLoad],Print[rgzEnvironmentLoad];Exit[1]];
Export[Environment["RGZ_TEST_ENVIRONMENT_RECORD"],<|"WolframVersion"->$Version,
 "SystemID"->$SystemID,"FeynCalcVersion"->FeynCalc`$FeynCalcVersion,
 "FeynArtsVersion"->FeynArts`$FeynArtsVersion|>,"RawJSON"];
Exit[0];

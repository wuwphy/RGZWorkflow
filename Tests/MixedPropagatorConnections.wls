(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Fresh FA diagrams and general-mass derivative-vertex cancellation.
   All graph checkpoints belong to an isolated temporary work directory. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
work=CreateDirectory[FileNameJoin[{$TemporaryDirectory,"RGZ-mixed-connections-"<>CreateUUID[]}]];
model=Environment["RGZ_TEST_MODEL"];
If[!StringQ[model]||model==="",model=FileNameJoin[{project,"Models","RGZ_W2.6"}]];
testModel=FileNameJoin[{work,"Model"}];
Scan[CopyFile[model<>#,testModel<>#]&, {".fr",".gen",".mod"}];
RGZInitialize[work,"ModelBase"->testModel,"Display"->"Quiet"];
r=Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];If[FailureQ[r],Print[r];Exit[1]];
r=RGZWorkflow`Private`loadLoopEngine[];If[FailureQ[r],Print[r];Exit[1]];
RGZWorkflow`Private`engineCode[Import[FileNameJoin[{project,"Tests","MixedPropagatorConnectionsBody.wl"}],"Text"]];

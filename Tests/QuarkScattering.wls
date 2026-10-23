(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
rgzTestProject=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{rgzTestProject,"RGZWorkflow.wl"}]];
(* Synthetic checkpoints must never replace the supplied FR export. *)
rgzSourceWork=Environment["RGZ_TEST_WORK"];
rgzTestWork=FileNameJoin[{$TemporaryDirectory,"rgz-quark-scattering-"<>CreateUUID[]}];
RGZInitialize[rgzTestWork,"Display"->"Quiet"];
If[StringQ[rgzSourceWork]&&DirectoryQ[rgzSourceWork],
 Scan[Function[name,With[{source=FileNameJoin[{rgzSourceWork,"Data",name}],target=FileNameJoin[{rgzTestWork,"Data",name}]},
  If[FileExistsQ[source],CopyFile[source,target,OverwriteTarget->True]]]],{"Tree.wxf","VerticesMetadata.wxf","RGZ_all_raw.wxf"}]];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
RGZWorkflow`Private`loadPTEngine[];
RGZWorkflow`Private`engineEvaluation[RGZWorkflow`Private`engineGet["RGZ_Residual39_M0_Diagnostic.wl"]];
RGZWorkflow`Private`engineEvaluation[RGZWorkflow`Private`engineGet["RGZ_Quark_Amplitude.wl",False]];
RGZWorkflow`Private`engineCode[Import[FileNameJoin[{rgzTestProject,"Tests","QuarkScatteringBody.wl"}],"Text"]];

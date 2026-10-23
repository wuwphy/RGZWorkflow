(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
rgzTestProject=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{rgzTestProject,"RGZWorkflow.wl"}]];
rgzTestWork=Environment["RGZ_TEST_WORK"];
If[!StringQ[rgzTestWork]||rgzTestWork==="",rgzTestWork="/tmp/rgz-integration-final-serial"];
RGZInitialize[rgzTestWork,"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
RGZWorkflow`Private`loadPTEngine[];
RGZWorkflow`Private`engineCode[Import[FileNameJoin[{rgzTestProject,"Tests","PinchStagesBody.wl"}],"Text"]];

(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
rgzTestProject=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{rgzTestProject,"RGZWorkflow.wl"}]];
RGZInitialize["/tmp/rgz-colour-normalization-test","Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
RGZWorkflow`Private`loadPTEngine[];
RGZWorkflow`Private`engineCode[Import[FileNameJoin[{rgzTestProject,"Tests","ColourNormalizationBody.wl"}],"Text"]];

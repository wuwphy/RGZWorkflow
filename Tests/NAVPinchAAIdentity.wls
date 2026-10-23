(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{project,"Work"}]];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
Print[RGZNAVSum[120]];
RGZWorkflow`Private`engineGet["RGZ_NAV_Pinch.wl"];
RGZWorkflow`Private`engineGet["RGZ_OneLoop_AA_PT_Check.wl"];
RGZWorkflow`Private`engineCode[Import[FileNameJoin[{project,"Tests","NAVPinchAAIdentityBody.wl"}],"Text"]];

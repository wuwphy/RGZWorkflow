(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{project,"Work"}],"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
RGZWorkflow`Private`engineGet["RGZ_OneLoop_AA_PT_Check.wl"];
RGZWorkflow`Private`engineGet["RGZ_NAV_Pinch.wl"];
RGZWorkflow`Private`engineCode["checks=Table[!FailureQ[rgzNAVExactPoleFactor[den,x,q]],{den,{x,x^2,x^2+\\[Alpha] M^4,RGZLoopDelta[x],x RGZLoopDelta[x],RGZLoopDelta[x]^2}}];Print[checks];Exit[If[And@@checks,0,1]]"];

(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
rgzTestProject=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{rgzTestProject,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"RGZ-pinch-physics-test"}],"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
rgzDirectKernels=RGZPinchKernels[];
If[!AssociationQ[rgzDirectKernels]||rgzDirectKernels["Mode"]=!="FourPinch",Print["Public kernel access failed"];Exit[1]];
RGZWorkflow`Private`loadLoopEngine[];
Scan[RGZWorkflow`Private`engineGet,{"RGZ_QCD_PartComparison.wl","RGZ_QCD_Stepwise_Check.wl"}];
RGZWorkflow`Private`engineCode[Import[FileNameJoin[{rgzTestProject,"Tests","ReviewedPinchFormula.wl"}],"Text"]];
RGZWorkflow`Private`engineCode[Import[FileNameJoin[{rgzTestProject,"Tests","PinchPhysicsBody.wl"}],"Text"]];

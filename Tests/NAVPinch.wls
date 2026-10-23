(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{project,"Work"}]];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
RGZWorkflow`Private`engineGet["RGZ_OneLoop_AA_PT_Check.wl"];
r=RGZWorkflow`Private`engineEvaluation[RGZWorkflow`Private`prepareNAVPinch[120]];
Print[If[AssociationQ[r],KeyTake[r,{"Schema","ConnectionCount","Fingerprint"}],r]];
If[FailureQ[r],Exit[1]];
Print["All 73 Ward reconstructions = ",And@@Lookup[r["Connections"],"ExactPolynomialReconstruction"]];
Print["Nonzero pinch connections = ",Count[Lookup[r["Connections"],"Kernel"],Except[0]]];
Print["Pinch tensor retains quark momentum = ",!FreeQ[r["Kernel"],RGZWorkflow`Engine`rgzNAVIncoming]];
Export[FileNameJoin[{project,"output","review","NAVPinch-summary.json"}],KeyTake[r,{"Schema","ConnectionCount","Fingerprint"}],"RawJSON"];
Exit[If[Length[r["Connections"]]===73&&FreeQ[r["Kernel"],RGZWorkflow`Engine`rgzNAVIncoming],0,1]];

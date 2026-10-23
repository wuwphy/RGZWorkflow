(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{project,"Work"}]];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
summary=RGZNAVSum[120];If[FailureQ[summary],Print[summary];Exit[1]];
RGZWorkflow`Private`engineCode["
navAAA=First[Select[RGZWorkflow`RGZResult[\"NAV\"][\"Connections\"],#[\"Fields\"]==={\"A\",\"A\",\"A\"}&]][\"Tensor\"];
navAAA=navAAA/.{m->0,mG->0,M->0,\\[Mu]->0,\\[Alpha]->1};
navV=SMP[\"g_s\"] SUNF[rgzNAVA,rgzNAVB,rgzNAVC] (MTD[rgzNAVNu,rgzNAVRho] FVD[p-q,rgzNAVSigma]+MTD[rgzNAVNu,rgzNAVSigma] FVD[-2p-q,rgzNAVRho]+MTD[rgzNAVRho,rgzNAVSigma] FVD[p+2q,rgzNAVNu]);
navExpected=I navV/(SPD[p,p] SPD[q,q] SPD[p+q,p+q]);
navDifference=TimeConstrained[Cancel[Together[ExpandScalarProduct[Contract[FCI[navAAA-navExpected]]]]],120,$TimedOut];
Print[<|\"QCD_FeynmanGauge_Independent_AAA_Tensor\"->TrueQ[navDifference===0]|>];
Exit[If[navDifference===0,0,1]];"];

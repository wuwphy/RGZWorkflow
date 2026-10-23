(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Configuration and fail-closed external-state checks; no production cache. *)
Get[FileNameJoin[{DirectoryName[$InputFileName,2],"RGZWorkflow.wl"}]];
Module[{work=FileNameJoin[{$TemporaryDirectory,"rgz-masses-"<>CreateUUID[]}],zeroKey,massKey,checks},
 RGZInitialize[work];zeroKey=RGZWorkflow`Private`modelKey[];
 checks={RGZConfiguration[]["QuarkMasses"]==={0,0},RGZWorkflow`Private`masslessQQReady[]===True};
 RGZInitialize[work,"QuarkMasses"->{1,2}];massKey=RGZWorkflow`Private`modelKey[];
 checks=Join[checks,{zeroKey=!=massKey,
   RGZWorkflow`Private`quarkMassRules[]==={RGZWorkflow`Engine`mQ1->1,RGZWorkflow`Engine`mQ2->2},
   MatchQ[RGZQQStart[1],Failure["MassiveQQNotImplemented",_Association]],
   MatchQ[RGZQQGaugeStart[1],Failure["MassiveQQNotImplemented",_Association]],
   FailureQ[RGZInitialize[work,"QuarkMasses"->{-1,0}]],
   FailureQ[RGZInitialize[work,"QuarkMasses"->{1}]],
   FailureQ[RGZInitialize[work,"QuarkMasses"->{True,0}]]}];
 Print["Quark mass checks: ",checks];Exit[If[And@@checks,0,1]]];

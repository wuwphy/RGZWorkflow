(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Expose the explicitly requested QCD benchmark through the public API.

   Inputs:
   Tree identities or computed amplitudes and the source-linked reports being checked.

   Dependencies:
   Tree/, OneLoop/, Algebra/ and Renormalization/ for the result being checked.
*)


RGZQCDBenchmark[seconds_:600] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 r=timed["QCDBenchmark",engineGet["Identities/QCDBenchmark.wl",False],seconds];If[FailureQ[r],Return[r]];
 r=RGZWorkflow`Engine`rgzQCDReport;If[FailureQ[r],Return[r]];If[!AssociationQ[r],Return[Failure["InvalidQCDReport",<||>]]];
 storeResult["QCD",r];<|"Status"->"Complete","Name"->"QCD","Statistics"->Lookup[r,"SumCheck",Missing["NotChecked"]]|>
];

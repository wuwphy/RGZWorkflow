(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Independent 4x4 Clifford matrices; no FeynCalc or model helpers. *)
z2=ConstantArray[0,{2,2}];id2=IdentityMatrix[2];id4=IdentityMatrix[4];
gam=Join[{ArrayFlatten[{{id2,z2},{z2,-id2}}]},Table[ArrayFlatten[{{z2,PauliMatrix[j]},{-PauliMatrix[j],z2}}],{j,3}]];
slash[v_]:=v[[1]]gam[[1]]-Sum[v[[j+1]]gam[[j+1]],{j,3}];sq[v_]:=v[[1]]^2-Total[Rest[v]^2];
r={5,0,0,2 Sqrt[6]};rp={5,2 Sqrt[6],0,0};k={2,1,-1,1/2};q=r-k;ell=rp-k;mq=1;dk=sq[k]-mq^2;
checks=Flatten[Table[{VerificationTest[Simplify[(gam[[j]].(slash[k]+mq id4).slash[q]+dk gam[[j]]).(slash[r]+mq id4)],ConstantArray[0,{4,4}],TestID->"RightWard"<>ToString[j]],VerificationTest[Simplify[(slash[rp]+mq id4).(slash[ell].(slash[k]+mq id4).gam[[j]]+dk gam[[j]])],ConstantArray[0,{4,4}],TestID->"LeftWard"<>ToString[j]]},{j,4}]];
report=TestReport[checks];Print[<|"Passed"->report["TestsSucceededCount"],"Failed"->report["TestsFailedCount"]|>];Exit[If[report["TestsFailedCount"]===0,0,1]];

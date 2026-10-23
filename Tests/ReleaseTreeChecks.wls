(* SPDX-License-Identifier: GPL-3.0-or-later *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
work=Environment["RGZ_TEST_WORK"];
If[!StringQ[work]||!FileExistsQ[FileNameJoin[{work,"Data","Tree.wxf"}]],Print["Isolated FeynRules export required."];Exit[1]];
RGZInitialize[work,"Display"->"Quiet"];
r=Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];If[FailureQ[r],Print[r];Exit[1]];
checks=<||>;
r=RGZCheckPropagators[300];Print[r];AssociateTo[checks,"Original69Propagators"->TrueQ[!FailureQ[r]&&r["Rows"]===69&&r["Statistics"]===<|"Minkowski"-><|"通过"->69|>,"Kernel"-><|"通过"->69|>|>]];
r=RGZCheckVertices[60,"IDs"->All];Print[r];AssociateTo[checks,"All48Vertices"->TrueQ[!FailureQ[r]&&r["Rows"]===48&&Total[Values[r["Statistics"]]]===48&&SubsetQ[{"FA=FR；未录入手推","三方一致"},Keys[r["Statistics"]]]]];
r=RGZCheckOrders[60,"IDs"->All,"Mode"->"Compare"];Print[r];AssociateTo[checks,"All702Orders"->TrueQ[!FailureQ[r]&&r["Rows"]===702&&r["Statistics"]===<|"通过"->702|>]];
Print[checks];Exit[If[And@@Values[checks],0,1]];

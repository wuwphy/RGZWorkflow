(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
testWork=Environment["RGZ_TEST_WORK"];If[!StringQ[testWork]||testWork==="",testWork=FileNameJoin[{$TemporaryDirectory,"RGZWorkflow-integration-0.3.0"}]];
RGZInitialize[testWork];
raw=Import[FileNameJoin[{testWork,"Data","RGZ_all_raw.wxf"}],"WXF"];vertexCount=Length[raw];expectedOrders=Total[Factorial[Length[#[[1]]]]&/@raw];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
rows={};failed=False;
Do[r=RGZCheckOrders[60,"IDs"->{id},"Mode"->"Compare"];
 If[FailureQ[r],Print[r];failed=True;Break[]];
 one=RGZResult["OrdersCompared"];rows=Join[rows,one];
 Print["Orders vertex ",id," / ",vertexCount,": ",r];
 If[!AllTrue[one,#["状态"]==="通过"&],failed=True],{id,vertexCount}];
Export[FileNameJoin[{project,"output","review","Lagrangian-orders-comparison.wxf"}],rows,"WXF"];
Print[<|"CheckedPermutations"->Length[rows],"Statistics"->Counts[Lookup[rows,"状态"]]|>];
Exit[If[!failed&&Length[rows]===expectedOrders,0,1]];

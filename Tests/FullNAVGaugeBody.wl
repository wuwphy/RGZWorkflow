(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Actual complete NAV kernel; zero mixed matrices are a wiring fixture,
   never a claim that the physical RGZ residual vanishes. *)
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",checks[n]]);
fixture=<|"T"->ConstantArray[0,{5,5}],"L"->ConstantArray[0,{8,8}],"全颜色张量矩阵"->ConstantArray[0,{8,8}],"T场顺序"->RGZMixBasis["T"],"L场顺序"->RGZMixBasis["L"]|>;
qcdfixture=<|"逐项结果"->Table[<|"项目"->nm,"状态"->"积分前后均通过","文献的I11系数"->1,"计算的I11系数"->1|>,{nm,{"box","nav全部","av全部","qse全部","gse"}}],"总和检查"-><|"抵消检查"->"通过","计算的五类规范差总和 / I11"->0|>|>;
packet=RGZWorkflow`Private`prepareNAVPinch[120];If[FailureQ[packet],Print[packet];Exit[1]];
RGZWorkflow`Private`storeResult["Mixed",fixture];RGZWorkflow`Private`storeResult["QCD",qcdfixture];
(* Exercise public defaults while retaining the already verified real NAV
   packet and isolating fixture PT caches from the physical workspace. *)
Block[{RGZWorkflow`Private`cacheDirectory,RGZWorkflow`Private`prepareNAVPinch},
 RGZWorkflow`Private`cacheDirectory[]:="/tmp/rgz-full-nav-gauge-test";
 RGZWorkflow`Private`prepareNAVPinch[___]:=packet;
 r=RGZWorkflow`RGZPTStart[]];
record["FullNAVInitialized",AssociationQ[r]&&rgzPTJob["NAVMode"]==="FullMatrix"];
record["NAVReplacedExactlyOnce",SameQ[rgzPTJob["项目"]["nav全部"]["原式"],packet["Kernel"]]];
Do[r=RGZPTRun["nav全部",stage,300];Print[r];record["NAV:"<>stage,AssociationQ[r]];If[FailureQ[r],Print[checks];Exit[1]],{stage,{"投影","分解","积分","标量"}}];
actualNAV=rgzPTJob["项目"]["nav全部"]["结果"]["标量"];
record["ScalarNAVHasNoUnintegratedHeads",RGZPTGood["标量",actualNAV]];
Do[part=rgzPTJob["项目"][nm];AssociateTo[part,"结果"-><|"标量"->0|>];RGZPTSetPart[nm,part],{nm,{"box","av全部","qse全部"}}];
(* Capture the real comparison input; it must contain the new integrated NAV once. *)
Block[{RGZGaugeFixedCompare},RGZGaugeFixedCompare[input_,___]:=<|"Captured"->input|>;
 captured=RGZPTFixedCompare[1,10];
 record["GaugeComparisonIncludesNewNAVOnce",Count[captured["Captured"],actualNAV]===1&&Last[captured["Captured"]]===0];];
Block[{RGZWorkflow`Engine`RGZPTFixedCompare},RGZWorkflow`Engine`RGZPTFixedCompare[___]:=<|"状态"->"只检查了选定系数","本次检查项数"->1,"统计"-><||>|>;
 result=RGZWorkflow`RGZGaugeCheck[10];record["PublicReportMarksFullNAV",TrueQ[result["FullNAVIncluded"]]&&result["CalculationScope"]==="FullMatrixNAVPinch"]];
Export["/tmp/rgz-full-nav-integrated.wxf",actualNAV,"WXF"];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];

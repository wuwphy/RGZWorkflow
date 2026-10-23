(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Zero mixed-matrix/QCD metadata fixtures test only the stage machinery.
   Tests/FeynCalc.wls separately calculates the actual QCD gluon diagrams. *)
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",checks[n]]);
fixture=<|"T"->ConstantArray[0,{5,5}],"L"->ConstantArray[0,{8,8}],"全颜色张量矩阵"->ConstantArray[0,{8,8}],"T场顺序"->RGZMixBasis["T"],"L场顺序"->RGZMixBasis["L"]|>;
qcdfixture=<|"逐项结果"->Table[<|"项目"->nm,"状态"->"积分前后均通过","文献的I11系数"->1,"计算的I11系数"->1|>,{nm,{"box","nav全部","av全部","qse全部","gse"}}],"总和检查"-><|"抵消检查"->"通过","计算的五类规范差总和 / I11"->0|>|>;
RGZWorkflow`Private`storeResult["Mixed",fixture];RGZWorkflow`Private`storeResult["QCD",qcdfixture];
r=RGZWorkflow`RGZPTStart["NAVMode"->"AAReplacement"];record["DefaultStartIncludesGammaP",AssociationQ[r]&&rgzPTJob["Mode"]==="FourPinch"];
firstDir=rgzPTJob["目录"];
record["PreviousStageGuard",FailureQ[RGZWorkflow`RGZRunPart["nav","积分",30]]];
Do[rr=RGZWorkflow`RGZRunPart[nm,st,120];record["Stage:"<>nm<>st,AssociationQ[rr]],{nm,{"box","nav","av","qse"}},{st,{"投影","分解","积分","标量"}}];
r=RGZWorkflow`RGZPinchResult[];record["CompletedResultIsFullPinch",AssociationQ[r]&&r["Mode"]==="FourPinch"];
record["NoTensorPVInFinalPinches",AssociationQ[r]&&And@@(RGZPTGood["标量",#]&/@Values[r["pinch分项"]])];
RGZWorkflow`RGZPTStart["NAVMode"->"AAReplacement"];rr=RGZWorkflow`RGZRunPart["nav","投影",30];record["StageCacheRestored",AssociationQ[rr]&&rr["状态"]==="缓存恢复"];
record["StableCacheIdentity",rgzPTJob["目录"]===firstDir];
RGZWorkflow`RGZPTStart["NAVMode"->"AAReplacement","Mode"->"GaugeDifference","ReferenceGauge"->1];record["ModeGetsDifferentCache",rgzPTJob["目录"]=!=firstDir];
Do[rr=RGZWorkflow`RGZRunPart[nm,st,120];record["DifferenceStage:"<>nm<>st,AssociationQ[rr]],{nm,{"box","nav","av","qse"}},{st,{"投影","分解","积分","标量"}}];
r=RGZWorkflow`RGZPinchResult[];
record["DifferenceResultHasNoFullSelfEnergySum",AssociationQ[r]&&MatchQ[r["候选总和T"],_Missing]];
record["DifferenceCannotBeAddedAsFullSelfEnergy",RGZPTFixedCompare[1,10][[1]]==="FullPinchRequired"];
oldJob=rgzPTJob;AssociateTo[rgzPTJob,"KernelVersion"->"old"];
record["LiveLegacyJobRejected",RGZWorkflow`RGZRunPart["box","投影",10][[1]]==="StalePTJob"];
rgzPTJob=oldJob;
record["SaveCurrentPT",RGZWorkflow`RGZSave["File"->FileNameJoin[{$TemporaryDirectory,"rgz-pinch-current-session.wxf"}]]["Status"]==="Saved"];
Clear[rgzPTJob];r=RGZWorkflow`RGZRestore["File"->FileNameJoin[{$TemporaryDirectory,"rgz-pinch-current-session.wxf"}]];
record["RestoreCurrentPT",AssociationQ[r]&&RGZPTJobCheck[]===True];
legacyResidual=FileNameJoin[{RGZWorkflow`Private`dataDirectory[],"RGZ_Gauge_Residual_39.wxf"}];
Export[legacyResidual,<|"Schema"->"RGZResidual39/1","ModelFingerprint"->RGZWorkflow`Private`modelKey[]|>,"WXF"];
record["LegacyResidualRejected",RGZWorkflow`RGZMassLimit["QCD",10][[1]]==="ResidualKernelMismatch"];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
Exit[If[And@@Values[checks],0,1]];

(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Exact comparison with the paper table; never changes workflow results. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
files=FileNames["factored.wxf",FileNameJoin[{project,"Work","Cache"}],Infinity];file=Last[SortBy[files,FileDate]];live=Import[file,"WXF"];paper=RGZPaperResult[];
rules={RGZWorkflow`ScalarAudit`rMinus->RGZPaper`a,RGZWorkflow`ScalarAudit`rPlus->RGZPaper`b,RGZWorkflow`ScalarAudit`mu2->RGZPaper`mu2,RGZWorkflow`ScalarAudit`s->RGZPaper`s,RGZWorkflow`ScalarAudit`t->RGZPaper`t,
 RGZWorkflow`Engine`qqS->RGZPaper`s,RGZWorkflow`Engine`qqT->RGZPaper`t,RGZWorkflow`Parameters`muR2->RGZPaper`muR2,FeynCalc`SUNN->RGZPaper`Nc,FeynCalc`SMP["g_s"]->RGZPaper`gs,
 RGZWorkflow`RGZPVFinite->RGZPaper`Finite,FeynCalc`A0->RGZPaper`A0,FeynCalc`B0->RGZPaper`B0,FeynCalc`C0->RGZPaper`C0,FeynCalc`D0->RGZPaper`D0};
normfun[f_]:=Module[{z=f/.rules},z/.{RGZPaper`Finite[m_,scale_]:>RGZPaper`Finite[Map[Cancel[Together[#]]&,m],Cancel[Together[scale]]],Log[a_]:>Log[Cancel[Together[a]]]}];
lf=DeleteDuplicates[Flatten[(normfun/@#["ReducedFiniteFunctionCoefficients"][[All,1]])&/@live["Rows"],1]];
pf=normfun/@paper["Functions"];
Print["FUNCTION SETS ",<|"Live"->Length[lf],"Paper"->Length[pf],"Equal"->(Sort[lf]===Sort[pf]),"LiveOnly"->Complement[lf,pf],"PaperOnly"->Complement[pf,lf]|>//InputForm];
rows={};
Do[pr=SelectFirst[paper["Rows"],#["ColorID"]===lr["ColorID"]&&#["SpinID"]===lr["SpinID"]&];
 lm=Association[(normfun[First[#]]->(Last[#]/.rules))&/@lr["ReducedFiniteFunctionCoefficients"]];
 pm=Association[(pf[[First[#]+1]]->Last[#])&/@pr["ReducedCoefficients"]];keys=Union[Keys[lm],Keys[pm]];
 pref=Cancel[Together[(lr["ScalarPrefactor"]/.rules)-pr["Prefactor"]]];
 diffs=Table[TimeConstrained[Cancel[Together[Lookup[lm,k,0]-Lookup[pm,k,0]]],30,$TimedOut],{k,keys}];
 AppendTo[rows,<|"ColorID"->lr["ColorID"],"SpinID"->lr["SpinID"],"LiveCoefficients"->Length[lm],"PaperCoefficients"->Length[pm],"PrefactorEqual"->(pref===0),"CoefficientsEqual"->AllTrue[diffs,#===0&],"NonzeroDifferences"->Count[diffs,Except[0]]|>];Print[Last[rows]//InputForm],{lr,live["Rows"]}];
report=<|"Schema"->"RGZPaperExactComparison/1","LiveFile"->file,"LiveSHA256"->IntegerString[FileHash[file,"SHA256"],16,64],"ReferenceFile"->paper["File"],"ReferenceSHA256"->IntegerString[FileHash[paper["File"],"SHA256"],16,64],"FunctionSetsEqual"->(Sort[lf]===Sort[pf]),"Rows"->rows,"AllCoefficientsEqual"->AllTrue[rows,TrueQ[#["PrefactorEqual"]]&&TrueQ[#["CoefficientsEqual"]]&],"CurrentSourceFingerprint"->live["SourceFingerprint"],"ReferenceSourceFingerprint"->paper["SourceFingerprint"]|>;
out=FileNameJoin[{project,"Evidence","runs","paper-reproduction"}];If[!DirectoryQ[out],CreateDirectory[out,CreateIntermediateDirectories->True]];
report=Join[report,<|"LiveFile"->FileNameDrop[file,FileNameDepth[project]],"ReferenceFile"->FileNameDrop[paper["File"],FileNameDepth[project]]|>];
Export[FileNameJoin[{out,"exact-comparison.json"}],report,"RawJSON"];Print["ALL EQUAL ",report["AllCoefficientsEqual"]];Exit[If[TrueQ[report["AllCoefficientsEqual"]]&&TrueQ[report["FunctionSetsEqual"]],0,1]];

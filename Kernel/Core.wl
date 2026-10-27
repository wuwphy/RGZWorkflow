(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Configuration, late binding, timed stages, explicit data packets. *)
workflowFingerprint[] := Hash[{$RGZWorkflowVersion,
 ({FileNameDrop[# ,FileNameDepth[$packageRoot]],FileHash[#,"SHA256"]}& /@
  Sort[FileNames[{"*.wl","*.m"},FileNameJoin[{$packageRoot,"Kernel"}],Infinity]])},"SHA256"];
ready[] := Which[Length[$config]===0,Failure["InitializeFirst",<|"Action"->"RGZInitialize[workDirectory]"|>],Lookup[$config,"ModelFingerprint",None]=!=modelKey[],Failure["ModelChangedRestart",<|"Explanation"->"Model files changed after initialization; restart before continuing."|>],Lookup[$config,"WorkflowFingerprint",None]=!=workflowFingerprint[],Failure["WorkflowChangedRestart",<|"Action"->"Restart the kernel after changing package sources."|>],True,True];
backendDirectory[] := FileNameJoin[{$packageRoot,"Kernel","Backend"}];
pinchFormulaFingerprint[] := FileHash[FileNameJoin[{backendDirectory[],"RGZ_Pinch_Kernels.wl"}],"SHA256"];
dataDirectory[] := $config["DataDirectory"];
cacheDirectory[] := $config["CacheDirectory"];
modelBase[] := $config["ModelBase"];
quarkMasses[] := Lookup[$config,"QuarkMasses",{0,0}];
quarkMassRules[] := Thread[{RGZWorkflow`Engine`mQ1,RGZWorkflow`Engine`mQ2}->quarkMasses[]];
modelFingerprint[base_,masses_] := Hash[{"RGZWorkflowEngineContext/3",Import[base<>#, "Text"]& /@ {".fr",".gen",".mod"},masses},"SHA256"];
modelKey[] := modelFingerprint[modelBase[],quarkMasses[]];
masslessQQReady[] := If[AllTrue[quarkMasses[],TrueQ[#==0]&],True,
 Failure["MassiveQQNotImplemented",<|"QuarkMasses"->quarkMasses[],"Explanation"->"Masses are supported in the base model and closed quark bubbles. Full QQ scattering still requires massless external quarks; massive on-shell residues, kinematics and counterterms are not implemented.","Action"->"Use a fresh kernel and RGZInitialize[..., \"QuarkMasses\" -> {0,0}] for the current full QQ workflow."|>]];
(* Model content is declarative JSON in both source and FA model; it is
   covered by modelKey and never inferred from an old session result. *)
modelContent[] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[{markers,packets,texts,declared,content},
 texts=Import[modelBase[]<>#, "Text"]&/@{".fr",".mod"};
 markers=StringCases[#,
  (StartOfLine~~"(* RGZ-CONTENT "~~json:Shortest[__]~~" *)"~~EndOfLine):>json]&/@texts;
 If[!AllTrue[markers,Length[#]===1&],Return[Failure["ModelContentRequired",<||>]]];
 packets=Quiet[Check[ImportString[First[#],"RawJSON"]&/@markers,$Failed]];
 If[!ListQ[packets]||Length[packets]=!=2||!SameQ@@packets||Lookup[First[packets],"Schema",None]=!="RGZModelContent/1",Return[Failure["ModelContentMismatch",<||>]]];
 content=First[packets];
 If[!And@@(ListQ[Lookup[content,#,None]]&/@{"GhostClasses","QuarkClasses","ScalarGhostClasses"})||Lookup[content,"ClosedQuarkFlavors",None]=!=Length[content["QuarkClasses"]],Return[Failure["ModelContentMismatch",<||>]]];
 Do[declared=Sort[FromDigits/@StringCases[text,(StartOfLine~~WhitespaceCharacter...~~spec[[1]]~~"["~~id:DigitCharacter..~~"]"~~WhitespaceCharacter...~~"=="):>id]];
 If[declared=!=Sort[content[spec[[2]]]],Throw[Failure["ModelContentClassMismatch",<|"FieldType"->spec[[1]]|>],rgzFunctionReturnTag]],
 {text,texts},{spec,{{"U","GhostClasses"},{"F","QuarkClasses"}}}];
 Join[content,<|"QuarkMasses"->quarkMasses[]|>]],rgzFunctionReturnTag]];
$engineContext = "RGZWorkflow`Engine`";
(* Establish shared physical/index symbols before dependencies are loaded. *)
Scan[Symbol[$engineContext<>#]&, {"m","mG","M","s","p","q","p$",
 "mQ1","mQ2","eta","etabar","Q1","Q1bar","Q2","Q2bar","\[Mu]","\[Alpha]","\[Xi]","\[Tau]","\[Phi]","\[Beta]","\[Omega]","\[Zeta]",
 "ϕ","ϕbar","βbar","A","b","c","gs","aS","m02","mu2","mg2","M4","gamma2","Nc","rho",
 "a","d","e","f","g","i","j","u","v","g1","g2","h1","h2","QCD","xiGauge","Ah","LTotal",
 "Gluon","Gribov","Colour","Generation","Fadj","dSUN","SU3C","RGZAuxiliaryMass"}];
engineContextPath[] := DeleteDuplicates[Join[{$engineContext},Select[{"FeynCalc`","FeynArts`","FeynRules`","FeynHelpers`"},MemberQ[$Packages,#]&],{"System`"}]];
engineGet[name_String, once_:True] := Module[{path,r},
 If[TrueQ[once] && MemberQ[$loaded,name],Return[Null]];
 path=FileNameJoin[{backendDirectory[],name}];
 If[!FileExistsQ[path],Return[Failure["BackendMissing",<|"File"->name|>]]];
 r=engineEvaluation[Block[{$Output={}},
  CheckAbort[Check[With[{loaded=Get[path]},If[FailureQ[loaded]||loaded===$Failed||loaded===$Aborted,loaded,Null]],Failure["BackendLoadFailed",<|"File"->name|>]],Failure["BackendAborted",<|"File"->name|>]]]];
 If[r===Null,AppendTo[$loaded,name]];r
];
(* Dependency helpers use ToExpression at run time; isolate execution as well as loading.
   In FC sessions the current Engine context is sufficient for new symbols. Putting
   Engine on $ContextPath makes creation of a duplicate notebook name emit shdw even
   when Global is absent from the path. FR needs Engine first for shared model names. *)
SetAttributes[engineEvaluation,HoldAll];
engineEvaluation[expr_] := Block[{$ContextPath={"System`"}},
 Block[{$Context=$engineContext,$ContextPath=If[MemberQ[$Packages,"FeynRules`"],engineContextPath[],DeleteCases[engineContextPath[],$engineContext]]},expr]];
engineCode[code_String] := engineEvaluation[ToExpression[code,InputForm]];
fcReady[] := If[MemberQ[$Packages,"FeynCalc`"],True,Failure["FeynCalcRequired",<|"Action"->"RGZLoadFeynCalc[]"|>]];
frReady[] := If[MemberQ[$Packages,"FeynRules`"],True,Failure["FeynRulesRequired",<|"Action"->"RGZLoadFeynRules[...] in a fresh kernel"|>]];
SetAttributes[frEvaluation,HoldAll];
frEvaluation[expr_] := engineEvaluation[expr];
$sharedSymbolNames={"A","b","c","eta","Q1","Q2","\[Xi]","\[Tau]","\[Phi]","\[Beta]","\[Omega]","\[Zeta]",
 "aS","gs","\[Alpha]","m02","mu2","mg2","M4","gamma2","Nc","rho","m","mG","M","s","p","q","\[Mu]"};
symbolSnapshot[names_,context_] := AssociationMap[Function[name,With[{sym=Symbol[context<>name]},
 <|"Context"->Context[sym],"DefinitionHash"->Hash[{OwnValues[sym],DownValues[sym],UpValues[sym],SubValues[sym],Attributes[sym]},"SHA256"],
 "Resolved"->ToExpression[name,InputForm,Function[x,Context[Unevaluated[x]],HoldAll]],
 "ReplacementCheck"->SameQ[sym /. HoldPattern[sym]->RGZWorkflow`Private`rgzAuditMarker,RGZWorkflow`Private`rgzAuditMarker]|>]],names];
modelRulesHash[] := Hash[{FeynRules`M$Parameters,FeynRules`M$ClassesDescription,
 FeynRules`MR$Parameters,FeynRules`MR$ClassesDescription},"SHA256"];
RGZSymbolAudit[] := Module[{r,master,rulesHash,workers={}},
 r=ready[];If[FailureQ[r],Return[r]];r=frReady[];If[FailureQ[r],Return[r]];
 master=Block[{$Context=$engineContext,$ContextPath=engineContextPath[]},symbolSnapshot[$sharedSymbolNames,$engineContext]];
 rulesHash=modelRulesHash[];
 If[TrueQ[Global`FR$Parallelize] && Length[Kernels[]]>0,
  (* Send only held audit code, never recursively distribute model definitions:
     copying master physics state would conceal a worker mismatch. *)
  With[{names=$sharedSymbolNames,context=$engineContext,
    symbolCode=DownValues[symbolSnapshot],rulesCode=DownValues[modelRulesHash]},
   workers=ParallelEvaluate[
    Block[{$Context="RGZWorkflow`Engine`",$ContextPath={"RGZWorkflow`Engine`","FeynRules`","System`"}},
     DownValues[RGZWorkflow`Private`symbolSnapshot]=symbolCode;
     DownValues[RGZWorkflow`Private`modelRulesHash]=rulesCode;
     <|"KernelID"->$KernelID,"Symbols"->RGZWorkflow`Private`symbolSnapshot[names,context],
       "ModelRulesHash"->RGZWorkflow`Private`modelRulesHash[]|>],DistributedContexts->None]]];
 r=<|"Status"->If[AllTrue[Values[master],#["Context"]===$engineContext && #["Resolved"]===$engineContext && TrueQ[#["ReplacementCheck"]]&] &&
  AllTrue[workers,#["Symbols"]===master && #["ModelRulesHash"]===rulesHash&],"Passed","Failed"],"Master"->master,"ModelRulesHash"->rulesHash,"Workers"->workers|>;
 storeResult["SymbolAudit",r];r
];
validMixed[x_] := AssociationQ[x] && And@@(KeyExistsQ[x,#]& /@ {"T","L","FullColorTensorMatrix","TFieldOrder","LFieldOrder"}) &&
 Dimensions[x["T"]]==={5,5} && Dimensions[x["L"]]==={8,8} && Dimensions[x["FullColorTensorMatrix"]]==={8,8} &&
 !qqFFInvalidQ[{x["T"],x["L"],x["FullColorTensorMatrix"]}];
SetAttributes[timed,HoldRest];
timed[name_,expr_,seconds_] := Module[{time,r,state},
 If[!NumericQ[seconds] || !TrueQ[seconds>0],Return[Failure["TimeLimit",<||>]]];
 {time,r}=AbsoluteTiming[CheckAbort[TimeConstrained[expr,seconds,Failure["StageTimeout",<|"Stage"->name,"Seconds"->seconds|>]],Failure["StageAborted",<|"Stage"->name|>]]];
 (* Backends can return interruption sentinels without throwing an abort or
    reaching this wrapper's own time limit. Normalize them before recording. *)
 r=Which[r===$Failed,Failure["StageEvaluationFailed",<|"Stage"->name|>],
  r===$Aborted,Failure["StageAborted",<|"Stage"->name,"Reason"->"BackendReturnedAbort"|>],
  r===$TimedOut,Failure["StageTimeout",<|"Stage"->name,"Reason"->"BackendReturnedTimeout","StageLimitSeconds"->seconds|>],
  MatchQ[r,_TerminatedEvaluation],Failure["StageEvaluationTerminated",<|"Stage"->name,"Cause"->r|>],
  True,r];
 state=If[FailureQ[r],"Stopped","Complete"];
 AssociateTo[$stageStatus,name-><|"Status"->state,"Seconds"->time,"FailureTag"->If[FailureQ[r],r[[1]],None]|>];r
];
storeResult[name_,r_] := (If[!FailureQ[r] && r=!=$Failed && r=!=$Aborted,AssociateTo[$results,name->r]];r);
atomicWXF[file_,packet_] := Module[{temp=file<>"."<>CreateUUID[]<>".pending.wxf",r},
 Internal`WithLocalSettings[Null,
 r=Check[Export[temp,packet,"WXF"],$Failed];
 If[r===$Failed || !FileExistsQ[temp],Return[Failure["SaveFailed",<|"File"->file|>]]];
 r=Check[RenameFile[temp,file,OverwriteTarget->True],$Failed];
 If[r===$Failed,Failure["CommitFailed",<|"File"->file|>],<|"Status"->"Saved","File"->file|>],
 If[FileExistsQ[temp],Quiet[DeleteFile[temp]]]]
];
Options[RGZInitialize]={"ModelBase"->Automatic,"Display"->"Compact","QuarkMasses"->{0,0}};
RGZInitialize[directory_String,OptionsPattern[]] := Module[{root,base,dirs,missing,mode,fingerprint,masses,workflow,changed,created},
 root=ExpandFileName[directory];base=OptionValue["ModelBase"];
 If[base===Automatic,base=FileNameJoin[{$packageRoot,"Models","RGZ_W2.6"}],base=ExpandFileName[base]];
 missing=Select[{".fr",".gen",".mod"},!FileExistsQ[base<>#]&];
 If[missing=!={},Return[Failure["ModelFilesMissing",<|"ModelBase"->base,"Extensions"->missing|>]]];
 masses=OptionValue["QuarkMasses"];
 If[!MatchQ[masses,{_,_}]||!AllTrue[masses,(NumberQ[#]&&TrueQ[Im[#]==0]&&TrueQ[#>=0])||(MatchQ[#,_Symbol]&&!MemberQ[{True,False,Null,Indeterminate,Infinity},#])&],Return[Failure["QuarkMasses",<|"Explanation"->"Specify two nonnegative real masses or unassigned symbolic masses, in flavor order."|>]]];
 fingerprint=modelFingerprint[base,masses];
 mode=OptionValue["Display"];If[!MemberQ[{"Compact","Quiet","Verbose"},mode],Return[Failure["DisplayMode",<||>]]];
 dirs=FileNameJoin[{root,#}]& /@ {"Data","Cache","Results"};workflow=workflowFingerprint[];
 changed=Length[$config]>0 && ($config["WorkDirectory"]=!=root || $config["ModelBase"]=!=base || Lookup[$config,"ModelFingerprint",None]=!=fingerprint || Lookup[$config,"WorkflowFingerprint",None]=!=workflow);
 If[changed && (Length[$loaded]>0 || MemberQ[$Packages,"FeynRules`"] || MemberQ[$Packages,"FeynCalc`"]),
  Return[Failure["FreshKernelForNewProject",<|"Explanation"->"Loaded engine state belongs to the current project. Restart before switching projects."|>]]];
 created=Quiet[Check[Scan[If[!DirectoryQ[#],CreateDirectory[#,CreateIntermediateDirectories->True]]&,dirs];And@@(DirectoryQ/@dirs),False]];
 If[!TrueQ[created],Return[Failure["WorkDirectoryUnavailable",<|"WorkDirectory"->root|>]]];
 If[changed,$results=<||>;$stageStatus=<||>;
  Clear[RGZWorkflow`Engine`rgzMixed,RGZWorkflow`Engine`rgzQCDReport,RGZWorkflow`Engine`rgzPTJob]];
 $config=<|"Version"->$RGZWorkflowVersion,"WorkflowFingerprint"->workflow,"WorkDirectory"->root,"DataDirectory"->dirs[[1]],"CacheDirectory"->dirs[[2]],"ResultsDirectory"->dirs[[3]],"ModelBase"->base,"QuarkMasses"->masses,"ModelFingerprint"->fingerprint,"Display"->mode|>;
 RGZWorkflow`Engine`modelRGZ=base;
 <|"Status"->"Initialized","WorkDirectory"->root,"ModelBase"->base,"NoCalculationsStarted"->True|>
];
RGZConfiguration[] := $config;
RGZSymbols[] := <|"mQ1"->RGZWorkflow`Engine`mQ1,"mQ2"->RGZWorkflow`Engine`mQ2,"m"->RGZWorkflow`Engine`m,"mG"->RGZWorkflow`Engine`mG,"M"->RGZWorkflow`Engine`M,
 "mu"->RGZWorkflow`Engine`\[Mu],"alpha"->RGZWorkflow`Engine`\[Alpha],"s"->RGZWorkflow`Engine`s,
 "p"->RGZWorkflow`Engine`p,"q"->RGZWorkflow`Engine`q|>;
RGZDependencies[] := <|"ModelContent"->If[Length[$config]>0,modelContent[],Missing["InitializeFirst"]],"WolframVersion"->$Version,"FeynRulesLoaded"->MemberQ[$Packages,"FeynRules`"],"FeynCalcLoaded"->MemberQ[$Packages,"FeynCalc`"],
 "FeynRulesFile"->Quiet[Check[FindFile["FeynRules`"],Missing["NotOnPath"]]],"FeynCalcFile"->Quiet[Check[FindFile["FeynCalc`"],Missing["NotOnPath"]]]|>;
Options[RGZLoadFeynRules]={"Directory"->Automatic};
RGZLoadFeynRules[OptionsPattern[]] := Module[{r,path,old,audit,env,entry},
 r=ready[];If[FailureQ[r],Return[r]];
 If[MemberQ[$Packages,"FeynCalc`"],Return[Failure["FreshKernelRequired",<|"Explanation"->"Use separate kernels for FeynRules and FeynCalc."|>]]];
 path=OptionValue["Directory"];
 If[path===Automatic,env=Environment["RGZ_FEYNRULES_DIRECTORY"];If[StringQ[env]&&StringLength[env]>0,path=env]];
 If[path===Automatic,
  entry=Quiet[Check[FindFile["FeynRules`"],$Failed]];
  If[!StringQ[entry]||!FileExistsQ[entry],Return[Failure["FeynRulesPath",<|"Action"->"Supply Directory -> the external FeynRules installation or set RGZ_FEYNRULES_DIRECTORY."|>]]];
  path=DirectoryName[entry]];
 If[!StringQ[path],Return[Failure["FeynRulesPath",<|"Directory"->path|>]]];
 path=ExpandFileName[path];
 If[!And@@(FileExistsQ[FileNameJoin[{path,#}]]&/@{"FeynRules.m","FeynRulesPackage.m"}),Return[Failure["FeynRulesPath",<|"Directory"->path|>]]];
 Global`$FeynRulesPath=path;old=Directory[];
 r=Internal`WithLocalSettings[Null,
  Block[{$Context="Global`",$ContextPath={"System`"}},CheckAbort[Check[SetDirectory[path];Get["FeynRules.m"],$Failed],$Aborted]],
  SetDirectory[old]];
 If[r===$Failed||r===$Aborted||!MemberQ[$Packages,"FeynRules`"],Return[Failure["LoadFeynRulesFailed",<||>]]];
  $ContextPath=DeleteDuplicates[Join[$ContextPath,{"FeynRules`"}]];
 r=timed["LoadFRModel",frEvaluation[engineCode["LoadModel[RGZWorkflow`Private`modelBase[] <> \".fr\"];"]],120];
 If[FailureQ[r],Return[r]];
 If[engineCode["Length[DownValues[LTotal]]+Length[OwnValues[LTotal]]"]===0,Return[Failure["FRModelDefinitionsMissing",<||>]]];
 If[TrueQ[Global`FR$Parallelize] && Length[Kernels[]]>0,
  ParallelEvaluate[$Context="RGZWorkflow`Engine`";$ContextPath={"RGZWorkflow`Engine`","FeynRules`","System`"}]];
 audit=RGZSymbolAudit[];
 If[!AssociationQ[audit] || audit["Status"]=!="Passed",Return[Failure["FRSymbolContextMismatch",<|"Action"->"RGZSymbolAudit[]","Explanation"->"Master/worker model symbols or definitions differ."|>]]];
 <|"Status"->"FRModelLoaded","ModelBase"->modelBase[],"SymbolAudit"->"Passed"|>
];
RGZLoadFeynCalc[] := Module[{r},
 r=ready[];If[FailureQ[r],Return[r]];
 If[MemberQ[$Packages,"FeynRules`"],Return[Failure["FreshKernelRequired",<|"Explanation"->"Export FR data, restart the kernel, then load FeynCalc."|>]]];
 If[!MemberQ[$Packages,"FeynCalc`"],
  (* FC migrates and removes these Global startup options. Keep their names
     in strings so re-reading Core after FC loads cannot recreate them. *)
  Block[{$ContextPath={"System`"}},ToExpression["Global`$LoadAddOns={\"FeynArts\"};Global`$FeynCalcStartupMessages=False;"]];
  r=Block[{$Context="Global`",$ContextPath={"System`"}},Check[Needs["FeynCalc`"],$Failed,{Get::noopen,Needs::nocont}]];
  If[r===$Failed,Return[Failure["LoadFeynCalcFailed",<||>]]]];
  $ContextPath=DeleteDuplicates[Join[$ContextPath,Select[{"FeynCalc`","FeynArts`"},MemberQ[$Packages,#]&]]];
 If[!MemberQ[$Packages,"FeynCalc`"] || engineCode["Length[DownValues[FCFAConvert]]"]===0,
  Return[Failure["FeynArtsAddonRequired",<|"Explanation"->"Load FeynCalc with $LoadAddOns={\"FeynArts\"} in a fresh kernel."|>]]];
 RGZWorkflow`Engine`modelRGZ=modelBase[];FeynArts`$FAVerbose=0;
 <|"Status"->"FeynCalcLoaded","ModelBase"->modelBase[]|>
];
RGZResult[name_String] := Lookup[$results,name,Missing["NotComputed",name]];
RGZStatus[] := $stageStatus;
RGZShow[x_,max_:1500] := If[LeafCount[x]<=max,Print[TraditionalForm[x]],Print["Expression too large; LeafCount = ",LeafCount[x]]];
reportFailure[x_] := Module[{f=x,n=0,detail},
 While[FailureQ[f] && n<3,
  Print["Stopped: ",f[[1]]];detail=f[[2]];
  Print[KeyTake[detail,{"Stage","Seconds","Reason","Progress","File","Action","Parts","Explanation","ChecksPassed","ID","StoppedOperation","Description","CauseTag","File","TimeLimit","ElapsedSeconds","underlyingMessages"}]];
  f=Lookup[detail,"Cause",Lookup[detail,"Cause",None]];n++]];
RGZReport[x_] := Which[FailureQ[x],reportFailure[x],
 AssociationQ[x],Print[KeyTake[x,{"Status","Name","Dimensions","Rows","Statistics","File","LoopOrder","Mode","ReferenceGauge","PTStatus","InvalidatedResults","KernelVersion","CalculationScope","FullNAVIncluded","Status","Statistics","Description"}]],True,Print["Result head: ",Head[x]]];
SetAttributes[RGZMonitor,HoldAll];
RGZMonitor[x_] := If[Length[DownValues[RGZWorkflow`Engine`RGZMixMonitor]]>0,RGZWorkflow`Engine`RGZMixMonitor[x],x];
RGZRegisterStage[name_String,f_] := (AssociateTo[$extensions,name->f];<|"Status"->"Registered","Name"->name|>);
RGZRunExtension[name_String,args___] := Module[{f=Lookup[$extensions,name,Missing[]]},
 If[Head[f]===Missing,Failure["UnknownExtension",<|"Name"->name|>],f[args]]];
Options[RGZSave]={"File"->Automatic};
RGZSave[OptionsPattern[]] := Module[{r,file,packet,pt},
 r=ready[];If[FailureQ[r],Return[r]];file=OptionValue["File"];
 If[file===Automatic,file=FileNameJoin[{$config["ResultsDirectory"],"Session.wxf"}]];
 pt=If[AssociationQ[RGZWorkflow`Engine`rgzPTJob],RGZWorkflow`Engine`rgzPTJob,Missing["NotPrepared"]];
 packet=<|"Schema"->"RGZWorkflowSession/2","Version"->$RGZWorkflowVersion,"WorkflowFingerprint"->workflowFingerprint[],"ModelFingerprint"->modelKey[],"Configuration"->$config,"Results"->$results,"StageStatus"->$stageStatus,"PTJob"->pt|>;
 atomicWXF[file,packet]
];
Options[RGZRestore]={"File"->Automatic};
RGZRestore[OptionsPattern[]] := Module[{r,file,packet,pt,compatible,relocated,dropped={}},
 r=ready[];If[FailureQ[r],Return[r]];file=OptionValue["File"];
 If[file===Automatic,file=FileNameJoin[{$config["ResultsDirectory"],"Session.wxf"}]];
 If[!FileExistsQ[file],Return[Failure["SessionFileMissing",<|"File"->file|>]]];
 packet=Check[Import[file,"WXF"],$Failed];
 If[!AssociationQ[packet]||Lookup[packet,"Schema",None]=!="RGZWorkflowSession/2",Return[Failure["SessionSchema",<||>]]];
 If[Lookup[packet,"Version",None]=!=$RGZWorkflowVersion,Return[Failure["SessionVersion",<||>]]];
 If[Lookup[packet,"ModelFingerprint",None]=!=modelKey[],Return[Failure["DifferentModel",<|"Action"->"Restart with the current model, regenerate FR data, and recompute mixed matrices and amplitudes; keep the old Session as historical data.","Explanation"->"The saved session has a different physical model, including its ghost/quark content."|>]]];
 If[Lookup[packet,"WorkflowFingerprint",None]=!=workflowFingerprint[],Return[Failure["SessionSourceMismatch",<|"Action"->"Recompute with the current package sources; retain this session as historical data."|>]]];
 If[!AssociationQ[Lookup[packet,"Results",None]]||!AllTrue[Keys[packet["Results"]],StringQ],Return[Failure["InvalidSessionResults",<||>]]];
 If[!AssociationQ[Lookup[packet,"Configuration",None]] || !AssociationQ[Lookup[packet,"StageStatus",<||>]],Return[Failure["InvalidSessionMetadata",<||>]]];
 If[!AllTrue[Keys[Lookup[packet,"StageStatus",<||>]],StringQ]||
 !AllTrue[Values[Lookup[packet,"StageStatus",<||>]],AssociationQ[#]&&MemberQ[{"Complete","Stopped"},Lookup[#,"Status",None]]&],
 Return[Failure["InvalidSessionMetadata",<||>]]];
 If[KeyExistsQ[packet["Results"],"Mixed"] && !validMixed[packet["Results"]["Mixed"]],Return[Failure["InvalidSessionMatrices",<||>]]];
 If[Lookup[packet["Configuration"],"WorkDirectory",None]===$config["WorkDirectory"]&&KeyExistsQ[packet["Results"],"QQ"]&&
 !TrueQ[qqSessionJobValidQ[packet["Results"]["QQ"]]],Return[Failure["InvalidSessionQQJob",<|"Action"->"Restore matching vertex exports and a complete QQ job, or start a fresh task."|>]]];
 If[Lookup[packet["Configuration"],"WorkDirectory",None]===$config["WorkDirectory"]&&KeyExistsQ[packet["Results"],"QQBeforeGauge"]&&
 !TrueQ[qqGaugeBackupValidQ[packet["Results"]["QQBeforeGauge"]]],
 Return[Failure["InvalidSessionQQBackup",<|"Action"->"Restore a session containing a complete original QQ task, or start a fresh task."|>]]];
 $results=packet["Results"];$stageStatus=Lookup[packet,"StageStatus",<||>];
 relocated=Lookup[packet["Configuration"],"WorkDirectory",None]=!=$config["WorkDirectory"];
 If[relocated,
  dropped=Select[Keys[$results],StringStartsQ[#,"QQ"]&];
  $results=KeyDrop[$results,dropped];
  $stageStatus=KeyDrop[$stageStatus,Select[Keys[$stageStatus],StringStartsQ[#,"QQ"]&]]];
 pt=Lookup[packet,"PTJob",None];
 compatible=!relocated && TrueQ[ptJobPacketQ[pt]] && Lookup[pt,"KernelVersion",None]==="RGZFourPinch/2" &&
   Lookup[pt,"FormulaFingerprint",None]===pinchFormulaFingerprint[] &&
   (Lookup[pt,"NAVMode",None]=!="FullMatrix" || Lookup[pt,"NAVFingerprint",None]===navFormulaFingerprint[]);
 If[!compatible,
  dropped=Union[dropped,Select[Keys[$results],MemberQ[{"Gauge","GaugePartial","GaugeStopped","Pinch"},#] || StringStartsQ[#,"MassLimit:"]&]];
  $results=KeyDrop[$results,dropped];$stageStatus=KeyDrop[$stageStatus,{"Gauge","Pinch"}]];
 Clear[RGZWorkflow`Engine`rgzMixed,RGZWorkflow`Engine`rgzQCDReport];
 If[KeyExistsQ[$results,"Mixed"] && validMixed[$results["Mixed"]],RGZWorkflow`Engine`rgzMixed=$results["Mixed"]];
 If[KeyExistsQ[$results,"QCD"],RGZWorkflow`Engine`rgzQCDReport=$results["QCD"]];
 Clear[RGZWorkflow`Engine`rgzPTJob];
 If[compatible && Lookup[packet["Configuration"],"WorkDirectory",None]===$config["WorkDirectory"],RGZWorkflow`Engine`rgzPTJob=pt];
 <|"Status"->"Restored","Names"->Keys[$results],"File"->file,
   "PTStatus"->If[compatible,"RestoredOrRelocated",If[AssociationQ[pt],"RestartRequired","NotPrepared"]],"InvalidatedResults"->dropped|>
];

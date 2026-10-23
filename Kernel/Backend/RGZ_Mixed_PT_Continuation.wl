(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* RGZ mixed self-energy + four CANDIDATE pinch kernels.
   Load FeynCalc/FeynArts, RGZ_Mixed_OneLoop_Overall.wl,
   RGZ_Mixed_Scalar_Accelerator.wl and RGZ_Mixed_Gauge_Check.wl first.
   This computes and checks the specified ansatz; it does NOT derive
   the full RGZ quark-scattering Ward/ST identities or omitted graphs.
   Use the successful QCD report from RGZ_QCD_Check_Clean.wl.
   Preserve D, m, mu, mG and M; no M->0 limit of individual PV functions.
   No gauge-fitted normalization, extra graph multiplicity or loop measure.
   A success proves only the checked candidate scalar residual is zero.
*)

ClearAll[RGZPTLegacyStart,RGZPTCheckQCD,RGZPTProject,RGZPTColour,
  RGZPTRun,RGZPTComputePart,RGZPTComputeAll,RGZPTCompare,
  RGZPTContinueAll,
  RGZPTStatus,RGZPTSummary,RGZPTResult,RGZPTSave,
  RGZPTProgress,RGZPTGood,RGZPTSetPart,RGZPTCommonSelf,RGZPTJobCheck,RGZPTKernelSignature];

RGZPTKernelSignature[] := Hash[{"RGZFourPinch/2",DownValues[RGZBuildPinches],
  Options[RGZBuildPinches]},"SHA256"];
RGZPTJobCheck[] := If[!AssociationQ[rgzPTJob],Failure["PTStartFirst",<||>],
  If[Lookup[rgzPTJob,"KernelVersion",None]=!="RGZFourPinch/2" ||
      Lookup[rgzPTJob,"FormulaFingerprint",None]=!=RGZWorkflow`Private`pinchFormulaFingerprint[] ||
      Lookup[rgzPTJob,"KernelSignature",None]=!=RGZPTKernelSignature[] ||
      (Lookup[rgzPTJob,"NAVMode",None]==="FullMatrix" && Lookup[rgzPTJob,"NAVFingerprint",None]=!=RGZWorkflow`Private`navFormulaFingerprint[]),
    Failure["StalePTJob",<|"Action"->"RGZPTStart[]","说明"->"计算公式已更新；重新初始化四图任务，保留旧缓存作为历史结果。"|>],True]];

RGZPTProgress[name_,stage_] := RGZMXProgressUpdate[
  "\:90e8\:95e8"->("RGZ pinch \:5019\:9009\:9879\:ff1a"<>name),"\:9636\:6bb5"->stage,
  "\:5b50\:6b65\:9aa4"->"\:51c6\:5907","\:56fe\:53f7"->0,"\:56fe\:6570"->0,
  "\:9879\:53f7"->0,"\:603b\:9879\:6570"->0,"\:5b8c\:6210\:9879\:6570"->0,"\:72b6\:6001"->"\:8ba1\:7b97\:4e2d",
  "\:9636\:6bb5\:5f00\:59cb\:65f6\:95f4"->AbsoluteTime[]];

(* A FIXED convention, previously checked with actual FeynArts gse.
   QCD D0=-I/s, D0^2=-1/s^2. In the paper's raw pinch-kernel convention,
   the mixed self contribution is -(D0.Pi.D0)AA. Therefore its QCD limit
   is +Pi/s^2, exactly the convention used to amputate the four pinch
   kernels by s^2 in RGZ_QCD_Stepwise_Check.wl. This minus is not chosen
   by fitting finite RGZ residuals. The common overall factor is 1.
   A comparison in the original connected-propagator convention instead
   multiplies the WHOLE resulting scalar by -1. Zero tests are identical. *)
RGZPTCommonSelf[work_Association] :=
  RGZPTColour /@ ((-work["\:6811\:7ea7\:6a2a\:5411AA"]^2 #)& /@ work["\:81ea\:80fdT\:5206\:9879"]);

RGZPTCheckQCD[report_:Automatic] := Module[
  {r=report,names,rows,states,total,gse,c,diff},
  If[r===Automatic,r=rgzQCDReport];
  If[!AssociationQ[r],Return[Failure["QCDReportRequired",<|
    "\:8bf4\:660e"->"\:5148\:8fd0\:884c RGZ_QCD_Check_Clean.wl\:ff0c\:5f97\:5230 rgzQCDReport\:3002"|>]]];
  names={"box","nav\:5168\:90e8","av\:5168\:90e8","qse\:5168\:90e8","gse"};
  rows=Lookup[r,"\:9010\:9879\:7ed3\:679c",{}];
  If[!ListQ[rows] || Length[rows]=!=5 || !And@@(AssociationQ/@rows) ||
      Sort[Lookup[rows,"\:9879\:76ee",Missing[]]]=!=Sort[names],
    Return[Failure["IncompleteQCDBenchmark",<||>]]];
  states=Lookup[rows,"\:72b6\:6001",Missing[]];
  If[!And@@(MemberQ[{"\:79ef\:5206\:524d\:540e\:5747\:901a\:8fc7","\:79ef\:5206\:540e\:901a\:8fc7\:ff1b\:79ef\:5206\:524d\:5f62\:5f0f\:4e0d\:540c"},#]&/@states),
    Return[Failure["QCDBenchmarkNotPassed",<||>]]];
  total=Lookup[r,"\:603b\:548c\:68c0\:67e5",<||>];
  If[!AssociationQ[total] || Lookup[total,"\:62b5\:6d88\:68c0\:67e5",""]=!="\:901a\:8fc7" ||
      !TrueQ[Lookup[total,"\:8ba1\:7b97\:7684\:4e94\:7c7b\:89c4\:8303\:5dee\:603b\:548c / I11",Missing[]]===0],
    Return[Failure["QCDTotalNotZero",<||>]]];
  (* Clean driver calls RGZQCDGSEStart[] with its fixed default 1.
     If the live session exists, additionally verify that it used 1. *)
  If[AssociationQ[rgzQCDGSESession] &&
      Lookup[rgzQCDGSESession,"\:7edf\:4e00\:56e0\:5b50",Missing[]]=!=1,
    Return[Failure["DifferentQCDConvention",<|
      "\:8bf4\:660e"->"\:6b64\:6d41\:7a0b\:91c7\:7528\:5df2\:7ecf\:68c0\:67e5\:7684\:56e0\:5b50 1\:ff1b\:4e0d\:81ea\:52a8\:4fee\:6539\:76f8\:5bf9\:56e0\:5b50\:3002"|>]]];
  gse=First[Select[rows,#["\:9879\:76ee"]==="gse"&]];
  c=Lookup[gse,"\:6587\:732e\:7684I11\:7cfb\:6570",Missing[]];
  If[!FreeQ[c,_Missing|_Failure] || TrueQ[c===0],
    Return[Failure["QCDGSECoefficientRequired",<||>]]];
  diff=TimeConstrained[Cancel[Together[
    Lookup[gse,"\:8ba1\:7b97\:7684I11\:7cfb\:6570",Missing[]]-c]],15,Missing["QCDCheckTimeout"]];
  If[!TrueQ[diff===0],Return[Failure["QCDGSEConventionNotVerified",<|
    "\:8bf4\:660e"->"\:5b9e\:9645 gse \:4e0e\:6587\:732e\:4e3b\:79ef\:5206\:7cfb\:6570\:5c1a\:672a\:6838\:5bf9\:4e3a\:76f8\:7b49\:3002"|>]]];
  <|"\:72b6\:6001"->"\:901a\:8fc7","\:5171\:540c\:56e0\:5b50"->1,
    "\:81ea\:80fd\:7ea6\:5b9a"->"-(D0.Pi.D0)AA","\:91cd\:6570"->{"box"->1,
      "nav\:5168\:90e8"->2,"av\:5168\:90e8"->2,"qse\:5168\:90e8"->4}|>
];

(* Candidate kernels contain colour Casimirs rather than external deltas.
   Mixed matrices already have their external adjoint delta stripped.
   Use the same SU(N) invariants, not a new numeric Nc substitution. *)
(* The model's bare N in mG^2/Sqrt[N] is the same colour
   dimension as SUNN. Do not replace the head of numerical N[...]. *)
RGZPTColour[x_] := Replace[RGZLoopNormalize[x],
  HoldPattern[System`N]->SUNN,{0,Infinity},Heads->False] /.
  {CA->SUNN,CF->(SUNN^2-1)/(2 SUNN)};

RGZPTProject[x_] := Module[{z},
  RGZMXProgressUpdate["\:5b50\:6b65\:9aa4"->"\:6a2a\:5411\:6295\:5f71\:5e76\:7edf\:4e00 Casimir \:7ea6\:5b9a"];
  z=Contract[FCI[RGZLoopPT[p,\[Mu],\[Nu]]] FCI[x]]/(D-1);
  z=RGZPTColour[ExpandScalarProduct[z]];
  If[!FreeQ[z,_LorentzIndex|_ExplicitLorentzIndex|_SUNIndex|_SUNF|_SUNDelta],
    Failure["PinchProjectionIncomplete",<||>],z]
];

RGZPTGood[stage_,x_] := !FailureQ[x] &&
  FreeQ[x,_Missing|$Failed|$Aborted|Indeterminate|_DirectedInfinity] &&
  If[MemberQ[{"\:79ef\:5206","\:6807\:91cf"},stage],
    FreeQ[x,_TID|_PaVeReduce|_FeynAmpDenominator],True] &&
  If[stage==="\:6807\:91cf",
    FreeQ[x,_LorentzIndex|_ExplicitLorentzIndex] &&
      Cases[x,z_ /; RGZMXPVObjectQ[z] && !RGZMXPVScalarQ[z],{0,Infinity}]==={},True];

Options[RGZPTLegacyStart]={"\:7f13\:5b58\:76ee\:5f55"->Automatic,"QCD\:62a5\:544a"->Automatic,"Mode"->"FourPinch","ReferenceGauge"->0,"NAVPacket"->None};
RGZPTLegacyStart[data_,OptionsPattern[]] := Module[
  {required,missing,work,qcd,pinches,names,context,fingerprint,root,dir,parts,treecheck,nav=OptionValue["NAVPacket"],previous=rgzPTJob},
  required={RGZGaugeStart,RGZGaugeCompare,RGZCandidatePinches,
    RGZMXIntegrate,RGZMXScalarReduce,RGZMXFastScalar,RGZMXWrite};
  missing=Select[required,DownValues[#]==={}&];
  If[missing=!={},Return[Failure["LoadPTDependencies",<|
    "\:8bf4\:660e"->"\:5148\:52a0\:8f7d\:6df7\:5408\:77e9\:9635\:6d41\:7a0b\:3001\:6807\:91cf\:52a0\:901f\:8865\:4e01\:548c\:89c4\:8303\:68c0\:67e5\:6587\:4ef6\:3002"|>]]];
  If[Total[Length/@{OwnValues[m],OwnValues[mG],OwnValues[M],
      OwnValues[\[Mu]],OwnValues[\[Alpha]]}]>0,
    Return[Failure["SymbolicParametersRequired",<|
      "\:8bf4\:660e"->"m\:3001mG\:3001M\:3001\:03bc\:3001\:03b1 \:5fc5\:987b\:4fdd\:6301\:7b26\:53f7\:ff1b\:672c\:6d41\:7a0b\:4e0d\:4f1a\:6e05\:9664\:4f60\:7684\:8d4b\:503c\:3002"|>]]];
  RGZPTProgress["\:521d\:59cb\:5316","\:68c0\:67e5\:8f93\:5165\:548c QCD \:7ea6\:5b9a"];
  qcd=RGZPTCheckQCD[OptionValue["QCD\:62a5\:544a"]];If[FailureQ[qcd],Return[qcd]];
  RGZMXProgressUpdate["\:5b50\:6b65\:9aa4"->"\:8bfb\:53d6\:6df7\:5408\:77e9\:9635\:5e76\:8fde\:63a5\:5916\:90e8 A \:4f20\:64ad\:5b50"];
  work=RGZGaugeStart[data];If[FailureQ[work],Return[work]];
  treecheck=Cancel[Together[(work["\:6811\:7ea7\:6a2a\:5411AA"] /.
    {m->0,mG->0,\[Mu]->0})+I/SPD[p,p]]];
  If[!TrueQ[treecheck===0],Return[Failure["UnexpectedTreeConvention",<||>]]];
  names={"box","nav\:5168\:90e8","av\:5168\:90e8","qse\:5168\:90e8"};
  RGZMXProgressUpdate["\:5b50\:6b65\:9aa4"->"\:6784\:9020\:56db\:7c7b\:5019\:9009\:9879\:5e76\:68c0\:67e5\:7f13\:5b58\:8eab\:4efd"];
  pinches=RGZCandidatePinches["Mode"->OptionValue["Mode"],"ReferenceGauge"->OptionValue["ReferenceGauge"]];
  If[FailureQ[pinches],Return[pinches]];
  If[!AssociationQ[pinches] || !And@@(KeyExistsQ[pinches,#]&/@names),
    Return[Failure["CandidatePinchesRequired",<||>]]];
  If[AssociationQ[nav],If[pinches["Mode"]=!="FourPinch",Return[Failure["FullNAVRequiresFourPinch",<||>]]];AssociateTo[pinches,"nav\:5168\:90e8"->nav["Kernel"]]];
  context={"RGZ-PT-FOUR-PINCH-v2",RGZPTKernelSignature[],pinches["Mode"],pinches["ReferenceGauge"],$VersionNumber,$FeynCalcVersion,
    RGZMXIntegralContext[],DownValues[RGZCandidatePinches],
    DownValues[RGZPTProject],DownValues[RGZPTColour],
    DownValues[RGZMXFastScalar],DownValues[RGZMXFastRational],
    DownValues[RGZMXFastApart],DownValues[RGZMXFastTerm],
    DownValues[RGZMXFastTadpole],DownValues[RGZMXFastMass]};
  fingerprint=Hash[{context,If[AssociationQ[nav],nav["Fingerprint"],None]},"SHA256"];
  root=OptionValue["\:7f13\:5b58\:76ee\:5f55"];
  If[root===Automatic,root=RGZWorkflow`Private`cacheDirectory[]];
  If[!StringQ[root],Return[Failure["SaveNotebookFirst",<||>]]];
  dir=FileNameJoin[{root,"RGZ_PT_candidate_cache_"<>
    StringTake[IntegerString[fingerprint,16],12]}];
  If[!DirectoryQ[dir],CreateDirectory[dir,CreateIntermediateDirectories->True]];
  parts=Association@@Table[name-><|"\:539f\:5f0f"->pinches[name],"\:7ed3\:679c"-><||>|>,{name,names}];
  If[AssociationQ[nav],AssociateTo[parts,"nav\:5168\:90e8"->Join[parts["nav\:5168\:90e8"],<|"Representation"->"Rational"|>]]];
  If[AssociationQ[previous]&&TrueQ[RGZPTJobCheck[]===True],
   Do[If[name=!="nav\:5168\:90e8"&&SameQ[Lookup[previous["\:9879\:76ee"][name],"\:539f\:5f0f",None],parts[name]["\:539f\:5f0f"]],
    AssociateTo[parts,name->previous["\:9879\:76ee"][name]]],{name,names}]];
  rgzPTJob=<|"\:76ee\:5f55"->dir,"\:73af\:5883"->fingerprint,"\:9879\:76ee\:987a\:5e8f"->names,
    "\:9879\:76ee"->parts,"\:81ea\:80fd\:5206\:9879"->RGZPTCommonSelf[work],
    "NAVMode"->If[AssociationQ[nav],"FullMatrix","AAReplacement"],"NAVFingerprint"->If[AssociationQ[nav],nav["Fingerprint"],None],
    "KernelVersion"->"RGZFourPinch/2","KernelSignature"->RGZPTKernelSignature[],
    "FormulaFingerprint"->RGZWorkflow`Private`pinchFormulaFingerprint[],
    "Mode"->pinches["Mode"],"ReferenceGauge"->pinches["ReferenceGauge"],
    "QCD\:7ea6\:5b9a"->qcd,"\:6bd4\:8f83"->Missing["NotCompared"]|>;
  RGZMXProgressUpdate["\:72b6\:6001"->"\:5b8c\:6210"];
  <|"\:72b6\:6001"->"\:51c6\:5907\:5b8c\:6210","FullNAVIncluded"->AssociationQ[nav],"\:9879\:76ee\:6570"->4,"Mode"->pinches["Mode"],"ReferenceGauge"->pinches["ReferenceGauge"],"\:7f13\:5b58\:76ee\:5f55"->dir,
    "\:8bf4\:660e"->"\:5c06\:590d\:7528 pinch \:7f13\:5b58\:ff1b\:539f\:6df7\:5408\:77e9\:9635\:7f13\:5b58\:4e0d\:6539\:52a8\:3002"|>
];

RGZPTSetPart[name_,part_] := Module[{parts=rgzPTJob["\:9879\:76ee"]},
  AssociateTo[parts,name->part];AssociateTo[rgzPTJob,"\:9879\:76ee"->parts];Null];

RGZPTRun[name_String,stage_String,seconds_:1200] := Module[
  {stages={"\:6295\:5f71","\:5206\:89e3","\:79ef\:5206","\:6807\:91cf"},part,res,n,input,path,old,z,time,
    record,r,key},
  With[{check=RGZPTJobCheck[]},If[FailureQ[check],Return[check]]];
  If[!MemberQ[rgzPTJob["\:9879\:76ee\:987a\:5e8f"],name] || !MemberQ[stages,stage] ||
      !NumericQ[seconds] || !TrueQ[seconds>0],Return[Failure["PTStageArguments",<||>]]];
  part=rgzPTJob["\:9879\:76ee"][name];res=part["\:7ed3\:679c"];n=First@FirstPosition[stages,stage];
  If[KeyExistsQ[res,stage] && RGZPTGood[stage,res[stage]],
    Return[<|"\:9879\:76ee"->name,"\:9636\:6bb5"->stage,"\:72b6\:6001"->"\:5df2\:5b8c\:6210\:ff0c\:8df3\:8fc7"|>]];
  If[n>1 && !KeyExistsQ[res,stages[[n-1]]],
    Return[Failure["PTRunPreviousStage",<|"\:9879\:76ee"->name,
      "\:7f3a\:5c11\:9636\:6bb5"->stages[[n-1]]|>]]];
  input=If[n===1,part["\:539f\:5f0f"],res[stages[[n-1]]]];
  key=ToString[First@FirstPosition[rgzPTJob["\:9879\:76ee\:987a\:5e8f"],name]];
  path=FileNameJoin[{rgzPTJob["\:76ee\:5f55"],"part_"<>key<>"_"<>ToString[n]<>".wl"}];
  RGZPTProgress[name,stage];
  If[FileExistsQ[path],old=Get[path];
    If[AssociationQ[old] && Lookup[old,"\:72b6\:6001",""]==="\:5b8c\:6210" &&
        SameQ[Lookup[old,"\:8f93\:5165",None],input] &&
        SameQ[Lookup[old,"\:73af\:5883",None],rgzPTJob["\:73af\:5883"]] &&
        RGZPTGood[stage,Lookup[old,"\:7ed3\:679c",Missing[]]],
      AssociateTo[res,stage->old["\:7ed3\:679c"]];AssociateTo[part,"\:7ed3\:679c"->res];
      RGZPTSetPart[name,part];RGZMXProgressUpdate["\:72b6\:6001"->"\:7f13\:5b58\:6062\:590d"];
      Return[<|"\:9879\:76ee"->name,"\:9636\:6bb5"->stage,"\:72b6\:6001"->"\:7f13\:5b58\:6062\:590d"|>]]];
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    Switch[stage,
      "\:6295\:5f71",RGZPTProject[input],
      "\:5206\:89e3",(* Candidate expressions are ALREADY simple-pole FADs.
                 Distribute products and combine their FAD factors only. *)
        RGZMXProgressUpdate["\:5b50\:6b65\:9aa4"->"\:5c55\:5f00\:6781\:70b9\:5206\:9879\:5e76\:5408\:5e76 FAD \:56e0\:5b50"];
        If[Lookup[part,"Representation",None]==="Rational",rgzNAVSplit[input],FeynAmpDenominatorCombine[Expand[FCI[input]]]],
      "\:79ef\:5206",RGZMXIntegrate[input,
        FileNameJoin[{rgzPTJob["\:76ee\:5f55"],"tid_units"}],seconds],
      "\:6807\:91cf",RGZMXScalarReduce[input,
        FileNameJoin[{rgzPTJob["\:76ee\:5f55"],"pv_units"}],seconds]],
    seconds,Failure["PTStageTimeout",<|"\:9879\:76ee"->name,
      "\:9636\:6bb5"->stage,"\:65f6\:9650"->seconds|>]],
    Failure["PTStageAborted",<|"\:9879\:76ee"->name,"\:9636\:6bb5"->stage|>]]];
  If[!RGZPTGood[stage,z],
    If[!FailureQ[z],z=Failure["PTStageUnresolved",<|
      "\:9879\:76ee"->name,"\:9636\:6bb5"->stage,"\:7ed3\:679c\:5934\:90e8"->Head[z]|>]];
    record=<|"\:8f93\:5165"->input,"\:73af\:5883"->rgzPTJob["\:73af\:5883"],"\:72b6\:6001"->"\:672a\:5b8c\:6210",
      "\:505c\:6b62\:539f\:56e0"->z,"\:8017\:65f6"->time|>;
    r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]];
    AssociateTo[part,"\:505c\:6b62\:539f\:56e0"->z];RGZPTSetPart[name,part];
    RGZMXProgressUpdate["\:72b6\:6001"->"\:672a\:5b8c\:6210"];Return[z]];
  record=<|"\:8f93\:5165"->input,"\:73af\:5883"->rgzPTJob["\:73af\:5883"],"\:72b6\:6001"->"\:5b8c\:6210",
    "\:7ed3\:679c"->z,"\:8017\:65f6"->time|>;
  r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]];
  AssociateTo[res,stage->z];part=Join[KeyDrop[part,"\:505c\:6b62\:539f\:56e0"],<|"\:7ed3\:679c"->res|>];
  RGZPTSetPart[name,part];RGZMXProgressUpdate["\:72b6\:6001"->"\:5b8c\:6210"];
  <|"\:9879\:76ee"->name,"\:9636\:6bb5"->stage,"\:72b6\:6001"->"\:5b8c\:6210","\:8017\:65f6"->time|>
];

RGZPTComputePart[name_String,seconds_:1200] := Module[{r,stage,stop},Catch[
  Do[r=RGZPTRun[name,stage,seconds];If[FailureQ[r],Throw[r,stop]],
    {stage,{"\:6295\:5f71","\:5206\:89e3","\:79ef\:5206","\:6807\:91cf"}}];
  RGZMXReport["\:6458\:8981",name,"\:ff1a\:56db\:4e2a\:9636\:6bb5\:5b8c\:6210\:3002"];
  <|"\:9879\:76ee"->name,"\:72b6\:6001"->"\:5b8c\:6210"|>,stop]
];
RGZPTComputeAll[seconds_:1200] := Module[{r,name,stop},Catch[
  With[{check=RGZPTJobCheck[]},If[FailureQ[check],Return[check]]];
  Do[r=RGZPTComputePart[name,seconds];If[FailureQ[r],Throw[r,stop]],
    {name,rgzPTJob["\:9879\:76ee\:987a\:5e8f"]}];
  <|"\:72b6\:6001"->"\:56db\:7c7b\:5019\:9009\:9879\:5168\:90e8\:5b8c\:6210","\:9879\:76ee\:6570"->4|>,stop]
];

RGZPTStatus[] := Module[{names,stages,rows},
  With[{check=RGZPTJobCheck[]},If[FailureQ[check],Return[check]]];
  names=rgzPTJob["\:9879\:76ee\:987a\:5e8f"];stages={"\:6295\:5f71","\:5206\:89e3","\:79ef\:5206","\:6807\:91cf"};
  rows=Table[Prepend[(If[KeyExistsQ[rgzPTJob["\:9879\:76ee"][name]["\:7ed3\:679c"],#],
    "\:5b8c\:6210","\:672a\:5b8c\:6210"]&/@stages),name],{name,names}];
  Grid[Prepend[rows,Prepend[stages,"\:9879\:76ee"]],Frame->All,Alignment->Left]
];
RGZPTSummary[x_] := Which[
  FailureQ[x],Print["\:505c\:6b62\:6807\:7b7e\:ff1a",x[[1]]];
    Module[{data=x[[2]],keys},keys=Select[
      {"\:9879\:76ee","\:9636\:6bb5","\:7f3a\:5c11\:9636\:6bb5","\:672a\:5b8c\:6210\:9879\:76ee","\:8f93\:5165\:5206\:9879","\:65f6\:9650","\:8bf4\:660e"},KeyExistsQ[data,#]&];
      If[keys=!={},Print[Grid[({#,data[#]}&/@keys),Frame->All,Alignment->Left]]]],
  AssociationQ[x] && KeyExistsQ[x,"\:6b8b\:5dee\:5206\:9879"],RGZGaugeSummary[x],
  AssociationQ[x],Print[Grid[Normal[KeyTake[x,
    {"\:72b6\:6001","\:9879\:76ee\:6570","\:7f13\:5b58\:76ee\:5f55","\:8bf4\:660e"}]]/.Rule[a_,b_]:>{a,b},
    Frame->All,Alignment->Left]],
  True,Print["\:672a\:5b8c\:6210\:ff1b\:7ed3\:679c\:5934\:90e8\:ff1a",Head[x]]];

RGZPTResult[] := Module[{names,missing,pinches,parts},
  With[{check=RGZPTJobCheck[]},If[FailureQ[check],Return[check]]];
  names=rgzPTJob["\:9879\:76ee\:987a\:5e8f"];
  missing=Select[names,!KeyExistsQ[rgzPTJob["\:9879\:76ee"][#]["\:7ed3\:679c"],"\:6807\:91cf"]&];
  If[missing=!={},Return[Failure["PinchPartsIncomplete",<|"\:672a\:5b8c\:6210\:9879\:76ee"->missing|>]]];
  pinches=Association@@Table[name->rgzPTJob["\:9879\:76ee"][name]["\:7ed3\:679c"]["\:6807\:91cf"],{name,names}];
  parts=If[rgzPTJob["Mode"]==="FourPinch",
    Join[rgzPTJob["\:81ea\:80fd\:5206\:9879"],Lookup[pinches,names]],Missing["FullPinchRequired"]];
  <|"\:5171\:540c\:7ea6\:5b9a\:81ea\:80fdT\:5206\:9879"->rgzPTJob["\:81ea\:80fd\:5206\:9879"],
    "\:5171\:540c\:7ea6\:5b9a\:81ea\:80fdT"->Total[rgzPTJob["\:81ea\:80fd\:5206\:9879"]],
    "pinch\:5206\:9879"->pinches,"pinch\:603b\:548c"->Total[Lookup[pinches,names]],
    "\:603b\:548c\:5206\:9879"->parts,"\:5019\:9009\:603b\:548cT"->If[ListQ[parts],Total[parts],parts],
    "QCD\:7ea6\:5b9a"->rgzPTJob["QCD\:7ea6\:5b9a"],"\:89c4\:8303\:68c0\:67e5"->rgzPTJob["\:6bd4\:8f83"],
    "Mode"->rgzPTJob["Mode"],"ReferenceGauge"->rgzPTJob["ReferenceGauge"],"KernelVersion"->rgzPTJob["KernelVersion"],
    "\:8bf4\:660e"->"\:6709\:9650 RGZ \:7684\:5019\:9009\:4f20\:64ad\:5b50\:578b\:6a2a\:5411\:6838\:ff1b\:672a\:8bc1\:660e RGZ pinch \:5b8c\:5907\:6027\:ff0c\:672a\:81ea\:52a8\:53d6 M->0\:3002"|>
];

Options[RGZPTCompare]={"\:9884\:5904\:7406\:65f6\:9650"->600};
RGZPTCompare[reference_:1,seconds_:60,OptionsPattern[]] := Module[{r,report},
  r=RGZPTResult[];If[FailureQ[r],Return[r]];
  If[r["Mode"]=!="FourPinch",Return[Failure["FullPinchRequired",<|"Action"->"RGZPTStart[\"Mode\" -> \"FourPinch\"]"|>]]];
  report=RGZGaugeCompare[r["\:603b\:548c\:5206\:9879"],reference,seconds,
    "\:9884\:5904\:7406\:65f6\:9650"->OptionValue["\:9884\:5904\:7406\:65f6\:9650"]];
  If[AssociationQ[report],report=Join[report,<|
    "\:68c0\:67e5\:5bf9\:8c61"->"\:5b8c\:6574\:6df7\:5408\:81ea\:80fd + \:56db\:7c7b pinch \:5019\:9009\:9879\:7684\:6a2a\:5411\:89c4\:8303\:5dee",
    "QCD\:7ea6\:5b9a"->rgzPTJob["QCD\:7ea6\:5b9a"],
    "\:8bf4\:660e"->"\:96f6\:6b8b\:5dee\:4ec5\:9a8c\:8bc1\:5f53\:524d\:5019\:9009\:7ec4\:5408\:ff1bRGZ Ward/ST \:5b8c\:5907\:6027\:548c\:6700\:7ec8\:8c03\:8282\:5b50\:6781\:9650\:4ecd\:9700\:72ec\:7acb\:6838\:5bf9\:3002"|>]];
  AssociateTo[rgzPTJob,"\:6bd4\:8f83"->report];report
];

Options[RGZPTContinueAll]={"\:9884\:5904\:7406\:65f6\:9650"->600,
  "\:7f13\:5b58\:76ee\:5f55"->Automatic,"QCD\:62a5\:544a"->Automatic};
RGZPTContinueAll[data_,unitSeconds_:1200,coefficientSeconds_:60,
    OptionsPattern[]] := Module[{init,run,report,require,stop},Catch[
  require[x_] := If[FailureQ[x] || Head[x]===Missing ||
      x===$Failed || x===$Aborted,Throw[x,stop],x];
  init=require[RGZPTLegacyStart[data,"\:7f13\:5b58\:76ee\:5f55"->OptionValue["\:7f13\:5b58\:76ee\:5f55"],
    "QCD\:62a5\:544a"->OptionValue["QCD\:62a5\:544a"]]];
  run=require[RGZPTComputeAll[unitSeconds]];
  report=require[RGZPTCompare[1,coefficientSeconds,
    "\:9884\:5904\:7406\:65f6\:9650"->OptionValue["\:9884\:5904\:7406\:65f6\:9650"]]];
  report,stop]
];

RGZPTSave[path_:Automatic] := Module[{file=path,r,t,status},
  r=RGZPTResult[];If[FailureQ[r],Return[r]];
  If[file===Automatic,file=FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],
    "RGZ_Mixed_PT_Check_Result.mx"}]];
  If[!StringQ[file],Return[Failure["PTSavePath",<||>]]];
  rgzPTSaved=r;
  {t,status}=AbsoluteTiming[Check[DumpSave[file,rgzPTSaved],$Failed]];
  If[status===$Failed,Failure["PTSaveFailed",<||>],
    <|"\:72b6\:6001"->"\:4fdd\:5b58\:5b8c\:6210","\:6587\:4ef6"->file,"\:8017\:65f6"->t|>]
];

Print["RGZ pinch \:540e\:7eed\:6d41\:7a0b\:5df2\:52a0\:8f7d\:ff1aRGZPTContinueAll[rgzMixed,1200,60]\:ff1b\:9010\:6b65\:8fd0\:884c\:7528 RGZPTLegacyStart\:3001RGZPTRun\:3001RGZPTCompare\:3002"];
(* Restore saved results without assigning Get's return value:
   Get[FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"RGZ_Mixed_PT_Check_Result.mx"}]];
   rgzPTCheck=rgzPTSaved["\:89c4\:8303\:68c0\:67e5"];RGZPTSummary[rgzPTCheck];
   Only request one residual row to avoid formatting giant expressions:
   RGZGaugeShow[rgzPTCheck,1]
*)

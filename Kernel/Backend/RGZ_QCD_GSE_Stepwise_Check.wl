(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Actual FeynArts gse versus arXiv:0909.2536 Eq.(2.56).
   Load the corrected main, comparison, and pinch-step files first.
   The model must encode full rational propagators in its two-leg vertices.
   Select only V[1] on both internal lines of the cubic-vertex bubble.
   Compare alpha minus alpha=0 after taking the massless QCD limit.
   This amplitude is ALREADY amputated: do not multiply it by (p^2)^2.
   The user-specified common conversion factor defaults to 1; no sign or
   phase is selected automatically by fitting the paper result. *)

ClearAll[RGZQCDGSEStart, RGZQCDGSERun, RGZQCDGSEShow,
  RGZQCDGSEPaint, RGZQCDGSETotal, RGZQCDGSEMap];

RGZQCDGSEMap[fun_,amps_] := Module[{out,bad},
  If[!ListQ[amps] || amps === {},
    Return[Failure["NoAmplitudes",<|"Input"->amps|>]]];
  out=fun /@ amps;
  bad=Select[out,FailureQ[#] || MemberQ[{$Failed,$Aborted},#]&];
  If[bad =!= {},First[bad],out]
];

Options[RGZQCDGSEStart]={"FeynArtsToPaperFactor"->1};
RGZQCDGSEStart[loops_:Automatic,OptionsPattern[]] := Module[
  {input=loops,sector="\:73bb\:8272\:573a\:4e24\:4e09\:70b9\:9876\:70b9\:5708\:56fe",graphs,probe},
  If[Length[DownValues[RGZQCDRationalMassless]] === 0 ||
      Length[DownValues[RGZQCDRow]] === 0,
    Return[Failure["LoadHelpers",<|
      "Message"->"\:5148\:52a0\:8f7d\:4e00\:5708\:68c0\:67e5\:3001QCD \:6bd4\:8f83\:548c pinch \:9010\:6b65\:68c0\:67e5\:4e09\:4e2a\:6587\:4ef6\:3002"|>]]];
  If[Total[Length /@ {OwnValues[m],OwnValues[mG],OwnValues[M],
      OwnValues[\[Mu]],OwnValues[\[Alpha]]}] > 0,
    Return[Failure["SymbolicParametersRequired",<|
      "Message"->"m\:3001mG\:3001M\:3001\:03bc\:3001\:03b1 \:5e94\:4e3a\:672a\:8d4b\:503c\:7684\:7b26\:53f7\:3002"|>]]];
  probe=FCI[FVD[q,\[Rho]]];
  If[RGZLoopNormalize[probe] =!= probe,
    Return[Failure["IndexNormalizationBug",<|
      "Message"->"\:5f53\:524d\:6807\:51c6\:5316\:51fd\:6570\:6539\:53d8\:4e86\:6d1b\:4f26\:5179\:6307\:6807\:ff0c\:8bf7\:5148\:52a0\:8f7d\:4fee\:6b63\:7248\:4e3b\:6587\:4ef6\:3002"|>]]];
  If[input === Automatic && AssociationQ[rgzOneLoop],input=rgzOneLoop];
  If[input === Automatic,
    If[!StringQ[modelRGZ],Return[Failure["SetModel",<|
      "Message"->"\:6ca1\:6709\:53ef\:590d\:7528\:7684 rgzOneLoop\:ff1b\:8bf7\:8bbe\:7f6e modelRGZ \:4e3a gen/mod \:7684\:5171\:540c\:8def\:5f84\:3002"|>]]];
    graphs=Missing["GenerateAtStage1"],
    If[!AssociationQ[input] || !KeyExistsQ[input,"\:63d2\:5165\:56fe"] ||
        !AssociationQ[input["\:63d2\:5165\:56fe"]] ||
        !KeyExistsQ[input["\:63d2\:5165\:56fe"],sector],
      Return[Failure["MissingGraphs",<|"Sector"->sector|>]]];
    graphs=input["\:63d2\:5165\:56fe"][sector]];
  rgzQCDGSESession=<|"\:5df2\:5b8c\:6210\:6b65\:9aa4"->0,"\:5f85\:9009\:56fe"->graphs,
    "\:7edf\:4e00\:56e0\:5b50"->OptionValue["FeynArtsToPaperFactor"]|>;
  Print["gse \:5df2\:51c6\:5907\:ff1a",If[Head[graphs] === Missing,
    "\:6b65\:9aa4 1 \:76f4\:63a5\:751f\:6210\:6240\:9700\:62d3\:6251\:3002","\:590d\:7528\:5df2\:6709\:63d2\:5165\:56fe\:3002"]];
  Print["\:9010\:4e2a\:8fd0\:884c RGZQCDGSERun[1] \:81f3 [9]\:ff1b\:672c\:9879\:4e0d\:4e58 (p^2)^2\:3002"];
  <|"\:72b6\:6001"->"\:51c6\:5907\:5b8c\:6210","\:7edf\:4e00\:56e0\:5b50"->rgzQCDGSESession["\:7edf\:4e00\:56e0\:5b50"]|>
];

RGZQCDGSERun[n_Integer,seconds_:60] := Module[
  {labels,keys,t,result,graphs,pure,fc,bad,r,parts,paper,a,ref,ratio},
  labels={"\:9009\:62e9\:7eaf\:80f6\:5b50\:4e24\:4e09\:70b9\:9876\:70b9\:5708\:56fe","CreateFeynAmp \:751f\:6210\:632f\:5e45",
    "FCFAConvert \:8f6c\:6362\:5230 FeynCalc","\:663e\:5f0f\:6709\:7406\:5206\:6bcd\:5e76\:53d6 QCD \:6781\:9650",
    "\:51cf\:53bb Landau \:89c4\:8303\:7684\:632f\:5e45","\:9010\:56fe\:6a2a\:5411\:6295\:5f71",
    "\:9010\:56fe\:7edf\:4e00\:5708\:52a8\:91cf\:5e76\:6c42\:548c","\:6784\:9020\:6587\:732e gse \:89c4\:8303\:5dee",
    "\:79ef\:5206\:524d\:540e\:6bd4\:8f83\:5e76\:663e\:793a\:7ed3\:679c"};
  keys={"\:63d2\:5165\:56fe","FeynArts\:632f\:5e45","FeynCalc\:632f\:5e45","QCD\:632f\:5e45",
    "\:89c4\:8303\:5dee\:632f\:5e45","\:6295\:5f71\:5217\:8868","\:8ba1\:7b97\:503c","\:6587\:732e\:53c2\:8003","\:6bd4\:8f83\:7ed3\:679c"};
  If[!AssociationQ[rgzQCDGSESession],
    Return[Failure["StartFirst",<|"Message"->"\:5148\:8fd0\:884c RGZQCDGSEStart[]\:3002"|>]]];
  If[n<1 || n>9,Return[Failure["InvalidStage",<|"Stage"->n|>]]];
  If[!NumericQ[seconds] || !TrueQ[seconds>0],
    Return[Failure["InvalidTimeout",<|"Seconds"->seconds|>]]];
  If[n =!= rgzQCDGSESession["\:5df2\:5b8c\:6210\:6b65\:9aa4"]+1,
    Return[Failure["StageOrder",<|
      "\:4e0b\:4e00\:6b65"->(rgzQCDGSESession["\:5df2\:5b8c\:6210\:6b65\:9aa4"]+1)|>]]];
  Print["gse\:ff0c\:6b65\:9aa4 ",n,"\:ff1a",labels[[n]]];
  {t,result}=AbsoluteTiming[TimeConstrained[Switch[n,
    1,
      graphs=rgzQCDGSESession["\:5f85\:9009\:56fe"];
      If[Head[graphs] === Missing,
        graphs=InsertFields[rgzLoopTopBosonBubble,{V[1]}->{V[1]},
          InsertionLevel->{Particles},Model->modelRGZ,
          GenericModel->modelRGZ]];
      pure=With[{fields=LoopFields[##]},fields =!= {} &&
        And@@(MatchQ[#,V[1,___]|-V[1,___]]& /@ fields)]&;
      DiagramSelect[graphs,pure],
    2,
      CreateFeynAmp[rgzQCDGSESession["\:63d2\:5165\:56fe"],Truncated->True,
        GaugeRules->{},PreFactor->1],
    3,
      fc=FCFAConvert[RGZLoopPrepareFA[rgzQCDGSESession["FeynArts\:632f\:5e45"]],
        IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{q},
        LorentzIndexNames->{\[Mu],\[Nu]},ChangeDimension->D,
        UndoChiralSplittings->True,List->True,DropSumOver->True,SMP->True];
      If[!ListQ[fc] || fc === {},Failure["NoConvertedAmplitudes",<|"Output"->fc|>],
        bad=RGZLoopUnresolved[fc];
        If[bad =!= {},Failure["UnresolvedCoupling",<|"Objects"->bad|>],fc]],
    4,
      RGZQCDGSEMap[Function[amp,
        r=RGZLoopNormalize[ChangeDimension[FCI[amp],D]];
        bad=Cases[r,x_ /; MemberQ[
          {"mG1","mG2","mG3","mA1","mA2","mB",
           "m1","m2","m3","m4","rgzLoopM1","rgzLoopM2",
           "rgzLoopM3","rgzLoopM4"},RGZLoopName[x]],Infinity];
        If[bad =!= {},Failure["RationalModelRequired",<|
          "Objects"->DeleteDuplicates[bad],
          "Message"->"\:9700\:4f7f\:7528\:5df2\:6838\:5bf9\:7684\:6709\:7406\:4f20\:64ad\:5b50\:6a21\:578b\:ff1b\:6b64\:5904\:4e0d\:5957\:7528\:65e7\:5360\:4f4d\:5206\:6bcd\:89c4\:5219\:3002"|>],
          RGZQCDRationalMassless[FeynAmpDenominatorExplicit[r]]]],
        rgzQCDGSESession["FeynCalc\:632f\:5e45"]],
    5,
      RGZQCDGSEMap[Function[amp,
        RGZLoopReduce[Expand[amp-(amp /. \[Alpha]->0)]]],
        rgzQCDGSESession["QCD\:632f\:5e45"]],
    6,
      RGZQCDGSEMap[RGZQCDProject,rgzQCDGSESession["\:89c4\:8303\:5dee\:632f\:5e45"]],
    7,
      parts=RGZQCDGSEMap[RGZQCDXY,rgzQCDGSESession["\:6295\:5f71\:5217\:8868"]];
      If[FailureQ[parts],parts,
        Cancel[Together[rgzQCDGSESession["\:7edf\:4e00\:56e0\:5b50"] Total[parts]]]],
    8,
      paper=RGZQCDPaperParts[]["gse"];
      a=RGZQCDRational[Expand[paper-(paper /. \[Alpha]->0)]];
      If[FailureQ[a],a,a=RGZQCDProject[a];
        If[FailureQ[a],a,RGZQCDXY[a]]],
    9,
      RGZQCDRow["gse","(2.56)",rgzQCDGSESession["\:8ba1\:7b97\:503c"],
        rgzQCDGSESession["\:6587\:732e\:53c2\:8003"]]],seconds,$Aborted]];
  Print["\:8017\:65f6\:ff1a",NumberForm[t,{10,3}]," \:79d2"];
  If[result === $Aborted,Return[Failure["Timeout",<|
    "\:6b65\:9aa4"->n,"\:8bf4\:660e"->"\:672a\:63a8\:8fdb\:72b6\:6001\:ff1b\:53ef\:589e\:5927\:79d2\:6570\:540e\:91cd\:8bd5\:5f53\:524d\:6b65\:9aa4\:3002"|>]]];
  If[result === $Failed || FailureQ[result],
    Return[Failure["StageFailed",<|"\:6b65\:9aa4"->n,"\:539f\:56e0"->result|>]]];
  AssociateTo[rgzQCDGSESession,{keys[[n]]->result,"\:5df2\:5b8c\:6210\:6b65\:9aa4"->n}];
  If[rgzQCDGSESession["\:5df2\:5b8c\:6210\:6b65\:9aa4"] =!= n ||
      !KeyExistsQ[rgzQCDGSESession,keys[[n]]],
    Return[Failure["StateWriteFailed",<|"\:6b65\:9aa4"->n|>]]];
  If[MemberQ[{3,4,5,6},n],Print["\:632f\:5e45\:9879\:6570\:ff1a",Length[result]]];
  If[n === 9,
    a=result["\:8ba1\:7b97\:7684I11\:7cfb\:6570"];ref=result["\:6587\:732e\:7684I11\:7cfb\:6570"];
    ratio=If[FailureQ[a] || FailureQ[ref] || TrueQ[ref === 0],
      Missing["RatioUnavailable"],Cancel[Together[a/ref]]];
    AssociateTo[rgzQCDGSESession,"\:7cfb\:6570\:6bd4\:503c\:ff08\:672a\:7528\:4e8e\:8c03\:6574\:ff09"->ratio];
    If[!AssociationQ[rgzQCDStepResults],rgzQCDStepResults=Association[]];
    AssociateTo[rgzQCDStepResults,"gse"->result];
    RGZQCDGSEShow[]];
  <|"\:6b65\:9aa4"->n,"\:72b6\:6001"->"\:5b8c\:6210","\:8017\:65f6\:ff08\:79d2\:ff09"->t,
    "\:8868\:8fbe\:5f0f\:5927\:5c0f"->LeafCount[result]|>
];

RGZQCDGSEPaint[] := If[AssociationQ[rgzQCDGSESession] &&
    KeyExistsQ[rgzQCDGSESession,"\:63d2\:5165\:56fe"],
  Paint[rgzQCDGSESession["\:63d2\:5165\:56fe"],Numbering->Simple,SheetHeader->None],
  Failure["SelectGraphsFirst",<|"Message"->"\:5148\:5b8c\:6210\:6b65\:9aa4 1\:3002"|>]];

RGZQCDGSEShow[] := Module[{row,fields},
  If[!AssociationQ[rgzQCDGSESession] ||
      !KeyExistsQ[rgzQCDGSESession,"\:6bd4\:8f83\:7ed3\:679c"],
    Return[Failure["ComparisonNotReady",<||>]]];
  row=rgzQCDGSESession["\:6bd4\:8f83\:7ed3\:679c"];
  fields={"\:8ba1\:7b97\:503c","\:6587\:732e\:53c2\:8003","\:6362\:5143\:540e\:5dee\:503c","\:8ba1\:7b97\:7684I11\:7cfb\:6570",
    "\:6587\:732e\:7684I11\:7cfb\:6570","\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570","\:72b6\:6001"};
  Print[Grid[Prepend[Map[{#,TraditionalForm[Lookup[row,#,
    Missing["NotComputed"]]]}&,fields],{"gse \:68c0\:67e5\:9879\:76ee","\:7ed3\:679c"}],
    Frame->All,Alignment->Left,Spacings->{2,1}]];
  Print["\:7edf\:4e00\:56e0\:5b50\:ff1a",rgzQCDGSESession["\:7edf\:4e00\:56e0\:5b50"]];
  Print["\:8ba1\:7b97 / \:6587\:732e \:7684\:79ef\:5206\:7cfb\:6570\:6bd4\:ff08\:672a\:7528\:4e8e\:8c03\:6574\:ff09\:ff1a",
    rgzQCDGSESession["\:7cfb\:6570\:6bd4\:503c\:ff08\:672a\:7528\:4e8e\:8c03\:6574\:ff09"]]
];

RGZQCDGSETotal[] := Module[{names,missing,rows,bad,coefficient},
  names={"box","nav\:5168\:90e8","av\:5168\:90e8","qse\:5168\:90e8","gse"};
  If[!AssociationQ[rgzQCDStepResults],
    Return[Failure["NoResults",<||>]]];
  missing=Select[names,!KeyExistsQ[rgzQCDStepResults,#]&];
  If[missing =!= {},Return[Failure["IncompleteParts",<|"\:7f3a\:5c11\:9879\:76ee"->missing|>]]];
  rows=Lookup[rgzQCDStepResults,names];
  bad=Select[rows,!MemberQ[{"\:79ef\:5206\:524d\:540e\:5747\:901a\:8fc7","\:79ef\:5206\:540e\:901a\:8fc7\:ff1b\:79ef\:5206\:524d\:5f62\:5f0f\:4e0d\:540c"},
    Lookup[#,"\:72b6\:6001",Missing["NotComputed"]]]&];
  If[bad =!= {},Return[Failure["ComparisonNotPassed",<|"\:672a\:901a\:8fc7\:9879\:76ee"->
    Lookup[bad,"\:9879\:76ee"]|>]]];
  coefficient=RGZQCDMasterCoefficient[Total[Lookup[rows,"\:8ba1\:7b97\:503c"]]];
  If[FailureQ[coefficient],Return[coefficient]];
  Print[Grid[Prepend[Map[{#["\:9879\:76ee"],#["\:72b6\:6001"],#["\:79ef\:5206\:540e\:5dee\:503c\:7cfb\:6570"]}&,
    rows],{"\:9879\:76ee","\:72b6\:6001","\:5dee\:503c / I11"}],Frame->All,Alignment->Left]];
  <|"\:8ba1\:7b97\:7684\:4e94\:7c7b\:89c4\:8303\:5dee\:603b\:548c / I11"->coefficient,
    "\:62b5\:6d88\:68c0\:67e5"->If[TrueQ[coefficient === 0],"\:901a\:8fc7","\:603b\:548c\:5c1a\:672a\:5f52\:96f6"]|>
];

Print["\:5b9e\:9645 gse \:9010\:6b65\:68c0\:67e5\:5df2\:52a0\:8f7d\:3002\:5148\:8fd0\:884c RGZQCDGSEStart[]\:ff0c\:518d\:9010\:4e2a\:8fd0\:884c RGZQCDGSERun[1] \:81f3 [9]\:3002"];

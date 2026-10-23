(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* RGZ residual-39: joint zero-mass QCD check, v1.2.

   v1.2: normalize actual Power objects using their base/exponent, instead
   of relying on a literal compound-power pattern. In particular, the
   reported remaining Sqrt[h^2 r^2] is exactly h*r for h>0,r>0.

   v1.1: explicitly Together BEFORE Numerator/Denominator/PolynomialQ.
   Native Mathematica Cancel threads over Plus; it does not combine the
   summands over a common denominator. Testing only in a compatible algebra
   engine did not expose that difference in v1.0.

   Reads the ORIGINAL RGZ_Gauge_Residual_39.wxf, not the seven-row M=0 table.
   No diagrams, integrals, mixed matrices or existing caches are recalculated.
   Exact diagnostic path, r>0:
     p^2=-11, N=SUNN=3,
     m^2=9 r, mu^2=r, mG^4=25 r^2, M^2=r; r -> 0.
   Thus ALL four mass parameters vanish together. alpha and D remain symbolic.
   mG^4=2 N gs^2 gamma^4 implies gamma -> 0; gs is NOT set to zero.
   A common nonzero Pi^2 gs^2 factor is stripped, not assumed to vanish.

   Prescription: first take r->0 at generic dimension (Re[D]>6 suffices for
   coefficients with poles no worse than r^-2), then analytically continue in D.
   Massless tadpoles vanish in dimensional regularization. Soft small-mass
   terms vanish in the stated dimension region. This does NOT assert that
   taking D->4 first and taking r->0 first commute.

   For a=r*k1,b=r*k2 the hard-region bubble expansion is
   B0(s,a,b)/B0(s,0,0) = 1-r*(D-3)*(k1+k2)/s
     +r^2*((k1^2+k2^2)*(D-4)*(D-3)/2+k1*k2*(D-6)*(D-3))/s^2+O(r^3).
   The second-order terms are kept BEFORE degenerate masters are merged.
   No PowerExpand, D->4, numerical alpha, or fitted cancellation is used.

   Scope: a test of the exported candidate GAUGE RESIDUAL on the stated joint
   path. It does not check the full QCD amplitude or prove completeness of the
   full S-matrix/pinch construction. It is not an all-path mass-limit proof.
   Source-derived exact algebra was checked independently; native Mathematica
   and FeynCalc execution have not been tested in the authoring environment.

   Get[FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"RGZ_Residual39_QCD_Zero_Check.wl"}]];
   rgzQCDZero=RGZ39QCDCheck[FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],
     "RGZ_Gauge_Residual_39.wxf"}],60];
   RGZ39QCDSummary[rgzQCDZero];
*)

BeginPackage["RGZResidual39QCD`"];
RGZ39QCDCheck::usage="RGZ39QCDCheck[file,seconds] checks the joint zero-mass limit of the original 39 residuals, keeping alpha and D symbolic. Automatic reads the WXF beside this script.";
RGZ39QCDSummary::usage="RGZ39QCDSummary[result] prints only a compact diagnostic table.";
RGZ39QCDRootCheck::usage="RGZ39QCDRootCheck[] checks the exact positive-scale root normalization before reading the WXF.";
Begin["`Private`"];

$sourceDirectory=If[StringQ[$InputFileName]&&StringLength[$InputFileName]>0,
  DirectoryName[ExpandFileName[$InputFileName]],Directory[]];
$stopTag="RGZ39QCDStopped";
$progress="\:51c6\:5907";
Clear[r,h,dim,a0,b0];

Clear[scaleRootValue,normalizeScaleRoots];
scaleRootValue[base_,exponent_]:=Module[{bb=Expand[base]},
  Which[
    SameQ[bb,r^2 h^2],(r h)^(2 exponent),
    SameQ[bb,r^2],r^(2 exponent),
    SameQ[bb,h^2],h^(2 exponent),
    True,Refine[base^exponent,r>0&&h>0]
  ]
];
normalizeScaleRoots[expr_]:=Module[{roots,rules},
  roots=DeleteDuplicates@Cases[expr,
    pw_Power/;Head[pw[[2]]]===Rational&&!FreeQ[pw,r|h],{0,Infinity}];
  rules=(#->scaleRootValue[#[[1]],#[[2]]])&/@roots;
  expr/.rules
];
RGZ39QCDRootCheck[]:=And[
  SameQ[normalizeScaleRoots[Sqrt[h^2 r^2]],h r],
  SameQ[normalizeScaleRoots[Sqrt[r^2]],r],
  SameQ[normalizeScaleRoots[1/Sqrt[h^2 r^2]],1/(h r)]
];

SetAttributes[bounded,HoldRest];
bounded[label_,body_,seconds_]:=Module[{ans},
  $progress=label;
  ans=TimeConstrained[body,seconds,
    Failure["QCDCheckTimeout",<|"\:64cd\:4f5c"->label,"\:65f6\:9650"->seconds|>]];
  If[ans===$Failed||ans===$Aborted,
    ans=Failure["QCDCheckFailed",<|"\:64cd\:4f5c"->label|>]];
  If[FailureQ[ans],Throw[ans,$stopTag]];
  ans
];

pointExpression[input_]:=Module[{expr},
  expr=input/.{
    HoldPattern[FeynCalc`A0[v_]]:>a0[v],
    HoldPattern[FeynCalc`B0[v_,u_,w_]]:>b0[v,u,w]
  };
  expr=expr/.{
    HoldPattern[FeynCalc`Pair[FeynCalc`Momentum[RGZWorkflow`Engine`p,___],FeynCalc`Momentum[RGZWorkflow`Engine`p,___]]]->-11,
    HoldPattern[FeynCalc`SPD[RGZWorkflow`Engine`p,RGZWorkflow`Engine`p]]->-11,
    HoldPattern[FeynCalc`SMP["g_s"]]->1,
    HoldPattern[System`N]->3,
    HoldPattern[FeynCalc`SUNN]->3,
    HoldPattern[RGZWorkflow`Engine`m]->3 Sqrt[r],
    HoldPattern[RGZWorkflow`Engine`\:03bc]->Sqrt[r],
    HoldPattern[RGZWorkflow`Engine`mG]->Sqrt[5] Sqrt[r],
    HoldPattern[RGZWorkflow`Engine`M]->Sqrt[r],
    HoldPattern[RGZWorkflow`Engine`\:03b1]->h^2,
    HoldPattern[System`D]->dim
  };
  (* Root bases are compared algebraically under the stated positive path. *)
  normalizeScaleRoots[expr]
];

prettyCoefficient[expr_]:=Module[{q},
  q=Expand[expr]/.{HoldPattern[h^n_Integer]/;EvenQ[n]:>RGZWorkflow`Engine`\:03b1^(n/2)};
  Factor[Cancel[Together[q/.{h->Sqrt[RGZWorkflow`Engine`\:03b1],dim->System`D}]]]
];

(* Do not replace Together with Cancel: Cancel threads over sums in MMA. *)
singleFraction[expr_]:=Cancel[Together[normalizeScaleRoots[expr]]];

(* Exact rational recurrence avoids an expensive general Series operation. *)
rationalLaurent[co_]:=Module[{fraction,nn,dd,nmin,dmin,low,d0,values,k,j,next},
  If[co===0,Return[0]];
  fraction=singleFraction[co];
  nn=Numerator[fraction];dd=Denominator[fraction];
  nmin=Exponent[nn,r,Min];dmin=Exponent[dd,r,Min];low=nmin-dmin;
  If[low>0,Return[0]];
  d0=Coefficient[dd,r,dmin];values=ConstantArray[0,1-low];
  Do[
    next=Coefficient[nn,r,nmin+k]-
      Sum[Coefficient[dd,r,dmin+j]values[[k-j+1]],{j,1,k}];
    values[[k+1]]=Factor[singleFraction[next/d0]],{k,0,-low}];
  Total[Table[values[[k+1]]r^(low+k),{k,0,-low}]]
];

bubbleTaylor[sp_,k1_,k2_]:=1-r(dim-3)(k1+k2)/sp+
  r^2((k1^2+k2^2)(dim-4)(dim-3)/2+k1 k2(dim-6)(dim-3))/sp^2;

checkCore[file_,seconds_]:=Block[
  {RGZWorkflow`Engine`m,RGZWorkflow`Engine`\:03bc,RGZWorkflow`Engine`mG,RGZWorkflow`Engine`M,RGZWorkflow`Engine`\:03b1,RGZWorkflow`Engine`p,FeynCalc`SUNN},
  Module[{packet,rows,row,i,master,co,nn,dd,low,laurent,parts={},zeros={},
    k1,k2,sp,unknown,whole,cm2,cm1,c0,answer,hash},
    packet=bounded["\:8bfb\:53d6\:539f\:59cb 39 \:9879 WXF",Quiet[Check[Import[file,"WXF"],$Failed]],seconds];
    If[!AssociationQ[packet],Throw[Failure["InvalidPacket",<||>],$stopTag]];
    rows=Lookup[packet,"\:6b8b\:5dee\:5206\:9879",Missing["NoRows"]];
    If[!ListQ[rows]||Length[rows]=!=39||!AllTrue[rows,AssociationQ],
      Throw[Failure["InvalidRows",<|"\:8bf4\:660e"->"\:9700\:8981\:539f\:59cb39\:9879\:6b8b\:5dee\:5206\:9879\:ff0c\:4e0d\:662f\:56fa\:5b9a\:8d28\:91cf\:540e\:7684\:4e03\:9879\:8868\:3002"|>],$stopTag]];
    If[!TrueQ[Lookup[packet,"\:53c2\:8003\:89c4\:8303",Missing[]]===1],
      Throw[Failure["DifferentReferenceGauge",<|"\:8bf4\:660e"->"\:672c\:68c0\:67e5\:8981\:6c42\:53c2\:8003\:89c4\:8303\:4e3a1\:3002"|>],$stopTag]];
    Do[
      row=rows[[i]];
      If[!And@@(KeyExistsQ[row,#]&/@{"\:7f16\:53f7","\:4e3b\:79ef\:5206","\:6b8b\:5dee\:7cfb\:6570"}),
        Throw[Failure["MissingRowKey",<|"\:7f16\:53f7"->i|>],$stopTag]];
      master=pointExpression[row["\:4e3b\:79ef\:5206"]];
      If[!MatchQ[master,_a0|_b0],
        Throw[Failure["UnsupportedMaster",<|"\:7f16\:53f7"->i|>],$stopTag]];
      co=bounded["\:5171\:540c\:96f6\:8d28\:91cf\:8def\:5f84\:ff1a\:7cfb\:6570 "<>ToString[i]<>" / 39",
        singleFraction[pointExpression[row["\:6b8b\:5dee\:7cfb\:6570"]]/Pi^2],seconds];
      unknown=DeleteDuplicates@Cases[co,sym_Symbol/;
        !MemberQ[{r,h,dim,System`I},sym]&&Context[sym]=!="System`",Infinity];
      If[unknown=!={},Throw[Failure["UnmappedSymbols",<|"\:7f16\:53f7"->i,"\:7b26\:53f7"->unknown|>],$stopTag]];
      If[co===0,AppendTo[zeros,row["\:7f16\:53f7"]];Continue[]];
      nn=Numerator[co];dd=Denominator[co];
      If[!PolynomialQ[nn,r]||!PolynomialQ[dd,r],
        Throw[Failure["NonRationalMassScale",<|"\:7f16\:53f7"->i,
          "\:7cfb\:6570\:5934\:90e8"->Head[co],"\:5206\:5b50\:5934\:90e8"->Head[nn],"\:5206\:6bcd\:5934\:90e8"->Head[dd],
          "\:5269\:4f59\:542b\:5c3a\:5ea6\:6839\:5f0f"->Take[DeleteDuplicates@Cases[co,
            pw:Power[base_,_Rational]/;!FreeQ[base,r],Infinity],UpTo[6]]|>],$stopTag]];
      low=Exponent[nn,r,Min]-Exponent[dd,r,Min];
      If[low< -2,
        Throw[Failure["HigherMassPole",<|"\:7f16\:53f7"->i,"\:6700\:4f4e\:5e42"->low,"\:8bf4\:660e"->"\:9700\:8981\:8d85\:8fc7\:4e8c\:9636\:7684\:4e3b\:79ef\:5206\:5c55\:5f00\:ff0c\:4e0d\:80fd\:76f4\:63a5\:901a\:8fc7\:3002"|>],$stopTag]];
      If[Head[master]===a0,
        k1=Cancel[master[[1]]/r];
        If[!FreeQ[k1,r],Throw[Failure["NonScalingTadpole",<|"\:7f16\:53f7"->i|>],$stopTag]];
        Continue[]
      ];
      sp=master[[1]];k1=Cancel[master[[2]]/r];k2=Cancel[master[[3]]/r];
      If[!TrueQ[sp===-11]||!FreeQ[{k1,k2},r],
        Throw[Failure["NonScalingBubble",<|"\:7f16\:53f7"->i|>],$stopTag]];
      laurent=bounded["Laurent \:7cfb\:6570\:ff1a"<>ToString[i]<>" / 39",rationalLaurent[co],seconds];
      AppendTo[parts,laurent bubbleTaylor[sp,k1,k2]],{i,39}];
    whole=bounded["\:5408\:5e76\:5168\:90e8\:9000\:5316\:4e3b\:79ef\:5206",Expand[r^2 Total[parts]],seconds];
    cm2=bounded["\:6838\:5bf9 r^-2 \:7cfb\:6570",Factor[singleFraction[Coefficient[whole,r,0]]],seconds];
    cm1=bounded["\:6838\:5bf9 r^-1 \:7cfb\:6570",Factor[singleFraction[Coefficient[whole,r,1]]],seconds];
    c0=bounded["\:6838\:5bf9\:6709\:9650\:7cfb\:6570",Factor[singleFraction[Coefficient[whole,r,2]]],seconds];
    answer=If[And@@(TrueQ[#===0]&/@{cm2,cm1,c0}),"\:901a\:8fc7","\:672a\:5f52\:96f6"];
    hash=Quiet[TimeConstrained[Check[FileHash[file,"SHA256","HexString"],Missing["NotAvailable"]],
      seconds,Missing["HashTimeout"]]];
    <|"\:7248\:672c"->"1.2","\:72b6\:6001"->answer,"\:539f\:59cb\:9879\:6570"->39,"\:53c2\:8003\:89c4\:8303"->1,
      "\:8def\:5f84"->"m^2=9 r, \:03bc^2=r, mG^4=25 r^2, M^2=r\:ff1br\:21920+",
      "\:52a8\:91cf\:5e73\:65b9"->-11,"\:989c\:8272\:6570"->3,"\:4fdd\:7559\:53c2\:6570"->{RGZWorkflow`Engine`\:03b1,System`D},
      "\:539f\:7cfb\:6570\:5f52\:96f6\:7f16\:53f7"->zeros,"\:5408\:5e76\:4e3b\:79ef\:5206"->FeynCalc`B0[-11,0,0],
      "r^-2\:7cfb\:6570"->prettyCoefficient[cm2],"r^-1\:7cfb\:6570"->prettyCoefficient[cm1],
      "\:6709\:9650\:7cfb\:6570"->prettyCoefficient[c0],
      "\:89c4\:8303\:6b8b\:5dee\:6781\:9650\:ff08\:5265\:79bbPi^2gs^2\:ff09"->prettyCoefficient[c0]FeynCalc`B0[-11,0,0],
      "\:8f93\:5165\:6587\:4ef6"->file,"\:8f93\:5165SHA256"->hash,
      "\:6781\:9650\:7ea6\:5b9a"->"\:5148\:5728 Re[D]>6 \:53d6\:5171\:540c\:96f6\:8d28\:91cf\:6781\:9650\:ff0c\:518d\:4f5c\:7ef4\:6570\:89e3\:6790\:5ef6\:62d3\:ff1bD\:4e0d\:7f6e4\:3002",
      "\:8303\:56f4"->"\:9a8c\:8bc1\:5df2\:5bfc\:51fa\:7684\:5019\:9009\:89c4\:8303\:6b8b\:5dee\:5728\:6b64\:8def\:5f84\:5f52\:96f6\:ff1b\:4e0d\:7b49\:540c\:4e8e\:5b8c\:6574QCD\:632f\:5e45\:6216\:5b8c\:6574S\:77e9\:9635\:8bc1\:660e\:3002"|>
  ]
];

Options[RGZ39QCDCheck]={"\:663e\:793a\:8fdb\:5ea6"->True};
RGZ39QCDCheck[file_:Automatic,seconds_:60,OptionsPattern[]]:=Module[{path,ans},
  If[!NumericQ[seconds]||!TrueQ[seconds>0],Return[Failure["InvalidTimeLimit",<||>]]];
  If[!TrueQ[RGZ39QCDRootCheck[]],
    Return[Failure["RootNormalizationFailed",<|"\:8bf4\:660e"->"\:6b63\:5c3a\:5ea6\:6839\:5f0f\:81ea\:68c0\:672a\:901a\:8fc7\:ff1b\:672a\:8bfb\:53d6\:6216\:4fee\:6539\:539f\:59cb\:6570\:636e\:3002"|>]]];
  If[Length[OwnValues[RGZWorkflow`Engine`\:03b1]]>0,
    Return[Failure["AssignedGaugeParameter",<|"\:8bf4\:660e"->"\:5148 Clear[\:03b1]\:ff0c\:4fdd\:7559\:89c4\:8303\:53c2\:6570\:4e3a\:7b26\:53f7\:3002"|>]]];
  If[Length[OwnValues[System`D]]>0,
    Return[Failure["AssignedDimension",<|"\:8bf4\:660e"->"\:7ef4\:6570D\:5df2\:6709\:6570\:503c\:ff1b\:8bf7\:5728\:4fdd\:7559\:7b26\:53f7D\:7684\:5185\:6838\:4e2d\:68c0\:67e5\:3002"|>]]];
  path=If[file===Automatic,FileNameJoin[{$sourceDirectory,"RGZ_Gauge_Residual_39.wxf"}],file];
  If[!StringQ[path]||!FileExistsQ[path],
    Return[Failure["InputNotFound",<|"\:6587\:4ef6"->path|>]]];
  ans=If[TrueQ[OptionValue["\:663e\:793a\:8fdb\:5ea6"]],
    Monitor[Catch[checkCore[path,seconds],$stopTag],Style[$progress,12]],
    Catch[checkCore[path,seconds],$stopTag]];
  ans
];

RGZ39QCDSummary[ans_]:=If[AssociationQ[ans],
  Print[Grid[{
    {"\:68c0\:67e5\:9879\:76ee","\:7ed3\:679c"},
    {"\:5171\:540c\:96f6\:8d28\:91cf\:8def\:5f84",ans["\:8def\:5f84"]},
    {"p\:00b2 / N",{ans["\:52a8\:91cf\:5e73\:65b9"],ans["\:989c\:8272\:6570"]}},
    {"\:4fdd\:7559\:53c2\:6570",ans["\:4fdd\:7559\:53c2\:6570"]},
    {"\:539f\:59cb\:9879\:6570",ans["\:539f\:59cb\:9879\:6570"]},
    {"\:5408\:5e76\:4e3b\:79ef\:5206",TraditionalForm[ans["\:5408\:5e76\:4e3b\:79ef\:5206"]]},
    {"r^-2 \:7cfb\:6570",TraditionalForm[ans["r^-2\:7cfb\:6570"]]},
    {"r^-1 \:7cfb\:6570",TraditionalForm[ans["r^-1\:7cfb\:6570"]]},
    {"\:6709\:9650\:7cfb\:6570",TraditionalForm[ans["\:6709\:9650\:7cfb\:6570"]]},
    {"\:89c4\:8303\:6b8b\:5dee\:68c0\:67e5",ans["\:72b6\:6001"]}
  },Frame->All,Alignment->Left,Spacings->{2,1}]],
  If[FailureQ[ans],Print["\:505c\:6b62\:6807\:7b7e\:ff1a",ans[[1]]];
    Print[Grid[Prepend[List@@@Normal[ans[[2]]],{"\:9879\:76ee","\:8bca\:65ad"}],Frame->All,Alignment->Left]],
    Print["\:5c1a\:672a\:83b7\:5f97\:6709\:6548\:68c0\:67e5\:7ed3\:679c\:ff1b\:5934\:90e8\:ff1a",Head[ans]]]
];

End[];
EndPackage[];
Print["\:5df2\:52a0\:8f7d\:5171\:540c\:96f6\:8d28\:91cf QCD \:68c0\:67e5 v1.2\:ff1b\:6839\:5f0f\:81ea\:68c0\:ff1a",RGZ39QCDRootCheck[],
  "\:3002\:8fd0\:884c RGZ39QCDCheck[\:539f\:59cbWXF\:8def\:5f84,60]\:ff1b\:7b80\:8868\:7528 RGZ39QCDSummary[\:7ed3\:679c]\:3002"];

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
$progress="Prepare";
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
    Failure["QCDCheckTimeout",<|"Operation"->label,"TimeLimit"->seconds|>]];
  If[ans===$Failed||ans===$Aborted,
    ans=Failure["QCDCheckFailed",<|"Operation"->label|>]];
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
    HoldPattern[RGZWorkflow`Engine`μ]->Sqrt[r],
    HoldPattern[RGZWorkflow`Engine`mG]->Sqrt[5] Sqrt[r],
    HoldPattern[RGZWorkflow`Engine`M]->Sqrt[r],
    HoldPattern[RGZWorkflow`Engine`α]->h^2,
    HoldPattern[System`D]->dim
  };
  (* Root bases are compared algebraically under the stated positive path. *)
  normalizeScaleRoots[expr]
];

prettyCoefficient[expr_]:=Module[{q},
  q=Expand[expr]/.{HoldPattern[h^n_Integer]/;EvenQ[n]:>RGZWorkflow`Engine`α^(n/2)};
  Factor[Cancel[Together[q/.{h->Sqrt[RGZWorkflow`Engine`α],dim->System`D}]]]
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
  {RGZWorkflow`Engine`m,RGZWorkflow`Engine`μ,RGZWorkflow`Engine`mG,RGZWorkflow`Engine`M,RGZWorkflow`Engine`α,RGZWorkflow`Engine`p,FeynCalc`SUNN},
  Module[{packet,rows,row,i,master,co,nn,dd,low,laurent,parts={},zeros={},
    k1,k2,sp,unknown,whole,cm2,cm1,c0,answer,hash},
    packet=bounded["ReadOriginal 39 Term WXF",Quiet[Check[Import[file,"WXF"],$Failed]],seconds];
    If[!AssociationQ[packet],Throw[Failure["InvalidPacket",<||>],$stopTag]];
    rows=Lookup[packet,"ResidualParts",Missing["NoRows"]];
    If[!ListQ[rows]||Length[rows]=!=39||!AllTrue[rows,AssociationQ],
      Throw[Failure["InvalidRows",<|"Description"->"Provide the original 39 residual terms rather than the seven-term fixed-mass table."|>],$stopTag]];
    If[!TrueQ[Lookup[packet,"ReferenceGauge",Missing[]]===1],
      Throw[Failure["DifferentReferenceGauge",<|"Description"->"This check requires reference gauge 1."|>],$stopTag]];
    Do[
      row=rows[[i]];
      If[!And@@(KeyExistsQ[row,#]&/@{"ID","MasterIntegral","ResidualCoefficient"}),
        Throw[Failure["MissingRowKey",<|"ID"->i|>],$stopTag]];
      master=pointExpression[row["MasterIntegral"]];
      If[!MatchQ[master,_a0|_b0],
        Throw[Failure["UnsupportedMaster",<|"ID"->i|>],$stopTag]];
      co=bounded["CommonZeroMassPath: Coefficient "<>ToString[i]<>" / 39",
        singleFraction[pointExpression[row["ResidualCoefficient"]]/Pi^2],seconds];
      unknown=DeleteDuplicates@Cases[co,sym_Symbol/;
        !MemberQ[{r,h,dim,System`I},sym]&&Context[sym]=!="System`",Infinity];
      If[unknown=!={},Throw[Failure["UnmappedSymbols",<|"ID"->i,"Symbol"->unknown|>],$stopTag]];
      If[co===0,AppendTo[zeros,row["ID"]];Continue[]];
      nn=Numerator[co];dd=Denominator[co];
      If[!PolynomialQ[nn,r]||!PolynomialQ[dd,r],
        Throw[Failure["NonRationalMassScale",<|"ID"->i,
          "Coefficienthead"->Head[co],"Numeratorhead"->Head[nn],"Denominatorhead"->Head[dd],
          "remainingremaining contains sizedegreeRadical"->Take[DeleteDuplicates@Cases[co,
            pw:Power[base_,_Rational]/;!FreeQ[base,r],Infinity],UpTo[6]]|>],$stopTag]];
      low=Exponent[nn,r,Min]-Exponent[dd,r,Min];
      If[low< -2,
        Throw[Failure["HigherMassPole",<|"ID"->i,"MinPower"->low,"Description"->"A master-integral expansion beyond second order is required; this check cannot pass directly."|>],$stopTag]];
      If[Head[master]===a0,
        k1=Cancel[master[[1]]/r];
        If[!FreeQ[k1,r],Throw[Failure["NonScalingTadpole",<|"ID"->i|>],$stopTag]];
        Continue[]
      ];
      sp=master[[1]];k1=Cancel[master[[2]]/r];k2=Cancel[master[[3]]/r];
      If[!TrueQ[sp===-11]||!FreeQ[{k1,k2},r],
        Throw[Failure["NonScalingBubble",<|"ID"->i|>],$stopTag]];
      laurent=bounded["Laurent Coefficient: "<>ToString[i]<>" / 39",rationalLaurent[co],seconds];
      AppendTo[parts,laurent bubbleTaylor[sp,k1,k2]],{i,39}];
    whole=bounded["CombineAlldegeneratesimplificationMasterIntegral",Expand[r^2 Total[parts]],seconds];
    cm2=bounded["Verify r^-2 Coefficient",Factor[singleFraction[Coefficient[whole,r,0]]],seconds];
    cm1=bounded["Verify r^-1 Coefficient",Factor[singleFraction[Coefficient[whole,r,1]]],seconds];
    c0=bounded["VerifyFiniteCoefficient",Factor[singleFraction[Coefficient[whole,r,2]]],seconds];
    answer=If[And@@(TrueQ[#===0]&/@{cm2,cm1,c0}),"Passed","Nonzero"];
    hash=Quiet[TimeConstrained[Check[FileHash[file,"SHA256","HexString"],Missing["NotAvailable"]],
      seconds,Missing["HashTimeout"]]];
    <|"Version"->"1.2","Status"->answer,"OriginalTermCount"->39,"ReferenceGauge"->1,
      "Path"->"m^2=9 r, μ^2=r, mG^4=25 r^2, M^2=r；r→0+",
      "MomentumSquared"->-11,"ColorCount"->3,"RetainParameter"->{RGZWorkflow`Engine`α,System`D},
      "OriginalCoefficientZeroID"->zeros,"CombineMasterIntegral"->FeynCalc`B0[-11,0,0],
      "r^-2Coefficient"->prettyCoefficient[cm2],"r^-1Coefficient"->prettyCoefficient[cm1],
      "FiniteCoefficient"->prettyCoefficient[c0],
      "GaugeResiduallimit (StripPi^2gs^2)"->prettyCoefficient[c0]FeynCalc`B0[-11,0,0],
      "InputFile"->file,"InputSHA256"->hash,
      "limitConvention"->"Take the common zero-mass limit at Re[D]>6, then analytically continue in D; do not set D=4.",
      "Scope"->"Verify that the exported candidate gauge residual vanishes on this path. This does not prove the full QCD amplitude or S matrix."|>
  ]
];

Options[RGZ39QCDCheck]={"ShowProgress"->True};
RGZ39QCDCheck[file_:Automatic,seconds_:60,OptionsPattern[]]:=Module[{path,ans},
  If[!NumericQ[seconds]||!TrueQ[seconds>0],Return[Failure["InvalidTimeLimit",<||>]]];
  If[!TrueQ[RGZ39QCDRootCheck[]],
    Return[Failure["RootNormalizationFailed",<|"Description"->"The positive-scale radical self-check failed; original data was neither read nor modified."|>]]];
  If[Length[OwnValues[RGZWorkflow`Engine`α]]>0,
    Return[Failure["AssignedGaugeParameter",<|"Description"->"Run Clear[alpha] and keep the gauge parameter symbolic."|>]]];
  If[Length[OwnValues[System`D]]>0,
    Return[Failure["AssignedDimension",<|"Description"->"D has a numeric value; run this check in a kernel with symbolic D."|>]]];
  path=If[file===Automatic,FileNameJoin[{$sourceDirectory,"RGZ_Gauge_Residual_39.wxf"}],file];
  If[!StringQ[path]||!FileExistsQ[path],
    Return[Failure["InputNotFound",<|"File"->path|>]]];
  ans=If[TrueQ[OptionValue["ShowProgress"]],
    Monitor[Catch[checkCore[path,seconds],$stopTag],Style[$progress,12]],
    Catch[checkCore[path,seconds],$stopTag]];
  ans
];

RGZ39QCDSummary[ans_]:=If[AssociationQ[ans],
  Print[Grid[{
    {"CheckParts","Result"},
    {"CommonZeroMassPath",ans["Path"]},
    {"p² / N",{ans["MomentumSquared"],ans["ColorCount"]}},
    {"RetainParameter",ans["RetainParameter"]},
    {"OriginalTermCount",ans["OriginalTermCount"]},
    {"CombineMasterIntegral",TraditionalForm[ans["CombineMasterIntegral"]]},
    {"r^-2 Coefficient",TraditionalForm[ans["r^-2Coefficient"]]},
    {"r^-1 Coefficient",TraditionalForm[ans["r^-1Coefficient"]]},
    {"FiniteCoefficient",TraditionalForm[ans["FiniteCoefficient"]]},
    {"GaugeResidualCheck",ans["Status"]}
  },Frame->All,Alignment->Left,Spacings->{2,1}]],
  If[FailureQ[ans],Print["StoppedTag: ",ans[[1]]];
    Print[Grid[Prepend[List@@@Normal[ans[[2]]],{"Parts","Diagnostic"}],Frame->All,Alignment->Left]],
    Print["NotYetobtain ValidCheckResult; head: ",Head[ans]]]
];

End[];
EndPackage[];
Print["Common zero-mass QCD checker v1.2 loaded; radical self-check: ",RGZ39QCDRootCheck[],
  ". Run RGZ39QCDCheck[originalWXFPath,60]; summarize with RGZ39QCDSummary[result]."];

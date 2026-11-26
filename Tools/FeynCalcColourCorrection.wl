(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Notebook compatibility for the duplicated sunf2 contraction in
   FeynCalc 10.1.0 and 10.2.1. Only the current kernel is changed.
   f2(b,a,c,d) f(a,c,e) = -CA/2 f(b,d,e),
   f2(b,c,a,d) f(a,c,e) = +CA/2 f(b,d,e).
   The installed dependency and saved calculation results are not edited. *)

ClearAll[rgzFeynCalcColourCorrection];
rgzFeynCalcColourCorrection[] := Module[{probe, before, after, original, code},
 If[!MemberQ[$Packages,"FeynCalc`"],
  Return[Failure["FeynCalcRequired",<|"Action"->"Run RGZLoadFeynCalc[] first."|>]]];
 probe[] := Block[{$Context="RGZWorkflow`ColourCompatibility`Probe`",
    $ContextPath={"FeynCalc`","System`"}},
  ToExpression[
   "Module[{v,net,four0,four2,internal},
    v=SUNIndex /@ {RGZWorkflow`Engine`rgzGluon1,RGZWorkflow`Engine`rgzGluon2,
      RGZWorkflow`Engine`rgzGribov2,RGZWorkflow`Engine`rgzGluon3,
      RGZWorkflow`Engine`rgzGluon5,RGZWorkflow`Engine`rgzGluon4,
      RGZWorkflow`Engine`rgzGluon6,RGZWorkflow`Engine`rgzGribov3,
      RGZWorkflow`Engine`rgzMixF4$99722};
    net=Times@@(Apply[SUNF,v[[#]]]&/@{{1,4,5},{2,6,7},{4,8,9},{6,3,9},{5,7,8}});
    four0=SUNSimplify[SUNF[g0,b,c]SUNF[b,e,d]SUNF[e,g1,h]SUNF[h,c,d],SUNNToCACF->False];
    four2=SUNSimplify[SUNF[g2,b,c]SUNF[b,e,d]SUNF[e,g1,h]SUNF[h,c,d],SUNNToCACF->False];
    internal=FeynCalc`Package`colorSimplifyGeneric[
      FeynCalc`SUNSimplify`Private`sunf2[SUNIndex[aa],SUNIndex[bb],SUNIndex[cc],SUNIndex[dd]] *
      FeynCalc`Package`sunf[SUNIndex[bb],SUNIndex[cc],SUNIndex[ee]]];
    internal=internal/.FeynCalc`Package`colorSimplifyGeneric->Identity/.FeynCalc`Package`sunf->SUNF;
    <|\"Checks\"-><|
       \"FiveStructureConstants\"->TrueQ[Expand[SUNSimplify[net,SUNNToCACF->False]+SUNN^2 Apply[SUNF,Take[v,3]]/4]===0],
       \"FourStructureConstantsG0\"->TrueQ[Expand[four0-FCI[SUNN^2 SUNDelta[g0,g1]/2]]===0],
       \"FourStructureConstantsG2\"->TrueQ[Expand[four2-FCI[SUNN^2 SUNDelta[g2,g1]/2]]===0],
       \"BACDContraction\"->TrueQ[Expand[internal+SUNN SUNF[SUNIndex[aa],SUNIndex[dd],SUNIndex[ee]]/2]===0]|>,
      \"KnownFaultPresent\"->TrueQ[Expand[internal-SUNN SUNF[SUNIndex[aa],SUNIndex[dd],SUNIndex[ee]]/2]===0]|>]"]];
 before=probe[];
 If[And@@Values[before["Checks"]],
  Return[<|"Status"->"ColourRulesVerified","Correction"->"AlreadyCorrect",
    "FeynCalcVersion"->FeynCalc`$FeynCalcVersion,"Checks"->before["Checks"]|>]];
 If[!TrueQ[before["KnownFaultPresent"]],
  Return[Failure["UnexpectedFeynCalcColourRules",<|"Checks"->before["Checks"],
    "Explanation"->"The colour tests failed without matching the known duplicated rule. No change was made."|>]]];
 original=DownValues[FeynCalc`Package`colorSimplifyGeneric];
 code="colorSimplifyGeneric[rest_. sunf2[b_,a_SUNIndex,c_SUNIndex,d_] *
     sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
     SUNN/2 (-1)^(Length[{r2}]+1) * colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];
   colorSimplifyGeneric[rest_. sunf2[b_,c_SUNIndex,a_SUNIndex,d_] *
     sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
     SUNN/2 (-1)^(Length[{r2}]) * colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];";
 Block[{$Context="FeynCalc`SUNSimplify`Private`",
   $ContextPath={"FeynCalc`Package`","FeynCalc`","System`"}},ToExpression[code]];
 after=probe[];
 If[!And@@Values[after["Checks"]],
  DownValues[FeynCalc`Package`colorSimplifyGeneric]=original;
  Return[Failure["FeynCalcColourCorrectionFailed",<|"Checks"->after["Checks"],
    "Explanation"->"The original kernel rules were restored. Do not start the calculation."|>]]];
 <|"Status"->"ColourRulesVerified","Correction"->"AppliedInCurrentKernel",
   "FeynCalcVersion"->FeynCalc`$FeynCalcVersion,"Checks"->after["Checks"]|>
];

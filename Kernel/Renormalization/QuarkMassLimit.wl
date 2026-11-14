(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Expand collected QQ coefficients in M squared and restore the reference finite master formula.

   Inputs:
   A collected PV/master coefficient pair or a rational expression in e=M^2.

   Dependencies:
   RegulatorLimit.wl supplies Taylor/Laurent master rules; Algebra/LoopPrimitives.wl supplies valid-value and canonical model conventions.
*)


rgzQQM0Term[pair_,seconds_] := Module[{point,co,l,master,z},
 If[!MatchQ[pair,{_,_}] || !RGZLoopValidValueQ[pair],Return[Failure["InvalidQQM0Pair",<||>]]];
 point=<|"p^2"->qqM0S,"m^2"->qqM0X,"mu^2"->qqM0Y,"mG^4"->qqM0Z,"N"->qqM0N|>;
 (* The existing generic-D M0 prescription makes regulator tadpoles zero.
   Determine that before simplifying a potentially enormous coefficient. *)
 master=Block[{RGZResidual39Diagnostic`Private`$parameterPoint=point},RGZResidual39Diagnostic`Private`pointExpression[pair[[1]]]];
 If[MatchQ[master,RGZResidual39Diagnostic`Private`a0[_]]&&TrueQ[RGZResidual39Diagnostic`Private`pvTaylor[master]===0],Return[{0,0,0}]];
 l=rgzQQLaurentByFactors[pair[[2]],point];
 (* Fractional factors can cancel across addends. Retain the original exact
    route as a fallback rather than rejecting a globally rational input. *)
 If[FailureQ[l]&&l[[1]]==="QQSelfM0NonRational",
 co=Block[{RGZResidual39Diagnostic`Private`$parameterPoint=point},RGZResidual39Diagnostic`Private`pointExpression[pair[[2]]]];
 co=Cancel[Together[co]];If[TrueQ[co===0],Return[{0,0,0}]];
 If[PolynomialQ[Numerator[co],RGZResidual39Diagnostic`Private`e]&&PolynomialQ[Denominator[co],RGZResidual39Diagnostic`Private`e],
 If[Exponent[Numerator[co],RGZResidual39Diagnostic`Private`e,Min]-Exponent[Denominator[co],RGZResidual39Diagnostic`Private`e,Min]<-2,Return[Failure["QQSelfM0HigherPole",<||>]]];
 l=RGZResidual39Diagnostic`Private`rationalLaurent[co]]];
 If[FailureQ[l],Return[l]];
 master=Block[{RGZResidual39Diagnostic`Private`$parameterPoint=point},RGZResidual39Diagnostic`Private`pointExpression[pair[[1]]]];
 If[master=!=1&&!MatchQ[master,RGZResidual39Diagnostic`Private`a0[_]|RGZResidual39Diagnostic`Private`b0[_,_,_]],Return[Failure["QQSelfM0Master",<||>]]];
 z=Expand[RGZResidual39Diagnostic`Private`e^2 l If[master===1,1,RGZResidual39Diagnostic`Private`pvTaylor[master]]];
 Table[Coefficient[z,RGZResidual39Diagnostic`Private`e,j],{j,0,2}]];
rgzQQM0Restore[finite_]:=SMP["g_s"]^2 (RGZResidual39Diagnostic`Private`publicMaster[RGZResidual39Diagnostic`Private`prettyCoefficient[finite]]/.
 {qqM0S->SPD[p,p],qqM0X->m^2,qqM0Y->\[Mu]^2,qqM0Z->mG^4,qqM0N->SUNN});

(* A single collected PV coefficient; negative powers are checked only after
   all coefficients have been summed, so cancellations are never discarded. *)

(* Expand in e before global rational simplification. Terms sharing the same
   e-dependent factors share one exact Laurent calculation. All coefficients,
   including higher poles, are retained until their sums are checked. *)
rgzQQLaurentByFactors[input_,point_,minimumAllowed_:-2] := Module[
 {addends,groups=<||>,term,factors,dependent,independent,key,entry,series,low,
 contributions=<||>,result=0,stop=Unique["qqLaurentStop"]},Catch[
 If[!RGZLoopValidValueQ[input],Return[Failure["InvalidQQLaurentInput",<||>]]];
 addends=If[Head[input]===Plus,List@@input,{input}];
 Do[
 term=If[point===None,addend,Block[{RGZResidual39Diagnostic`Private`$parameterPoint=point},RGZResidual39Diagnostic`Private`pointExpression[addend]]];
 factors=If[Head[term]===Times,List@@term,{term}];
 independent=Times@@Select[factors,FreeQ[#,RGZResidual39Diagnostic`Private`e]&];
 dependent=Times@@Select[factors,!FreeQ[#,RGZResidual39Diagnostic`Private`e]&];
 key=Hash[dependent,"SHA256"];
 If[KeyExistsQ[groups,key],entry=groups[key];
 If[entry[[1]]=!=dependent,Throw[Failure["QQLaurentHashCollision",<||>],stop]];
 AssociateTo[groups,key->{dependent,Append[entry[[2]],independent]}],AssociateTo[groups,key->{dependent,{independent}}]],{addend,addends}];
 Do[
 dependent=Cancel[Together[entry[[1]]]];
 If[TrueQ[dependent===0],Continue[]];
 If[!PolynomialQ[Numerator[dependent],RGZResidual39Diagnostic`Private`e]||!PolynomialQ[Denominator[dependent],RGZResidual39Diagnostic`Private`e],Throw[Failure["QQSelfM0NonRational",<||>],stop]];
 low=Exponent[Numerator[dependent],RGZResidual39Diagnostic`Private`e,Min]-Exponent[Denominator[dependent],RGZResidual39Diagnostic`Private`e,Min];
 If[low>0,Continue[]];
 series=RGZResidual39Diagnostic`Private`rationalLaurent[dependent];
 independent=Total[entry[[2]]];
 Do[term=Coefficient[Expand[series RGZResidual39Diagnostic`Private`e^(-low)],RGZResidual39Diagnostic`Private`e,k-low];
 If[term=!=0,AssociateTo[contributions,k->Append[Lookup[contributions,k,{}],independent term]]],{k,low,0}],{entry,Values[groups]}];
 Do[term=Total[contributions[k]];
 If[k<minimumAllowed,If[Cancel[Together[term]]=!=0,Throw[Failure["QQSelfM0HigherPole",<|"Power"->k|>],stop]],result+=term RGZResidual39Diagnostic`Private`e^k],{k,Keys[contributions]}];
 result,stop]];

rgzQQGaugeZeroCoefficient[input_] := Module[{converted,result},
 (* alpha approaches zero from above, so sqrt(e^2)=e for e>0. *)
 converted=input/.\[Alpha]->RGZResidual39Diagnostic`Private`e^2;
 converted=converted/.HoldPattern[(RGZResidual39Diagnostic`Private`e^2)^power_Rational]:>RGZResidual39Diagnostic`Private`e^(2 power);
 result=rgzQQLaurentByFactors[converted,None,0];
 If[FailureQ[result],If[result[[1]]==="QQSelfM0NonRational",
 Limit[Cancel[Together[input]],\[Alpha]->0,Direction->"FromAbove"],ComplexInfinity],result]];

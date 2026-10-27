(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* MIXED MATRIX accelerator: independent of the AA accelerator.
   External colour tensor coefficients stay outside loop integration.
   Load AFTER RGZ_Mixed_OneLoop_Overall.wl, once the running evaluation has ended
   or has been aborted normally. No model, graph or cache identity change.
   This patch is for scalar projected one-loop two-point integrals in D
   dimensions with quadratic FAD propagators q, q-p or q+p.
   It does not change TID options or $ParallelizeFeynCalc.
   Exact numerator cancellation precedes loop reduction. Simple bubbles
   use B0; radial tadpoles use exact D-dimensional A0 identities. Genuine
   repeated-pole bubbles are sent to TID with UNIT numerator. Unsupported
   inputs or failed preprocessing fall back to the original TID call.
   The original I Pi^2 normalization and scaleless-zero convention remain.
   No RGZ pole-mass identities, numerical masses or epsilon expansion.
   Existing successful unit/graph checkpoints are reused unchanged.
   Revision 2: apply the second partial fraction expansion TERM BY TERM;
   applying it to the entire first expansion can recombine cancelled
   numerator powers and defeat scalar reduction.
   Revision 3: upgrade cached tensor-PV units into scalar sidecars and
   rebuild an unfinished scalar stage from its saved split integrand.
   Original TID/PV checkpoints are retained; no model/hash changes. *)

If[DownValues[RGZMXIntegralUnit]==={} || DownValues[RGZMXWrite]==={},
  Print["Load RGZ_Mixed_OneLoop_Overall.wl before this patch."];
  Abort[]];

ClearAll[RGZMXSetAcceleration,RGZMXFastScalar,RGZMXFastRational,RGZMXFastApart,
  RGZMXFastTerm,RGZMXFastTadpole,RGZMXFastMass,RGZMXFastEvaluate,
  RGZMXUnitDiagnostics,RGZMXIntegralUnit,rgzMXFastLastMethod,
  RGZMXFastPVCount,RGZMXUpgradeCachedUnit,RGZMXRefreshIntegral];
If[!AssociationQ[rgzMXAcceleration],rgzMXAcceleration=<|
  "Enabled"->True,"SingleKernelTimeLimit"->300,"PreprocessTimeLimit"->10|>];

Options[RGZMXSetAcceleration]={"Enabled"->True,"SingleKernelTimeLimit"->300,"PreprocessTimeLimit"->10};
RGZMXSetAcceleration[OptionsPattern[]] := Module[{unit,prep,enabled},
  unit=OptionValue["SingleKernelTimeLimit"];prep=OptionValue["PreprocessTimeLimit"];
  enabled=OptionValue["Enabled"];
  If[!MemberQ[{True,False},enabled] || !NumericQ[unit] || !TrueQ[unit>0] ||
    !NumericQ[prep] || !TrueQ[prep>0],Return[Failure["InvalidAccelerationOptions",<||>]]];
  rgzMXAcceleration=<|"Enabled"->enabled,"SingleKernelTimeLimit"->unit,"PreprocessTimeLimit"->prep|>
];

(* m is a mass, while root is its square. Never replace a labelled mass
   by Sqrt[m^2]; that needlessly introduces square-root branch choices. *)
RGZMXFastMass[root_,metadata_List] := Module[{found},
  found=Select[metadata,TrueQ[Cancel[root-#[[2]]^2]===0]&,1];
  If[found==={},Missing["UnknownPoleMass",root],found[[1,2]]]
];

RGZMXFastApart[r_,x_Symbol,y_Symbol] := Flatten[
  RGZMXTerms[Expand[Apart[#,y]]]& /@
    RGZMXTerms[Expand[Apart[r,x]]],1];

RGZMXFastRational[kernel_] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[
  {denoms,metadata={},k,cq,cp,route,shifted={},sign=-1,r,x,y,s,terms,back,bad},
  (* Loop scalar products must remain independent. Otherwise conversion
     can mistake an assigned q^2 for an external constant. *)
  If[!MatchQ[ChangeDimension[FCI[SPD[q,q]],D],
      HoldPattern[Pair[Momentum[q,D],Momentum[q,D]]]] ||
    !MatchQ[ChangeDimension[FCI[SPD[p,q]],D],HoldPattern[
      Pair[Momentum[p,D],Momentum[q,D]]|Pair[Momentum[q,D],Momentum[p,D]]]],
    Return[Failure["AssignedLoopScalarProducts",<|
      "Description"->"q^2 or p.q has an assigned value. Restore independent loop scalar products before integration."|>]]];
  (* Scalar projection is required. No Lorentz/Dirac/tensor objects. *)
  If[!FreeQ[kernel,_LorentzIndex|_ExplicitLorentzIndex|_DiracGamma|_DiracChain],
    Return[Missing["NotScalar"]]];
  denoms=Cases[ChangeDimension[FCI[kernel],D],
    HoldPattern[PropagatorDenominator[Momentum[v_,___],mass_]] :> {v,mass},Infinity];
  If[denoms==={},Return[Missing["NoQuadraticFAD"]]];
  Do[
    k=Expand[d[[1]]];
    If[!PolynomialQ[k,{p,q}],Throw[Missing["UnsupportedRouting",k],rgzFunctionReturnTag]];
    cq=Coefficient[k,q];cp=Coefficient[k,p];
    If[!MemberQ[{-1,1},cq] || !MemberQ[{-1,0,1},cp] ||
      !TrueQ[Expand[k-cq q-cp p]===0],Throw[Missing["UnsupportedRouting",k],rgzFunctionReturnTag]];
    route=cp/cq;
    AppendTo[metadata,{route,d[[2]]}];
    If[route=!=0,AppendTo[shifted,route]],{d,denoms}];
  shifted=DeleteDuplicates[shifted];
  If[Length[shifted]>1,Return[Missing["ThreeMomentumRoutes"]]];
  If[shifted=!={},sign=First[shifted]];
  r=ExpandScalarProduct[FeynAmpDenominatorExplicit[ChangeDimension[FCI[kernel],D]]];
  r=r /. {
    HoldPattern[Pair[Momentum[q,D],Momentum[q,D]]]->x,
    HoldPattern[Pair[Momentum[p,D],Momentum[p,D]]]->s,
    HoldPattern[Pair[Momentum[p,D],Momentum[q,D]]]->(y-x-s)/(2 sign),
    HoldPattern[Pair[Momentum[q,D],Momentum[p,D]]]->(y-x-s)/(2 sign)};
  If[!FreeQ[r,p|q|_FeynAmpDenominator|_Pair|_CartesianPair],
    Return[Missing["UnconvertedMomentum"]]];
  r=Cancel[Together[r]];
  If[!PolynomialQ[Numerator[r],{x,y}] || !PolynomialQ[Denominator[r],{x,y}],
    Return[Missing["NotRationalScalar"]]];
  (* x=q^2, y=(q+sign p)^2. Partial fractions have the same i0
     prescription as the original FADs; no pole is moved. *)
  terms=RGZMXFastApart[r,x,y];
  back=Cancel[Together[r-Total[terms]]];
  If[!TrueQ[back===0],Return[Missing["RationalIdentityNotVerified"]]];
  <|"OriginalExpression"->r,"Contributions"->terms,"Variable"->{x,y,s},
    "Mass"->DeleteDuplicates[metadata],"ChangeOfVariablesReconstructionDifference"->back|>
],rgzFunctionReturnTag]];

(* D-dimensional angular average, including a shifted tadpole:
   <(p.r)^(2n)>=(2n-1)!! (p^2 r^2)^n/[D(D+2)...].
   For q=r+-p, either sign gives the same average of (q^2)^k.
   Repeated poles follow by mass-square differentiation of A0, retaining
   the exact D dependence rather than setting D=4 in divergent terms. *)
RGZMXFastTadpole[poly_,var_,other_,s_,root_,order_Integer] := Module[
  {u,z,avg,angular,n,k,out=0,coeff,radial},
  If[TrueQ[root===0],Return[0]]; (* massless tadpoles are scaleless *)
  If[!PolynomialQ[poly,{var,other}],Return[Missing["NonPolynomialTadpole"]]];
  angular=Expand[poly/.{var->u,other->u+s+2 z}];
  n=If[TrueQ[angular===0],0,Exponent[angular,z]];
  avg[j_Integer] := Which[j===0,1,OddQ[j],0,
    True,Factorial[j] (s u)^(j/2)/
      (2^(j/2) Factorial[j/2] Product[D+2 t,{t,0,j/2-1}])];
  angular=Expand[Total[Table[Coefficient[angular,z,j] avg[j],{j,0,n}]]];
  If[!PolynomialQ[angular,u],Return[Missing["AngularAverageNotPolynomial"]]];
  n=If[TrueQ[angular===0],0,Exponent[angular,u]];
  Do[
    coeff=Coefficient[angular,u,k];
    radial=Total[Table[Binomial[k,j] root^(k-j) *
      Product[D/2-1-t,{t,0,order-j-2}]/((order-j-1)! root^(order-j-1)),
      {j,0,Min[k,order-1]}]];
    out+=coeff radial,{k,0,n}];
  I Pi^2 A0[root] Cancel[Together[out]]
];

RGZMXFastTerm[term_,variables_List,metadata_List] := Module[{rgzFunctionReturnTag=Unique["RGZWorkflow`Private`functionReturn"]},Catch[
Module[
  {x=variables[[1]],y=variables[[2]],s=variables[[3]],num,den,factors,
   factor,power,c,root,rx=None,ry=None,nx=0,ny=0,scale=1,z,mx,my,fd},
  z=Cancel[Together[term]];num=Numerator[z];den=Denominator[z];
  If[!PolynomialQ[num,{x,y}],Return[Missing["NonPolynomialNumerator"]]];
  factors=FactorList[den];scale=factors[[1,1]];
  Do[
    {factor,power}=item;
    Which[
      FreeQ[factor,x|y],scale*=factor^power,
      FreeQ[factor,y] && Exponent[factor,x]===1,
        If[nx=!=0,Throw[Missing["MultipleXPoles"],rgzFunctionReturnTag]];
        c=Coefficient[factor,x];root=Cancel[-Coefficient[factor,x,0]/c];
        rx=root;nx=power;scale*=c^power,
      FreeQ[factor,x] && Exponent[factor,y]===1,
        If[ny=!=0,Throw[Missing["MultipleYPoles"],rgzFunctionReturnTag]];
        c=Coefficient[factor,y];root=Cancel[-Coefficient[factor,y,0]/c];
        ry=root;ny=power;scale*=c^power,
      True,Throw[Missing["NonseparableDenominator",factor],rgzFunctionReturnTag]],{item,Rest[factors]}];
  num/=scale;
  Which[
    nx===0 && ny===0,0,
    nx>0 && ny===0,RGZMXFastTadpole[num,x,y,s,rx,nx],
    nx===0 && ny>0,RGZMXFastTadpole[num,y,x,s,ry,ny],
    True,
      If[!FreeQ[num,x|y],Return[Missing["BubbleNumeratorNotCancelled"]]];
      If[nx===1 && ny===1,Return[I Pi^2 num B0[s,rx,ry]]];
      mx=RGZMXFastMass[rx,Select[metadata,#[[1]]===0&]];
      my=RGZMXFastMass[ry,Select[metadata,#[[1]]=!=0&]];
      If[Head[mx]===Missing || Head[my]===Missing,Return[Missing["MassLabelMissing"]]];
      (* Only genuine repeated poles reach TID, with rank-zero numerator. *)
      fd=ChangeDimension[FCI[FAD@@Join[ConstantArray[{q,mx},nx],
        ConstantArray[{q-p,my},ny]]],D];
      RGZMXProgressUpdate["Substep"->"Repeated pole: TID without a momentum numerator"];
      num TID[fd,q,ToPaVe->True,UsePaVeBasis->True,ApartFF->False]
  ]
],rgzFunctionReturnTag]];

(* Bound algebraic preprocessing separately. An unsupported term sends
   the WHOLE original kernel to ordinary TID; partial results are not
   mixed with a second evaluation of the original kernel. *)
RGZMXFastScalar[kernel_] := Module[{data,terms,result,secs},
  secs=rgzMXAcceleration["PreprocessTimeLimit"];
  data=CheckAbort[TimeConstrained[RGZMXFastRational[kernel],secs,
    Missing["PreparationTimeout"]],$Aborted];
  If[data===$Aborted,Return[$Aborted]];
  If[Head[data]===Missing || FailureQ[data],Return[data]];
  (* An unsupported term sends the whole unit back to ordinary TID. *)
  result=RGZMXFastTerm[#,data["Variable"],data["Mass"]]&/@data["Contributions"];
  If[MemberQ[result,$Aborted],Return[$Aborted]];
  If[AnyTrue[result,Head[#]===Missing || FailureQ[#]&],
    Return[Missing["ScalarReductionUnsupported"]]];
  terms=Total[result]/.data["Variable"][[3]]->SPD[p,p];
  If[!FreeQ[terms,data["Variable"][[1]]|data["Variable"][[2]]|_TID|_FeynAmpDenominator],
    Return[Missing["ScalarReductionIncomplete"]]];
  FCI[terms]
];

RGZMXFastEvaluate[kernel_] := Module[{r},
  rgzMXFastLastMethod="TID";
  If[TrueQ[rgzMXAcceleration["Enabled"]],
    RGZMXProgressUpdate["Substep"->"ScalarIdentity: eliminate MomentumNumerator"];
    r=RGZMXFastScalar[kernel];
    If[r===$Aborted,Return[$Aborted]];
    If[MatchQ[r,Failure["AssignedLoopScalarProducts",_]],Return[r]];
    If[Head[r]=!=Missing && !FailureQ[r] && r=!=$Failed,
      rgzMXFastLastMethod="ScalarIdentityReduce";Return[r]]];
  RGZMXProgressUpdate["Substep"->"TID (ScalarPreprocessNotapplicable)"];
  TID[kernel,q,ToPaVe->True,UsePaVeBasis->True,ApartFF->False]
];

RGZMXFastPVCount[expr_] := Length[DeleteDuplicates[Cases[expr,
  x_ /; RGZMXPVObjectQ[x] && !RGZMXPVScalarQ[x],{0,Infinity}]]];

(* A cached TID result can contain hundreds of tensor coefficient functions
   even though its original projected integrand is scalar. Upgrade only
   such units, and keep the ORIGINAL unit file unchanged. Sidecars are
   bound to the exact input, old environment and fast reducer definitions.
   Unsupported/slow upgrades retain the original valid TID result. *)
RGZMXUpgradeCachedUnit[old_Association,path_,context_,seconds_] := Module[
  {original=old["Result"],fastContext,upgradePath=None,saved,z,time,
   limit,record,r,good},
  If[!TrueQ[rgzMXAcceleration["Enabled"]] || RGZMXFastPVCount[original]===0,
    Return[original]];
  limit=Min[seconds,rgzMXAcceleration["PreprocessTimeLimit"]];
  fastContext=Join[context,{"AA-SCALAR-SIDECAR-v3",limit,
    Hash[{DownValues[RGZMXFastScalar],DownValues[RGZMXFastRational],
      DownValues[RGZMXFastApart],DownValues[RGZMXFastTerm],
      DownValues[RGZMXFastTadpole],DownValues[RGZMXFastMass]},"SHA256"]}];
  If[StringQ[path],upgradePath=path<>".scalar_v3.wl"];
  If[StringQ[upgradePath] && FileExistsQ[upgradePath],saved=Get[upgradePath];
    If[AssociationQ[saved] && Lookup[saved,"Version",0]===3 &&
      SameQ[Lookup[saved,"Input",Missing[]],old["Input"]] &&
      SameQ[Lookup[saved,"Environment",Missing[]],fastContext],
      If[Lookup[saved,"Status",""]==="Complete" && KeyExistsQ[saved,"Result"],
        AssociateTo[rgzMXLastIntegralUnit,"Algorithm"->"ScalarCacheRestored"];
        Return[saved["Result"]]];
      If[Lookup[saved,"Status",""]==="RetainOriginalPVResult",Return[original]]]];
  RGZMXProgressUpdate["Substep"->"LegacyIntegrateKernel: direct ScalarReduce"];
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    RGZMXFastScalar[old["Input"]],limit,Missing["UpgradeTimeout"]],$Aborted]];
  (* A normal user abort stops the graph; a bounded unsupported upgrade
     is safe to replace by the original, fully evaluated TID checkpoint. *)
  If[z===$Aborted,Return[Failure["ScalarUpgradeAborted",<||>]]];
  If[MatchQ[z,Failure["AssignedLoopScalarProducts",_]],Return[z]];
  good=z=!=$Failed && !FailureQ[z] &&
    FreeQ[z,_Missing|_TID|_PaVeReduce|_FeynAmpDenominator] &&
    RGZMXFastPVCount[z]===0;
  record=<|"Version"->3,"Input"->old["Input"],"Environment"->fastContext,
    "Status"->If[good,"Complete","RetainOriginalPVResult"],"ElapsedSeconds"->time,
    "OriginalCache"->path,"Result"->If[good,z,original]|>;
  If[!good,AssociateTo[record,"UpgradeIncompleteCause"->z]];
  If[StringQ[upgradePath],r=RGZMXWrite[record,upgradePath];
    If[FailureQ[r],Return[r]]];
  AssociateTo[rgzMXLastIntegralUnit,{"Algorithm"->If[good,
    "LegacyCacheconvertIsScalarProductpart","reuseOriginalPVCache"],"UpgradeElapsedSeconds"->time}];
  If[good,z,original]
];

(* Exact old input/context/hash are preserved: cached results remain valid.
   A per-unit cap is added INSIDE the caller's whole-stage time limit. *)
RGZMXIntegralUnit[kernel_,path_,context_,seconds_] := Module[
  {old,record,z,time,den,r,status,unitSeconds},
  If[StringQ[path] && FileExistsQ[path],old=Get[path];
    If[AssociationQ[old] && Lookup[old,"Version",0]===1 &&
      Lookup[old,"Status",""]==="Complete" &&
      SameQ[Lookup[old,"Input",Missing[]],kernel] &&
      SameQ[Lookup[old,"Environment",Missing[]],context] && KeyExistsQ[old,"Result"],
      AssociateTo[rgzMXLastIntegralUnit,"Status"->"RestoreCache"];
      Return[RGZMXUpgradeCachedUnit[old,path,context,seconds]]]];
  record=<|"Version"->1,"Input"->kernel,"Environment"->context,"Status"->"process In"|>;
  If[StringQ[path],r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]]];
  unitSeconds=Min[seconds,rgzMXAcceleration["SingleKernelTimeLimit"]];
  rgzMXFastLastMethod="NotStarted";
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    If[FreeQ[kernel,_FeynAmpDenominator],
      den=Denominator[Together[kernel]];
      If[!FreeQ[den,q],Failure["UnsplitLoopDenominator",<|"InputFile"->path|>],0],
      RGZMXFastEvaluate[kernel]],unitSeconds,$Aborted],$Aborted]];
  If[z=!=$Aborted && z=!=$Failed && !FailureQ[z] &&
    !FreeQ[z,_TID|_FeynAmpDenominator|_Missing],
    z=Failure["IncompleteScalarReduction",<|"InputFile"->path|>]];
  status=Which[z===$Aborted,"TimeoutOrAborted",z===$Failed || FailureQ[z],"ComputeFailed",True,"Complete"];
  record=Join[record,<|"Status"->status,"ElapsedSeconds"->time,"Result"->z,
    "Algorithm"->rgzMXFastLastMethod,"SingleKernelTimeLimit"->unitSeconds|>];
  AssociateTo[rgzMXLastIntegralUnit,{"Status"->status,"ElapsedSeconds"->time,
    "Algorithm"->rgzMXFastLastMethod,"SingleKernelTimeLimit"->unitSeconds}];
  If[StringQ[path],r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]]];
  If[status==="Complete",z,Failure["IntegralUnitStopped",<|
    "TermID"->Lookup[rgzMXLastIntegralUnit,"TermID"],"InputFile"->path,
    "Status"->status,"Cause"->z,"SingleKernelTimeLimit"->unitSeconds|>]]
];

RGZMXUnitDiagnostics[] := Module[{a,path,r,objects,momenta},
  If[!AssociationQ[rgzMXLastIntegralUnit],Return[Missing["NoUnitRun"]]];
  a=rgzMXLastIntegralUnit;path=Lookup[a,"InputFile",None];
  r=If[StringQ[path] && FileExistsQ[path],Get[path],<||>];
  Join[KeyTake[a,{"TermID","TotalTermCount","InputFile","InputSize","Status","ElapsedSeconds","Algorithm","SingleKernelTimeLimit"}],
    If[AssociationQ[r] && KeyExistsQ[r,"Input"],
      objects=Cases[r["Input"],_PropagatorDenominator,Infinity];
      <|"PropagatorCount"->Length[objects],"Propagator"->DeleteDuplicates[objects],
        "LoopMomentumNumerator"->(r["Input"]/. _FeynAmpDenominator->1)|>,<||>]]
];

(* Rebuild ONLY the integral representation for one unfinished T/L scalar
   stage. Splitting, projection, other graphs and completed scalar stages
   are untouched. Commit the replacement graph only after every integral
   unit succeeded; original TID and PV unit files remain available. *)
RGZMXRefreshIntegral[n_Integer,component_String:"T",seconds_:600] := Module[
  {row,states,splitStage,integralStage,scalarStage,oldCount,newCount,z,time,
   path,newRow,all,r,result},
  If[!MemberQ[{"T","L"},component] || !NumericQ[seconds] || !TrueQ[seconds>0],
    Return[Failure["RefreshArguments",<||>]]];
  If[!AssociationQ[rgzMXJob],Return[Failure["StartFirst",<||>]]];
  If[n<1 || n>Length[rgzMXJob["PerDiagram"]],Return[Failure["GraphNumber",<||>]]];
  row=RGZMXMigrateRow[rgzMXJob["PerDiagram"][[n]]];states=row["Status"];
  splitStage="Decompose"<>component;integralStage="Integrate"<>component;
  scalarStage="Scalar"<>component;
  If[Lookup[states,scalarStage,""]==="Complete",
    Return[<|"ID"->n,"Stage"->scalarStage,"Status"->"Complete; skipped"|>]];
  If[Lookup[states,splitStage,""]=!="Complete" || !KeyExistsQ[row,splitStage],
    Return[Failure["RunPreviousStage",<|"MissingStage"->{splitStage}|>]]];
  oldCount=If[KeyExistsQ[row,integralStage],RGZMXFastPVCount[row[integralStage]],
    Missing["IntegralNotRun"]];
  RGZMXProgressUpdate["Sector"->row["Category"],"DiagramID"->n,
    "DiagramCount"->Length[rgzMXJob["PerDiagram"]],"Stage"->"Reorganize"<>integralStage,
    "Substep"->"organize LegacyIntegrateKernel","TermID"->0,"TotalTermCount"->0,"CompleteTermCount"->0,
    "Status"->"ComputeIn","StageStartTime"->AbsoluteTime[]];
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    RGZMXIntegrate[row[splitStage],
      FileNameJoin[{rgzMXJob["CacheDirectory"],"tid_units_v1"}],seconds],
    seconds,$Aborted],$Aborted]];
  If[z===$Aborted || z===$Failed || FailureQ[z],
    RGZMXProgressUpdate["Status"->"Reorganization is incomplete; original diagram records are retained"];
    Return[Failure["IntegralRefreshStopped",<|"ID"->n,
      "Stage"->integralStage,"ElapsedSeconds"->time,"Cause"->z|>]]];
  newCount=RGZMXFastPVCount[z];
  states=Join[states,<|integralStage->"Complete",scalarStage->"NotRun"|>];
  result=<|"ID"->n,"Stage"->integralStage,"Status"->"ReorganizeComplete",
    "ElapsedSeconds"->time,"ReorganizeBeforepending ReducePVCount"->oldCount,"ReorganizeAfterpending ReducePVCount"->newCount|>;
  newRow=Join[KeyDrop[row,{scalarStage}],<|integralStage->z,
    "Status"->states,"ScalarCacheReorganize"->Join[Lookup[row,"ScalarCacheReorganize",<||>],
      <|component->result|>]|>];
  path=FileNameJoin[{rgzMXJob["CacheDirectory"],"graph_"<>ToString[n]<>".wl"}];
  r=RGZMXWrite[newRow,path];If[FailureQ[r],Return[r]];
  all=rgzMXJob["PerDiagram"];all[[n]]=newRow;
  AssociateTo[rgzMXJob,"PerDiagram"->all];
  RGZMXProgressUpdate["Status"->"Complete"];
  result
];


(* Preserve the current original private graph runner. Re-loading THIS
   patch must not wrap its wrapper recursively. If the base workflow was
   reloaded, its new original implementation replaces the snapshot. *)
If[FreeQ[DownValues[RGZMXRun],RGZMXRunAccelerated],
  DownValues[RGZMXRunBase]=DownValues[RGZMXRun]/.RGZMXRun->RGZMXRunBase];

ClearAll[RGZMXRunAccelerated,RGZMixSetAcceleration,
  RGZMixRefreshIntegral,RGZMixUnitDiagnostics];
RGZMXRunAccelerated[n_Integer,stage_String,seconds_:300] := Module[
  {row,component,integralStage,splitStage,refreshRecord,r},
  If[TrueQ[rgzMXAcceleration["Enabled"]] &&
      MemberQ[{"ScalarT","ScalarL"},stage] && AssociationQ[rgzMXJob] &&
      1<=n<=Length[rgzMXJob["PerDiagram"]],
    row=RGZMXMigrateRow[rgzMXJob["PerDiagram"][[n]]];
    component=StringTake[stage,-1];
    integralStage="Integrate"<>component;splitStage="Decompose"<>component;
    refreshRecord=Lookup[Lookup[row,"ScalarCacheReorganize",<||>],component,<||>];
    If[Lookup[row["Status"],stage,""]=!="Complete" &&
      Lookup[row["Status"],splitStage,""]==="Complete" &&
      Lookup[row["Status"],integralStage,""]==="Complete" &&
      KeyExistsQ[row,integralStage] &&
      Lookup[refreshRecord,"Status",""]=!="ReorganizeComplete" &&
      RGZMXFastPVCount[row[integralStage]]>0,
      r=RGZMXRefreshIntegral[n,component,seconds];
      If[FailureQ[r],Return[r]]]];
  RGZMXRunBase[n,stage,seconds]
];
RGZMXRun[n_Integer,stage_String,seconds_:300] :=
  RGZMXRunAccelerated[n,stage,seconds];

Options[RGZMixSetAcceleration]=Options[RGZMXSetAcceleration];
RGZMixSetAcceleration[opts___Rule] := RGZMXSetAcceleration[opts];
RGZMixRefreshIntegral[n_Integer,component_String:"T",seconds_:600] :=
  RGZMXRefreshIntegral[n,component,seconds];
RGZMixUnitDiagnostics[] := RGZMXUnitDiagnostics[];

RGZMXReport["Summary",
  "Scalar accelerator loaded. Original caches are retained; incomplete scalar stages reorganize old integrals. Configure with RGZMixSetAcceleration[\"SingleKernelTimeLimit\"->120,\"PreprocessTimeLimit\"->15]."];

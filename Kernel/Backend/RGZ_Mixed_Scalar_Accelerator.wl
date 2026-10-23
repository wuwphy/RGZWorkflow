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
  Print["\:8bf7\:5148\:8f7d\:5165 RGZ_Mixed_OneLoop_Overall.wl\:ff0c\:518d\:8f7d\:5165\:672c\:8865\:4e01\:3002"];
  Abort[]];

ClearAll[RGZMXSetAcceleration,RGZMXFastScalar,RGZMXFastRational,RGZMXFastApart,
  RGZMXFastTerm,RGZMXFastTadpole,RGZMXFastMass,RGZMXFastEvaluate,
  RGZMXUnitDiagnostics,RGZMXIntegralUnit,rgzMXFastLastMethod,
  RGZMXFastPVCount,RGZMXUpgradeCachedUnit,RGZMXRefreshIntegral];
If[!AssociationQ[rgzMXAcceleration],rgzMXAcceleration=<|
  "\:542f\:7528"->True,"\:5355\:6838\:65f6\:9650"->300,"\:9884\:5904\:7406\:65f6\:9650"->10|>];

Options[RGZMXSetAcceleration]={"\:542f\:7528"->True,"\:5355\:6838\:65f6\:9650"->300,"\:9884\:5904\:7406\:65f6\:9650"->10};
RGZMXSetAcceleration[OptionsPattern[]] := Module[{unit,prep,enabled},
  unit=OptionValue["\:5355\:6838\:65f6\:9650"];prep=OptionValue["\:9884\:5904\:7406\:65f6\:9650"];
  enabled=OptionValue["\:542f\:7528"];
  If[!MemberQ[{True,False},enabled] || !NumericQ[unit] || !TrueQ[unit>0] ||
    !NumericQ[prep] || !TrueQ[prep>0],Return[Failure["InvalidAccelerationOptions",<||>]]];
  rgzMXAcceleration=<|"\:542f\:7528"->enabled,"\:5355\:6838\:65f6\:9650"->unit,"\:9884\:5904\:7406\:65f6\:9650"->prep|>
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

RGZMXFastRational[kernel_] := Module[
  {denoms,metadata={},k,cq,cp,route,shifted={},sign=-1,r,x,y,s,terms,back,bad},
  (* Loop scalar products must remain independent. Otherwise conversion
     can mistake an assigned q^2 for an external constant. *)
  If[!MatchQ[ChangeDimension[FCI[SPD[q,q]],D],
      HoldPattern[Pair[Momentum[q,D],Momentum[q,D]]]] ||
    !MatchQ[ChangeDimension[FCI[SPD[p,q]],D],HoldPattern[
      Pair[Momentum[p,D],Momentum[q,D]]|Pair[Momentum[q,D],Momentum[p,D]]]],
    Return[Failure["AssignedLoopScalarProducts",<|
      "\:8bf4\:660e"->"q^2 \:6216 p.q \:5df2\:88ab\:8d4b\:503c\:ff1b\:5148\:6062\:590d\:72ec\:7acb\:5708\:52a8\:91cf\:6807\:91cf\:79ef\:ff0c\:518d\:79ef\:5206\:3002"|>]]];
  (* Scalar projection is required. No Lorentz/Dirac/tensor objects. *)
  If[!FreeQ[kernel,_LorentzIndex|_ExplicitLorentzIndex|_DiracGamma|_DiracChain],
    Return[Missing["NotScalar"]]];
  denoms=Cases[ChangeDimension[FCI[kernel],D],
    HoldPattern[PropagatorDenominator[Momentum[v_,___],mass_]] :> {v,mass},Infinity];
  If[denoms==={},Return[Missing["NoQuadraticFAD"]]];
  Do[
    k=Expand[d[[1]]];
    If[!PolynomialQ[k,{p,q}],Return[Missing["UnsupportedRouting",k]]];
    cq=Coefficient[k,q];cp=Coefficient[k,p];
    If[!MemberQ[{-1,1},cq] || !MemberQ[{-1,0,1},cp] ||
      !TrueQ[Expand[k-cq q-cp p]===0],Return[Missing["UnsupportedRouting",k]]];
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
  <|"\:539f\:5f0f"->r,"\:5206\:9879"->terms,"\:53d8\:91cf"->{x,y,s},
    "\:8d28\:91cf"->DeleteDuplicates[metadata],"\:6362\:5143\:91cd\:6784\:5dee\:503c"->back|>
];

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

RGZMXFastTerm[term_,variables_List,metadata_List] := Module[
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
        If[nx=!=0,Return[Missing["MultipleXPoles"]]];
        c=Coefficient[factor,x];root=Cancel[-Coefficient[factor,x,0]/c];
        rx=root;nx=power;scale*=c^power,
      FreeQ[factor,x] && Exponent[factor,y]===1,
        If[ny=!=0,Return[Missing["MultipleYPoles"]]];
        c=Coefficient[factor,y];root=Cancel[-Coefficient[factor,y,0]/c];
        ry=root;ny=power;scale*=c^power,
      True,Return[Missing["NonseparableDenominator",factor]]],{item,Rest[factors]}];
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
      RGZMXProgressUpdate["\:5b50\:6b65\:9aa4"->"\:91cd\:590d\:6781\:70b9\:ff1a\:65e0\:52a8\:91cf\:5206\:5b50\:7684 TID"];
      num TID[fd,q,ToPaVe->True,UsePaVeBasis->True,ApartFF->False]
  ]
];

(* Bound algebraic preprocessing separately. An unsupported term sends
   the WHOLE original kernel to ordinary TID; partial results are not
   mixed with a second evaluation of the original kernel. *)
RGZMXFastScalar[kernel_] := Module[{data,terms,result,secs},
  secs=rgzMXAcceleration["\:9884\:5904\:7406\:65f6\:9650"];
  data=CheckAbort[TimeConstrained[RGZMXFastRational[kernel],secs,
    Missing["PreparationTimeout"]],$Aborted];
  If[data===$Aborted,Return[$Aborted]];
  If[Head[data]===Missing || FailureQ[data],Return[data]];
  (* An unsupported term sends the whole unit back to ordinary TID. *)
  result=RGZMXFastTerm[#,data["\:53d8\:91cf"],data["\:8d28\:91cf"]]&/@data["\:5206\:9879"];
  If[MemberQ[result,$Aborted],Return[$Aborted]];
  If[AnyTrue[result,Head[#]===Missing || FailureQ[#]&],
    Return[Missing["ScalarReductionUnsupported"]]];
  terms=Total[result]/.data["\:53d8\:91cf"][[3]]->SPD[p,p];
  If[!FreeQ[terms,data["\:53d8\:91cf"][[1]]|data["\:53d8\:91cf"][[2]]|_TID|_FeynAmpDenominator],
    Return[Missing["ScalarReductionIncomplete"]]];
  FCI[terms]
];

RGZMXFastEvaluate[kernel_] := Module[{r},
  rgzMXFastLastMethod="TID";
  If[TrueQ[rgzMXAcceleration["\:542f\:7528"]],
    RGZMXProgressUpdate["\:5b50\:6b65\:9aa4"->"\:6807\:91cf\:6052\:7b49\:5f0f\:ff1a\:6d88\:53bb\:52a8\:91cf\:5206\:5b50"];
    r=RGZMXFastScalar[kernel];
    If[r===$Aborted,Return[$Aborted]];
    If[MatchQ[r,Failure["AssignedLoopScalarProducts",_]],Return[r]];
    If[Head[r]=!=Missing && !FailureQ[r] && r=!=$Failed,
      rgzMXFastLastMethod="\:6807\:91cf\:6052\:7b49\:5f0f\:7ea6\:5316";Return[r]]];
  RGZMXProgressUpdate["\:5b50\:6b65\:9aa4"->"TID\:ff08\:6807\:91cf\:9884\:5904\:7406\:4e0d\:9002\:7528\:ff09"];
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
  {original=old["\:7ed3\:679c"],fastContext,upgradePath=None,saved,z,time,
   limit,record,r,good},
  If[!TrueQ[rgzMXAcceleration["\:542f\:7528"]] || RGZMXFastPVCount[original]===0,
    Return[original]];
  limit=Min[seconds,rgzMXAcceleration["\:9884\:5904\:7406\:65f6\:9650"]];
  fastContext=Join[context,{"AA-SCALAR-SIDECAR-v3",limit,
    Hash[{DownValues[RGZMXFastScalar],DownValues[RGZMXFastRational],
      DownValues[RGZMXFastApart],DownValues[RGZMXFastTerm],
      DownValues[RGZMXFastTadpole],DownValues[RGZMXFastMass]},"SHA256"]}];
  If[StringQ[path],upgradePath=path<>".scalar_v3.wl"];
  If[StringQ[upgradePath] && FileExistsQ[upgradePath],saved=Get[upgradePath];
    If[AssociationQ[saved] && Lookup[saved,"\:7248\:672c",0]===3 &&
      SameQ[Lookup[saved,"\:8f93\:5165",Missing[]],old["\:8f93\:5165"]] &&
      SameQ[Lookup[saved,"\:73af\:5883",Missing[]],fastContext],
      If[Lookup[saved,"\:72b6\:6001",""]==="\:5b8c\:6210" && KeyExistsQ[saved,"\:7ed3\:679c"],
        AssociateTo[rgzMXLastIntegralUnit,"\:7b97\:6cd5"->"\:6807\:91cf\:7f13\:5b58\:6062\:590d"];
        Return[saved["\:7ed3\:679c"]]];
      If[Lookup[saved,"\:72b6\:6001",""]==="\:4fdd\:7559\:539fPV\:7ed3\:679c",Return[original]]]];
  RGZMXProgressUpdate["\:5b50\:6b65\:9aa4"->"\:65e7\:79ef\:5206\:6838\:ff1a\:76f4\:63a5\:6807\:91cf\:7ea6\:5316"];
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    RGZMXFastScalar[old["\:8f93\:5165"]],limit,Missing["UpgradeTimeout"]],$Aborted]];
  (* A normal user abort stops the graph; a bounded unsupported upgrade
     is safe to replace by the original, fully evaluated TID checkpoint. *)
  If[z===$Aborted,Return[Failure["ScalarUpgradeAborted",<||>]]];
  If[MatchQ[z,Failure["AssignedLoopScalarProducts",_]],Return[z]];
  good=z=!=$Failed && !FailureQ[z] &&
    FreeQ[z,_Missing|_TID|_PaVeReduce|_FeynAmpDenominator] &&
    RGZMXFastPVCount[z]===0;
  record=<|"\:7248\:672c"->3,"\:8f93\:5165"->old["\:8f93\:5165"],"\:73af\:5883"->fastContext,
    "\:72b6\:6001"->If[good,"\:5b8c\:6210","\:4fdd\:7559\:539fPV\:7ed3\:679c"],"\:8017\:65f6"->time,
    "\:539f\:59cb\:7f13\:5b58"->path,"\:7ed3\:679c"->If[good,z,original]|>;
  If[!good,AssociateTo[record,"\:5347\:7ea7\:672a\:5b8c\:6210\:539f\:56e0"->z]];
  If[StringQ[upgradePath],r=RGZMXWrite[record,upgradePath];
    If[FailureQ[r],Return[r]]];
  AssociateTo[rgzMXLastIntegralUnit,{"\:7b97\:6cd5"->If[good,
    "\:65e7\:7f13\:5b58\:8f6c\:4e3a\:6807\:91cf\:79ef\:5206","\:590d\:7528\:539fPV\:7f13\:5b58"],"\:5347\:7ea7\:8017\:65f6"->time}];
  If[good,z,original]
];

(* Exact old input/context/hash are preserved: cached results remain valid.
   A per-unit cap is added INSIDE the caller's whole-stage time limit. *)
RGZMXIntegralUnit[kernel_,path_,context_,seconds_] := Module[
  {old,record,z,time,den,r,status,unitSeconds},
  If[StringQ[path] && FileExistsQ[path],old=Get[path];
    If[AssociationQ[old] && Lookup[old,"\:7248\:672c",0]===1 &&
      Lookup[old,"\:72b6\:6001",""]==="\:5b8c\:6210" &&
      SameQ[Lookup[old,"\:8f93\:5165",Missing[]],kernel] &&
      SameQ[Lookup[old,"\:73af\:5883",Missing[]],context] && KeyExistsQ[old,"\:7ed3\:679c"],
      AssociateTo[rgzMXLastIntegralUnit,"\:72b6\:6001"->"\:6062\:590d\:7f13\:5b58"];
      Return[RGZMXUpgradeCachedUnit[old,path,context,seconds]]]];
  record=<|"\:7248\:672c"->1,"\:8f93\:5165"->kernel,"\:73af\:5883"->context,"\:72b6\:6001"->"\:5904\:7406\:4e2d"|>;
  If[StringQ[path],r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]]];
  unitSeconds=Min[seconds,rgzMXAcceleration["\:5355\:6838\:65f6\:9650"]];
  rgzMXFastLastMethod="\:672a\:5f00\:59cb";
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    If[FreeQ[kernel,_FeynAmpDenominator],
      den=Denominator[Together[kernel]];
      If[!FreeQ[den,q],Failure["UnsplitLoopDenominator",<|"\:8f93\:5165\:6587\:4ef6"->path|>],0],
      RGZMXFastEvaluate[kernel]],unitSeconds,$Aborted],$Aborted]];
  If[z=!=$Aborted && z=!=$Failed && !FailureQ[z] &&
    !FreeQ[z,_TID|_FeynAmpDenominator|_Missing],
    z=Failure["IncompleteScalarReduction",<|"\:8f93\:5165\:6587\:4ef6"->path|>]];
  status=Which[z===$Aborted,"\:8d85\:65f6\:6216\:4e2d\:6b62",z===$Failed || FailureQ[z],"\:8ba1\:7b97\:5931\:8d25",True,"\:5b8c\:6210"];
  record=Join[record,<|"\:72b6\:6001"->status,"\:8017\:65f6"->time,"\:7ed3\:679c"->z,
    "\:7b97\:6cd5"->rgzMXFastLastMethod,"\:5355\:6838\:65f6\:9650"->unitSeconds|>];
  AssociateTo[rgzMXLastIntegralUnit,{"\:72b6\:6001"->status,"\:8017\:65f6"->time,
    "\:7b97\:6cd5"->rgzMXFastLastMethod,"\:5355\:6838\:65f6\:9650"->unitSeconds}];
  If[StringQ[path],r=RGZMXWrite[record,path];If[FailureQ[r],Return[r]]];
  If[status==="\:5b8c\:6210",z,Failure["IntegralUnitStopped",<|
    "\:9879\:53f7"->Lookup[rgzMXLastIntegralUnit,"\:9879\:53f7"],"\:8f93\:5165\:6587\:4ef6"->path,
    "\:72b6\:6001"->status,"\:539f\:56e0"->z,"\:5355\:6838\:65f6\:9650"->unitSeconds|>]]
];

RGZMXUnitDiagnostics[] := Module[{a,path,r,objects,momenta},
  If[!AssociationQ[rgzMXLastIntegralUnit],Return[Missing["NoUnitRun"]]];
  a=rgzMXLastIntegralUnit;path=Lookup[a,"\:8f93\:5165\:6587\:4ef6",None];
  r=If[StringQ[path] && FileExistsQ[path],Get[path],<||>];
  Join[KeyTake[a,{"\:9879\:53f7","\:603b\:9879\:6570","\:8f93\:5165\:6587\:4ef6","\:8f93\:5165\:5927\:5c0f","\:72b6\:6001","\:8017\:65f6","\:7b97\:6cd5","\:5355\:6838\:65f6\:9650"}],
    If[AssociationQ[r] && KeyExistsQ[r,"\:8f93\:5165"],
      objects=Cases[r["\:8f93\:5165"],_PropagatorDenominator,Infinity];
      <|"\:4f20\:64ad\:5b50\:6570"->Length[objects],"\:4f20\:64ad\:5b50"->DeleteDuplicates[objects],
        "\:5708\:52a8\:91cf\:5206\:5b50"->(r["\:8f93\:5165"]/. _FeynAmpDenominator->1)|>,<||>]]
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
  If[n<1 || n>Length[rgzMXJob["\:9010\:56fe"]],Return[Failure["GraphNumber",<||>]]];
  row=RGZMXMigrateRow[rgzMXJob["\:9010\:56fe"][[n]]];states=row["\:72b6\:6001"];
  splitStage="\:5206\:89e3"<>component;integralStage="\:79ef\:5206"<>component;
  scalarStage="\:6807\:91cf"<>component;
  If[Lookup[states,scalarStage,""]==="\:5b8c\:6210",
    Return[<|"\:7f16\:53f7"->n,"\:9636\:6bb5"->scalarStage,"\:72b6\:6001"->"\:5df2\:5b8c\:6210\:ff0c\:8df3\:8fc7"|>]];
  If[Lookup[states,splitStage,""]=!="\:5b8c\:6210" || !KeyExistsQ[row,splitStage],
    Return[Failure["RunPreviousStage",<|"\:7f3a\:5c11\:9636\:6bb5"->{splitStage}|>]]];
  oldCount=If[KeyExistsQ[row,integralStage],RGZMXFastPVCount[row[integralStage]],
    Missing["IntegralNotRun"]];
  RGZMXProgressUpdate["\:90e8\:95e8"->row["\:7c7b\:522b"],"\:56fe\:53f7"->n,
    "\:56fe\:6570"->Length[rgzMXJob["\:9010\:56fe"]],"\:9636\:6bb5"->"\:91cd\:6574"<>integralStage,
    "\:5b50\:6b65\:9aa4"->"\:6574\:7406\:65e7\:79ef\:5206\:6838","\:9879\:53f7"->0,"\:603b\:9879\:6570"->0,"\:5b8c\:6210\:9879\:6570"->0,
    "\:72b6\:6001"->"\:8ba1\:7b97\:4e2d","\:9636\:6bb5\:5f00\:59cb\:65f6\:95f4"->AbsoluteTime[]];
  {time,z}=AbsoluteTiming[CheckAbort[TimeConstrained[
    RGZMXIntegrate[row[splitStage],
      FileNameJoin[{rgzMXJob["\:7f13\:5b58\:76ee\:5f55"],"tid_units_v1"}],seconds],
    seconds,$Aborted],$Aborted]];
  If[z===$Aborted || z===$Failed || FailureQ[z],
    RGZMXProgressUpdate["\:72b6\:6001"->"\:91cd\:6574\:672a\:5b8c\:6210\:ff1b\:539f\:56fe\:8bb0\:5f55\:4fdd\:7559"];
    Return[Failure["IntegralRefreshStopped",<|"\:7f16\:53f7"->n,
      "\:9636\:6bb5"->integralStage,"\:8017\:65f6"->time,"\:539f\:56e0"->z|>]]];
  newCount=RGZMXFastPVCount[z];
  states=Join[states,<|integralStage->"\:5b8c\:6210",scalarStage->"\:672a\:8fd0\:884c"|>];
  result=<|"\:7f16\:53f7"->n,"\:9636\:6bb5"->integralStage,"\:72b6\:6001"->"\:91cd\:6574\:5b8c\:6210",
    "\:8017\:65f6"->time,"\:91cd\:6574\:524d\:5f85\:7ea6\:5316PV\:6570"->oldCount,"\:91cd\:6574\:540e\:5f85\:7ea6\:5316PV\:6570"->newCount|>;
  newRow=Join[KeyDrop[row,{scalarStage}],<|integralStage->z,
    "\:72b6\:6001"->states,"\:6807\:91cf\:7f13\:5b58\:91cd\:6574"->Join[Lookup[row,"\:6807\:91cf\:7f13\:5b58\:91cd\:6574",<||>],
      <|component->result|>]|>];
  path=FileNameJoin[{rgzMXJob["\:7f13\:5b58\:76ee\:5f55"],"graph_"<>ToString[n]<>".wl"}];
  r=RGZMXWrite[newRow,path];If[FailureQ[r],Return[r]];
  all=rgzMXJob["\:9010\:56fe"];all[[n]]=newRow;
  AssociateTo[rgzMXJob,"\:9010\:56fe"->all];
  RGZMXProgressUpdate["\:72b6\:6001"->"\:5b8c\:6210"];
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
  If[TrueQ[rgzMXAcceleration["\:542f\:7528"]] &&
      MemberQ[{"\:6807\:91cfT","\:6807\:91cfL"},stage] && AssociationQ[rgzMXJob] &&
      1<=n<=Length[rgzMXJob["\:9010\:56fe"]],
    row=RGZMXMigrateRow[rgzMXJob["\:9010\:56fe"][[n]]];
    component=StringTake[stage,-1];
    integralStage="\:79ef\:5206"<>component;splitStage="\:5206\:89e3"<>component;
    refreshRecord=Lookup[Lookup[row,"\:6807\:91cf\:7f13\:5b58\:91cd\:6574",<||>],component,<||>];
    If[Lookup[row["\:72b6\:6001"],stage,""]=!="\:5b8c\:6210" &&
      Lookup[row["\:72b6\:6001"],splitStage,""]==="\:5b8c\:6210" &&
      Lookup[row["\:72b6\:6001"],integralStage,""]==="\:5b8c\:6210" &&
      KeyExistsQ[row,integralStage] &&
      Lookup[refreshRecord,"\:72b6\:6001",""]=!="\:91cd\:6574\:5b8c\:6210" &&
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

RGZMXReport["\:6458\:8981",
  "\:6df7\:5408\:77e9\:9635\:6807\:91cf\:52a0\:901f\:5df2\:52a0\:8f7d\:ff1b\:539f\:7f13\:5b58\:4fdd\:7559\:ff0c\:672a\:5b8c\:6210\:7684\:6807\:91cf\:9636\:6bb5\:81ea\:52a8\:91cd\:6574\:65e7\:79ef\:5206\:3002\:8bbe\:7f6e\:ff1aRGZMixSetAcceleration[\"\:5355\:6838\:65f6\:9650\"->120,\"\:9884\:5904\:7406\:65f6\:9650\"->15]\:3002"];


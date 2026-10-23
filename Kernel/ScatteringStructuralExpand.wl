(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Distribute structural sums without expanding independent scalar algebra.
   Slots have no definitions: Expand cannot see their coefficients. Restore
   the coefficients only after bilinearity and the term split are checked. *)
qqFFSelectiveExpand[input_,pattern_,chains_:False] := Module[
 {tag=Unique["RGZWorkflow`Private`scalarTag"],mask,values=<||>,index=0,masked,expanded,rules},
 mask[x_] := If[FreeQ[x,pattern],
  If[NumberQ[x],x,index++;AssociateTo[values,index->x];qqFFScalarSlot[tag,index]],
  If[MemberQ[{Plus,Times,Dot,Power},Head[x]],Map[mask,x],x]];
 masked=mask[input];
 qqProgressUpdate[<|"ScalarBlocks"->index|>];
 expanded=If[TrueQ[chains],qqLinearizeChains[masked],Expand[masked]];
 rules=Dispatch[KeyValueMap[qqFFScalarSlot[tag,#1]->#2&,values]];
 <|"Masked"->expanded,"Rules"->rules|>];

qqFFStructuralParts[input_] := Module[{prepared,expanded,terms,parts,groups,spins,spinRules},
 qqProgressUpdate[<|"Stage"->"QQFormFactors:Structure","Index"->1,"Total"->4,
  "Master"->Missing["NotAvailable"],"OutputLeaves"->Missing["NotAvailable"],
  "StepLimitSeconds"->"run total","AlgebraGroupIndex"->0,"AlgebraGroupTotal"->0|>];
 qqProgressOperation["Protect scalar coefficients during structural expansion"];
 qqProgressUpdate[<|"InputLeaves"->LeafCount[input]|>];
 prepared=qqFFSelectiveExpand[input,qqFFColorHeads|qqFFSpinHeads|Dot,True];
 expanded=prepared["Masked"];
 terms=If[Head[expanded]===Plus,List@@expanded,{expanded}];
 qqProgressOperation["Split protected color-spin terms"];
 qqProgressUpdate[<|"StructuralTerms"->Length[terms]|>];
 parts=qqFFSplit /@ terms;
 If[AnyTrue[parts,FailureQ],Return[First[Select[parts,FailureQ]]]];
 qqProgressOperation["Verify protected term reconstruction"];
 If[Expand[Total[Times@@@parts]-expanded]=!=0,
  Return[Failure["QQFFTermReconstruction",<||>]]];
 qqProgressOperation["Group equal protected color-spin structures"];
 groups=GatherBy[parts,Take[#,2]&];
 parts=({#[[1,1]],#[[1,2]],Total[#[[All,3]]]}& /@ groups);
 (* Scalar slots contain no Lorentz indices. Canonicalize spin dummy names
    before restoring large coefficients or running Clifford simplification. *)
 spins=DeleteDuplicates[parts[[All,2]]];
 spinRules=Dispatch[Table[
  qqProgressOperation["Canonicalize spin dummy indices "<>ToString[i]<>" / "<>ToString[Length[spins]]];
  qqProgressUpdate[<|"AlgebraGroupIndex"->i,"AlgebraGroupTotal"->Length[spins]|>];
  spins[[i]]->qqFFCanon[spins[[i]]],{i,Length[spins]}]];
 parts=({#[[1]],#[[2]]/.spinRules,#[[3]]}& /@ parts);
 groups=GatherBy[parts,Take[#,2]&];
 parts=({#[[1,1]],#[[1,2]],Total[#[[All,3]]]}& /@ groups);
 qqProgressUpdate[<|"StructuralGroups"->Length[parts]|>];
 qqProgressOperation["Restore unexpanded scalar coefficients"];
 parts=parts/.prepared["Rules"];
 qqProgressUpdate[<|"OutputLeaves"->LeafCount[parts]|>];parts];

qqFFMasterTerms[input_] := Module[{prepared,x},
 prepared=qqFFSelectiveExpand[input,qqFFMasterHeads];
 x=prepared["Masked"];
 If[Head[x]===Plus,List@@x,{x}]/.prepared["Rules"]];

(* Old serialized Module-local slots are never legitimate physical symbols. *)
qqFFUnresolvedScalarQ[x_] := !FreeQ[x,_qqFFScalarSlot] ||
 !FreeQ[x,(h_Symbol)[i_Integer]/;Context[Unevaluated[h]]==="RGZWorkflow`Private`"&&StringMatchQ[SymbolName[Unevaluated[h]],"slot$"~~DigitCharacter..]];

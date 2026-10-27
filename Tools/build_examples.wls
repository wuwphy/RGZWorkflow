(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Rebuild only this distribution's clean, input-only examples. *)
root=DirectoryName[DirectoryName[$InputFileName]];
text[s_]:=Cell[s,"Text"];
heading[s_]:=Cell[s,"Section"];
input[s_]:=Cell[BoxData[ToBoxes[ToExpression["("<>s<>")",InputForm,Defer],StandardForm]],"Input"];
load={input["project=ParentDirectory[NotebookDirectory[]];\nGet[FileNameJoin[{project,\"RGZWorkflow.wl\"}]];"],
 input["RGZReport[RGZInitialize[FileNameJoin[{project,\"Work\"}],\"Display\"->\"Compact\"]];"]};
main=Join[{Cell["RGZWorkflow 1.0.0 — Start Here","Title"],
 text["Version 1.0.0. The default model includes eta ghosts and two dynamical quark flavours, with distinct-flavour massless external quarks. This notebook includes no computed results. Earlier probe-model checks do not validate the current model. Use a fresh Work directory."],
 heading["1 - FeynRules: export the actual model"],
 text["Run in a fresh kernel. Install FeynRules externally and set RGZ_FEYNRULES_DIRECTORY if needed. Keep FeynCalc in a separate kernel."]},load,
 {input["Global`FR$Parallel=False;\nRGZReport[RGZLoadFeynRules[]];"],
 input["RGZReport[RGZTreeBenchmark[60]];"],
 input["RGZReport[RGZExtractTree[300]];"],
 input["RGZReport[RGZExportVertices[300]];"],
 input["RGZReport[RGZExtraTreeAudit[120]];"],
 input["RGZReport[RGZOneLoopCoverage[300]];"],
 heading["2 - Restart the kernel: FeynCalc and tree checks"],
 text["Quit the kernel using the menu, then reload below. The 69-entry report covers the original mixed matrix and FP connections; it does not independently validate every free propagator. All vertices includes eta and quark interactions."]},load,
 {input["RGZReport[RGZLoadFeynCalc[]];"],
 input["RGZReport[RGZCheckPropagators[300]];"],
 input["RGZReport[RGZCheckExtraPropagators[]];\nRGZReport[RGZCheckOneLoopCoverage[]];"],
 input["RGZReport[RGZCheckVertices[60,\"IDs\"->All]];"],
 input["RGZReport[RGZCheckOrders[60,\"IDs\"->All,\"Mode\"->\"Compare\"]];"],
 heading["3 - Complete mixed matrix"],
 text["This step may take a long time. Model enumeration includes bosons, all ghost families and two closed quark flavours. Do not add another eta loop. After interruption, reuse only completed checkpoints with matching fingerprints."],
 input["RGZReport[RGZMonitor[RGZMixedComputeAll[12000]]];"],
 input["RGZReport[RGZMixedResult[1200]];\nRGZReport[RGZSave[]];"],
 heading["4 - Complete bare amplitude"],
 text["Continue only after all stages succeed. A bare amplitude alone is not a finite, verified S matrix."]},
 {input["RGZReport[RGZQQStart[600,\"Gauge\"->0,\"RegulatorMass\"->0]];"],
 input["RGZReport[RGZQQPrepareSelf[1800,\"TotalSeconds\"->3600]];"]},
 (input["RGZReport[RGZQQRunPart[\""<>#<>"\",1800]];"]& /@ {"AV1","AV2","Box1","Box2","NAV1","NAV2"}),
 {input["qqSummary=RGZQQAmplitude[];\nRGZReport[qqSummary];\nIf[!FailureQ[qqSummary],qqResult=RGZResult[\"QQAmplitude\"]];"],
 heading["5 - Tree and pure one-loop form factors"],
 text["Organize the D-dimensional color-spin structures first. Keep evanescent terms; do not remove them with four-dimensional Fierz identities. Successful reconstruction does not prove basis independence or minimality."],
 input["RGZReport[RGZQQFormFactors[1200,\"LoopOrder\"->0,\"CoefficientSeconds\"->30]];\nqqTreeFF=RGZResult[\"QQFormFactors\"];"],
 input["RGZReport[RGZQQFormFactors[1200,\"LoopOrder\"->1,\"CoefficientSeconds\"->30]];\nqqOneLoopFF=RGZResult[\"QQFormFactors\"];"],
 input["If[AssociationQ[qqOneLoopFF],KeyTake[qqOneLoopFF,{\"LoopOrder\",\"ReconstructionVerified\",\"SpinBasisIndependent\",\"File\"}]]"],
 heading["6 - General gauge difference for the current model"],
 text["Recompute this check without assuming a zero result. Compare symbolic alpha with reference gauge 1. See the documentation for parameter domains and IR prescriptions. Nonzero redundant coefficients, unreduced integrals and timeouts need further diagnosis."],
 input["RGZReport[RGZQQGaugeStart[600]];"],
 input["RGZReport[RGZQQGaugePrepareSelf[1800,\"TotalSeconds\"->3600]];"],
 input["RGZReport[RGZQQGaugeEarlyPrepare[600,\"ReferenceGauge\"->1]];"],
 input["RGZReport[RGZQQGaugeEarlyCheck[12000,\"ReferenceGauge\"->1,\"TensorSeconds\"->600,\"CoefficientSeconds\"->120,\"ReduceTensorIntegrals\"->True]];"],
 input["gaugePacket=RGZResult[\"QQGaugeEarly\"];\ngaugeFile=If[AssociationQ[gaugePacket],Lookup[gaugePacket,\"File\",Missing[\"NotComputed\"]],Missing[\"NotComputed\"]];"],
 heading["7 - Restore the fixed-gauge job and save"],
 input["RGZReport[RGZQQGaugeRestore[]];\nRGZReport[RGZSave[]];"],
 text["The restore invalidates derived diagnostics. Keep gaugeFile and pass it explicitly to certification. Continue with section 3 of Diagnostics in this kernel, or save and restore the compatible fixed-gauge job. Rebuild UV/IR and MSbar after restoring. RGZSave saves a workflow packet rather than the entire kernel."],
 heading["8 - Author electronic reference"],
 text["This independently readable table is reference data. It does not certify this Work directory. See Paper/provenance.json and Documentation/Reproducibility.md."],
 input["paperReference=RGZPaperResult[];\nIf[AssociationQ[paperReference],KeyTake[paperReference,{\"Schema\",\"CheckedCoefficients\",\"ReferenceOnly\",\"CurrentTaskCertified\"}],paperReference]"]}];
diag=(If[#[[1]]==="Input",input[#[[2]]],Cell[#[[2]],#[[1]]]]& /@ Import[FileNameJoin[{root,"Tools","diagnostics_cells.json"}],"RawJSON"]);
Do[Put[Notebook[spec[[2]],StyleDefinitions->"Default.nb",WindowTitle->spec[[1]]],FileNameJoin[{root,"Examples",spec[[1]]<>".nb"}]],
 {spec,{{"RGZWorkflow — Start Here",main},{"RGZWorkflow — Diagnostics",diag}}}];
Scan[(Export[#,StringReplace[Import[#,"Text"],RegularExpression["(?m)[ \t]+$"]->""],"Text"])&,
 FileNames["*.nb",FileNameJoin[{root,"Examples"}]]];
Print["Clean input-only notebooks written."];

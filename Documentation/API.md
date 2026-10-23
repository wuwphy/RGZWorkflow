# Public API

Generated from this snapshot's public usage strings. A listed interface may require its explicit prerequisites; listing is not a verification claim.

| Symbol | Usage / scope |
|---|---|
| `RGZInitialize` | RGZInitialize[workDirectory] creates isolated Data, Cache and Results directories and selects the bundled model. Option QuarkMasses -> {0,0} specifies the two base-model quark masses; full QQ external states currently remain massless. |
| `RGZConfiguration` | RGZConfiguration[] returns the current small configuration association. |
| `RGZSymbols` | RGZSymbols[] returns the canonical private physical parameters and momenta for substitutions. |
| `RGZSymbolAudit` | RGZSymbolAudit[] checks canonical model symbol identity, definitions and name resolution in the FR master and workers. |
| `RGZDependencies` | RGZDependencies[] reports installed computational dependencies without loading them. |
| `RGZLoadFeynRules` | RGZLoadFeynRules["Directory"->path] loads FeynRules and the selected FR model in a fresh kernel. |
| `RGZLoadFeynCalc` | RGZLoadFeynCalc[] loads FeynCalc with FeynArts in a fresh kernel. |
| `RGZTreeBenchmark` | RGZTreeBenchmark[seconds] independently checks the explicit 5x5 and 8x8 Minkowski tree kernels. |
| `RGZExtractTree` | RGZExtractTree[seconds] extracts and saves the actual FR quadratic kernels. |
| `RGZExportVertices` | RGZExportVertices[seconds] exports the current FR two-, three- and four-leg vertices. |
| `RGZCheckIdentities` | RGZCheckIdentities[seconds] checks BRST algebra and quark Ward identities and audits available propagator/NAV evidence; unimplemented loop ST/Nielsen identities remain explicit. |
| `RGZIdentityRequirements` | RGZIdentityRequirements[] lists the ST, Ward, constraint and Nielsen identities required for a complete verification. |
| `RGZCheckPropagators` | RGZCheckPropagators[seconds] compares the model propagators with the freshly extracted tree kernels. |
| `RGZCheckVertices` | RGZCheckVertices[seconds,"IDs"->All] checks selected three- and four-leg vertices against FR. |
| `RGZCheckOrders` | RGZCheckOrders[seconds,"Mode"->"Compare"] checks labeled permutations; Generate checks distinct field orders only. |
| `RGZMixedStart` | RGZMixedStart[] prepares the one-loop mixed-matrix manifest without generating every graph. |
| `RGZMixedStartPair` | RGZMixedStartPair[a,b,seconds,"Sectors"->All] prepares one ordered pair. |
| `RGZRunGraph` | RGZRunGraph[n,stage,seconds] runs one graph stage in the current mixed pair. |
| `RGZMixedComputePair` | RGZMixedComputePair[a,b,seconds] computes one ordered pair with resumable caches. |
| `RGZMixedComputeAll` | RGZMixedComputeAll[seconds] computes the 64 ordered pairs; never called automatically. |
| `RGZMixedResult` | RGZMixedResult[seconds] collects and validates the projected mixed matrices. |
| `RGZQCDBenchmark` | RGZQCDBenchmark[] checks the four candidate contributions and actual pure-gluon QCD self-energy. |
| `RGZPinchKernels` | RGZPinchKernels["Mode"->"FourPinch"\|"GaugeDifference","ReferenceGauge"->0,"Representation"->"Rational"\|"FAD"] returns the four reviewed kernels, GammaP and their total. |
| `RGZNAVSum` | RGZNAVSum[seconds,"QuarkMass"->0] sums all field-matrix NAV connections and saves a raw connected vertex tensor; no pinch decomposition or integration. |
| `RGZPTStart` | RGZPTStart["Mode"->"FourPinch","ReferenceGauge"->0] prepares pinch kernels; NAVMode defaults to FullMatrix and replaces AA NAV with all matrix connections. |
| `RGZPinchResult` | RGZPinchResult[] returns completed four-part scalar kernels and the separately assembled mixed self-energy contribution. |
| `RGZRunPart` | RGZRunPart[name,stage,seconds] runs one box/nav/av/qse stage. |
| `RGZGaugeCheck` | RGZGaugeCheck[seconds,"Coefficients"->{1}] compares the current candidate combination with a reference gauge, stopping on timeout. |
| `RGZExportResidual` | RGZExportResidual[] writes a complete 39-row residual packet when available. |
| `RGZQQStart` | RGZQQStart[seconds] builds complete conventional massless distinct-flavour quark amplitudes with two dynamical quark flavors, defaulting to alpha=0 and M=0. |
| `RGZQQPrepareSelf` | RGZQQPrepareSelf[seconds,"TotalSeconds"->3600] prepares the mixed self-energy limits with per-coefficient checkpoints; progress is RGZResult["QQProgress"]. |
| `RGZQQAmplitude` | RGZQQAmplitude[] assembles all completed conventional graphs and four on-shell external residues; reports bare, unverified status explicitly. |
| `RGZQQSMatrix` | RGZQQSMatrix[] requires independent full-amplitude gauge and renormalization verification; fails while either proof is missing. |
| `RGZLoadFeynHelpers` | RGZLoadFeynHelpers[] loads the installed Package-X interface in a FeynCalc session; no calculation is started. |
| `RGZQQUVIRIntegral` | RGZQQUVIRIntegral[expr, loop, seconds] separates UV/IR and finite parts of a supplied unreduced real-mass integral, retaining logarithmic scaleless integrals. ImplicitPrefactor defaults to 1/(2 Pi)^(4-2 Epsilon). This does not certify the full amplitude. |
| `RGZQQScalelessLedger` | RGZQQScalelessLedger[seconds] restores UV/IR transfers of explicit scaleless tensor cores in all six Landau M=0 probe graphs without rewriting graph caches. |
| `RGZQQMSbarCounterterms` | RGZQQMSbarCounterterms[] gives independently specified Landau MSbar parameter shifts (default: two dynamical massless quarks) and their renormalization-factor relations. |
| `RGZQQMSbar` | RGZQQMSbar[seconds] checks MSbar pole closure with the raw-core ledger and two-flavor local counterterms; eta is already in the base model, never inserted again; optionally builds a finite PV representation. Does not promote the full S matrix. |
| `RGZPVFinite` | RGZPVFinite[master, scaleSquared] denotes the finite common-bar-epsilon Laurent coefficient of the specified PV integral, with UV and IR poles subtracted at the same scale. No numerical or arbitrary-branch evaluation is implied. |
| `RGZQQPVUVIRLedger` | RGZQQPVUVIRLedger[seconds] audits saved pre/post PV identities for lost scaleless UV/IR pole shifts. Rebuild pure-loop form factors first. Earlier TID and mass limits remain outside its scope. |
| `RGZQQUVIR` | RGZQQUVIR[seconds] computes UV and supported IR poles of LoopOrder -> 1 form factors, with per-master checkpoints. Lost scaleless terms and unsupported complex-mass integrals are reported, not guessed. |
| `RGZQQRenormalize` | RGZQQRenormalize[seconds] checks caller-supplied local ParameterCounterterms against represented UV poles. Explicit Mode -> PoleSubtraction performs a formal pole subtraction only; neither certifies full MSbar renormalization. |
| `RGZQQFormFactors` | RGZQQFormFactors[seconds] organizes the bare amplitude into two SU(N) color flows and D-dimensional spin structures, with scalar master coefficients. LoopOrder -> All (default), 0 or 1 selects full, tree or pure one-loop M. File may select a saved bare-amplitude.wxf. |
| `RGZQQRepairProbeConvention` | RGZQQRepairProbeConvention[] migrates a known old QQ job to the model-consistent +I g quark vertex; preserves complete self energy and even-vertex graphs, reverses only NAV graphs, and invalidates assembled comparisons. Option File selects an old job.wxf. |
| `RGZQQRepairIncomplete` | RGZQQRepairIncomplete[seconds] quarantines failed QQ checkpoints and recomputes affected parts; a valid finite M0 checkpoint avoids reloading the large mixed matrix. |
| `RGZQQRunPart` | RGZQQRunPart[name,seconds] reduces a full vertex, box or quark self-energy integrand to PV functions. |
| `RGZMassLimit` | RGZMassLimit["M0"\|"QCD",seconds] checks the existing specialized 39-row mass-limit diagnostics; M0 accepts "ParameterPoint" -> an exact parameter association. |
| `RGZThermalReduce` | RGZThermalReduce[expr,q,p,mu,nu,seconds] explicitly runs the optional route-restricted V6 thermal reducer. |
| `RGZThermalEvaluate` | RGZThermalEvaluate[coefficients,massSquaredAssociation,seconds,options] runs the optional T=0-subtracted numerical backend. |
| `RGZVacuumSubtract` | RGZVacuumSubtract[expr,seconds] runs the original explicit MSbar pole subtraction; FeynHelpers must already be loaded. |
| `RGZSplit` | RGZSplit[expr,q,p,seconds] explicitly factorizes loop propagators using the checked loop backend. |
| `RGZPoleMasses` | RGZPoleMasses[expr] replaces the rgzLoopM1..4 labels with the physical pole masses. |
| `RGZSave` | RGZSave[] saves an explicit versioned WXF session packet, without notebook output. |
| `RGZRestore` | RGZRestore[] restores a versioned packet and rejects a different model fingerprint. |
| `RGZResult` | RGZResult[name] returns stored data; use a semicolon for large matrices. |
| `RGZStatus` | RGZStatus[] returns a small stage-status association; RGZStatus["Mixed"\|"PT"] shows the relevant engine table. |
| `RGZReport` | RGZReport[result] prints a bounded status/diagnostic report, never a huge expression. |
| `RGZShow` | RGZShow[expression,maxLeaves] displays only a bounded expression. |
| `RGZMonitor` | RGZMonitor[command] displays one compact engine progress panel when available. |
| `RGZRegisterStage` | RGZRegisterStage[name,function] registers an explicit developer extension. |
| `RGZRunExtension` | RGZRunExtension[name,arguments...] explicitly runs a registered stage. |
| `$RGZWorkflowVersion` | The RGZWorkflow API/data-schema version. |
| `RGZQQGaugeStart` | Start a symbolic-alpha M=0 QQ task with an explicit common dimensional scaleless-zero prescription; preserves the previous task. |
| `RGZQQGaugePrepareSelf` | Prepare the symbolic-gauge mixed self energy. |
| `RGZQQGaugeRunPart` | Reduce one full diagram in the symbolic-gauge task. |
| `RGZQQGaugeAssemble` | Assemble the symbolic-gauge one-loop packet; no extra pinch term is added. |
| `RGZQQGaugeEarlyPrepare` | Prepare protected raw integrand gauge differences before pole splitting and TID; experimental, not a gauge proof. |
| `RGZQQGaugeEarlyCheck` | Reduce the combined prepared six-diagram gauge difference and add mixed self-energy and external-residue differences; experimental independent check. |
| `RGZQQGaugeCheck` | Check every full one-loop color-spin-PV gauge difference at symbolic alpha. |
| `RGZQQWardCoverage` | Compare the full gauge difference with the complete existing pinch combination, then test its gauge difference; not a functional ST proof. |
| `RGZQQGaugeRestore` | Restore the QQ task saved before the symbolic-gauge calculation. |

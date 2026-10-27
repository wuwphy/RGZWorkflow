# Quark renormalization and certification

Use Diagnostics after the fixed-gauge bare amplitude is complete. Preserve `gaugeFile`, the general-gauge `comparison.wxf`, before `RGZQQGaugeRestore[]` invalidates derived in-memory diagnostics.

The required sequence is:

1. `RGZQQFormFactors[..., "LoopOrder" -> 1]`, `RGZQQUVIR`, `RGZQQScalelessLedger`, `RGZQQPVUVIRLedger`.
2. `RGZQQMSbarCounterterms[]`, then `RGZQQMSbar`.
3. `RGZQQMassLimitLedger`, `RGZQQMassRegionMatch`, `RGZQQRawMassPoleAudit`, `RGZQQWeightedUVIRLedger`.
4. `RGZQQCompleteMassLedger`, `RGZQQFiniteAudit`, `RGZQQCertification["GaugeFile" -> gaugeFile]`.
5. `RGZQQSMatrix[]`.

The notebook binds the fixed-gauge job file and current matrix manifest. Manifests use `Record`; the audit adapter also accepts a consistent `Records` alias. A pre-TID unit inventory alone is not source graph coverage.

Retain D until all graph weights and Laurent products are assembled. For D=4-2 epsilon the finite contribution includes `-2 c'(4) Pole1 + 2 c''(4) Pole2`. Include the dimensional measure conversion once. The Born term is added once. Regulator tadpoles must be combined with small-mass bubble regions in the recorded limit order; raw pole bounds are checked independently of their saved mass series.

After mass completion has replaced the active ledger, rerun the tadpole-ledger cell before retrying mass regions, as directed by the notebook. Completed checkpoints remain reusable. A timeout does not establish a mismatch.

The final certificate requires matching source jobs, gauge preparation, actual finite payloads, reconstructed rows and linked evidence hashes. Incomplete evidence produces `CertificationIncomplete` and no certified amplitude. The final result retains its IR conditions and branch prescription.

Scalar conversion may shift separate UV and IR representations in opposite directions. `SeparateUVIRUnchanged -> False` is compatible with a passed common-pole and finite-equivalence audit. Local reports deliberately do not certify the complete amplitude; only the final linked certificate does.

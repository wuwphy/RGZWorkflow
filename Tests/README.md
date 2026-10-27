# Essential checks

The retained checks cover package loading/session guards, the scalar audit adapter, notebook dependency handling, exact generic-D scalar identities, interrupted weighted assembly, certificate payload validation, paper-table integrity and Python filesystem safety.

```sh
python3 Tools/run_tests.py --kernel /path/to/WolframKernel --suite core
python3 Tools/run_tests.py --kernel /path/to/WolframKernel --suite algebra
python3 Tests/ReleaseTools.py
```

Core checks require no external physics packages. Algebra checks require FeynCalc/FeynArts. The complete native model/graph calculation is exercised by Start Here and Diagnostics. Historical development tests are preserved in the local cleanup backup.

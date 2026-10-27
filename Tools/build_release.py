#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Create a deterministic source ZIP; never copy Work, dependencies or run logs."""
import argparse,hashlib,pathlib,subprocess,sys,zipfile
ROOT=pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'Tools'))
from check_release import files

def main():
 p=argparse.ArgumentParser(description=__doc__);p.add_argument('--output',type=pathlib.Path,default=ROOT/'artifacts');a=p.parse_args()
 result=subprocess.run([sys.executable,str(ROOT/'Tools/check_release.py')],cwd=ROOT)
 if result.returncode:return result.returncode
 out=a.output.resolve();out.mkdir(parents=True,exist_ok=True)
 if ROOT in out.parents and out!=ROOT/'artifacts':p.error('Output inside source must be the excluded artifacts directory')
 archive=out/'RGZWorkflow-1.0.0.zip'
 with zipfile.ZipFile(archive,'w',compression=zipfile.ZIP_DEFLATED,compresslevel=9) as z:
  for f in files():
   info=zipfile.ZipInfo('RGZWorkflow-1.0.0/'+str(f.relative_to(ROOT)),date_time=(2026,10,28,0,0,0))
   info.compress_type=zipfile.ZIP_DEFLATED;info.external_attr=(0o100644<<16)
   z.writestr(info,f.read_bytes(),compress_type=zipfile.ZIP_DEFLATED,compresslevel=9)
 digest=hashlib.sha256(archive.read_bytes()).hexdigest();(out/'SHA256SUMS').write_text(f'{digest}  {archive.name}\n')
 print(f'{archive.name}: {archive.stat().st_size} bytes; SHA256 {digest}');return 0
if __name__=='__main__':sys.exit(main())

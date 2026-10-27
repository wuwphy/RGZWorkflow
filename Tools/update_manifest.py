#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Explicitly refresh the source manifest after intentional source/evidence edits."""
import argparse, hashlib, json
from check_release import ROOT, files
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--write',action='store_true',help='Required acknowledgement to replace the manifest')
a=p.parse_args()
if not a.write:p.error('Use --write after reviewing intentional changes')
if any(f.is_symlink() for f in files()):p.error('Symlinks are not permitted in the source distribution')
data={'schema':'RGZSourceManifest/1','release':'1.0.0','algorithm':'SHA256','files':{str(f.relative_to(ROOT)):hashlib.sha256(f.read_bytes()).hexdigest() for f in files() if f.name!='RELEASE_MANIFEST.json'}}
(ROOT/'RELEASE_MANIFEST.json').write_text(json.dumps(data,indent=2)+'\n')
print('Manifest contains',len(data['files']),'files.')

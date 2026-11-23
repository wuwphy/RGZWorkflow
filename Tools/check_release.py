#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Read-only source-distribution checks, requiring only Python's standard library."""
from pathlib import Path
import hashlib,json,re,sys,os
from urllib.parse import unquote
ROOT=Path(__file__).resolve().parents[1]
EXCLUDE={'Work','.git','__pycache__','artifacts','output'}
def included(p):
 rel=p.relative_to(ROOT)
 return not any(x in EXCLUDE for x in rel.parts) and rel.parts[:2]!=('Evidence','runs')
def files():
 found=[]
 for parent,dirs,names in os.walk(ROOT,followlinks=False):
  parent=Path(parent)
  dirs[:]=[d for d in dirs if d not in EXCLUDE and not (parent==ROOT/'Evidence' and d=='runs')]
  found.extend(parent/d for d in dirs if (parent/d).is_symlink())
  dirs[:]=[d for d in dirs if not (parent/d).is_symlink()]
  found.extend(parent/name for name in names if name!='.DS_Store')
 return sorted(found)
def main():
 errors=[]
 for name in ['Kernel/RGZWorkflow.wl','Kernel/init.m','RGZWorkflow.wl','PacletInfo.wl','COPYING','LICENSE','CITATION.cff','RELEASE_MANIFEST.json','Evidence/verified-run.json','README.md']:
  if not (ROOT/name).is_file():errors.append('Missing '+name)
 for ext in ['fr','mod','gen']:
  if not (ROOT/f'Models/RGZ_W2.6.{ext}').is_file():errors.append('Missing model companion '+ext)
 if errors:
  print(json.dumps({'errors':errors},indent=2));return True
 if (ROOT/'FeynRules').exists():errors.append('Third-party FeynRules must not be bundled')
 if (ROOT/'COPYING').is_file() and 'Version 3, 29 June 2007' not in (ROOT/'COPYING').read_text():errors.append('Incomplete GPL text')
 # Current release metadata must agree; historical evidence is deliberately versioned separately.
 version_match=re.search(r'"Version"->"([^"]+)"',(ROOT/'PacletInfo.wl').read_text())
 if version_match is None:
  print(json.dumps({'errors':['Invalid paclet version metadata']}));return True
 version=version_match.group(1)
 if version != '1.1.1':errors.append('Unexpected package version '+version)
 for name in ['Kernel/RGZWorkflow.wl','CITATION.cff','README.md','Tools/build_release.py','Tools/update_manifest.py','Tools/run_tests.py']:
  if version not in (ROOT/name).read_text():errors.append('Version mismatch '+name)
 source=(ROOT/'Kernel/RGZWorkflow.wl').read_text()
 m=re.search(r'Scan\[Get,.*?/@\s*\{(.*?)\}\];',source,re.S)
 modules=re.findall(r'"([^"]+\.wl)"',m.group(1)) if m else []
 if not modules or len(modules)!=len(set(modules)):errors.append('Invalid ordered loader')
 # The ordinary loader and on-demand physics files share physical directories.
 kernel=ROOT/'Kernel'
 sources={str(p.relative_to(kernel)):p for p in kernel.rglob('*.wl')}
 seen=set()
 def visit(name):
  if name in seen:return
  seen.add(name)
  if name not in sources:
   errors.append('Missing module '+name);return
  # Source/hash references also identify the FeynCalc/FeynRules files loaded
  # only when their calculation is requested. Generated output names are ignored.
  for child in re.findall(r'"([^"\n]+\.wl)"',sources[name].read_text()):
   if child in sources:visit(child)
 for name in modules:visit(name)
 for name in sources:
  if name!='RGZWorkflow.wl' and name not in seen:
   errors.append('Unindexed calculation file '+name)
 for p in files():
  relpath=p.relative_to(ROOT)
  allowed_roots={'Kernel','Models','Tests','Tools','Examples','Documentation','Paper','Evidence','.github'}
  allowed_files={'README.md','COPYING','LICENSE','CITATION.cff','RELEASE_MANIFEST.json','CONTRIBUTING.md','CHANGELOG.md','THIRD_PARTY.md','PacletInfo.wl','RGZWorkflow.wl','.gitignore'}
  if (len(relpath.parts)==1 and p.name not in allowed_files) or (len(relpath.parts)>1 and relpath.parts[0] not in allowed_roots):errors.append('Unexpected distribution file '+str(relpath))
  if p.is_symlink():
   errors.append('Symlink '+str(p.relative_to(ROOT)));continue
  if p.suffix in {'.wxf','.mx','.pending','.pdf'}:errors.append('Generated/private artifact '+str(p.relative_to(ROOT)))
  if p.suffix in {'.md','.wl','.wls','.wlt','.m','.fr','.mod','.gen','.nb','.json','.cff','.py','.yml','.log'}:
   text=p.read_text();rel=str(p.relative_to(ROOT))
   decoded=re.sub(r'\\:([0-9a-fA-F]{4})',lambda m:chr(int(m.group(1),16)),text)
   if re.search(r'[\u3400-\u4dbf\u4e00-\u9fff]',decoded):errors.append('Non-English CJK text '+rel)
   # Patterns are assembled to avoid flagging this checker's own literals.
   if ('/Us'+'ers/') in text or ('/private/var/'+'folders/') in text:errors.append('Absolute private path '+rel)
   if p.suffix=='.md':
    for target in re.findall(r'\[[^\]]*\]\((<[^>]+>|[^)]+)\)',text):
     target=unquote(target.strip('<>').split('#',1)[0])
     if target and '://' not in target and not (p.parent/target).exists():errors.append(f'Broken link {rel}: {target}')
   if p.suffix in {'.wl','.wls','.wlt','.m','.fr','.mod','.gen'} and 'SPDX-License-Identifier: GPL-3.0-or-later' not in text:errors.append('Missing SPDX '+rel)
 manifest=ROOT/'RELEASE_MANIFEST.json'
 if manifest.is_file():
  data=json.loads(manifest.read_text());expected=data['files'];actual={str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in files() if p.name!='RELEASE_MANIFEST.json' and not p.is_symlink()}
  for key in sorted(set(expected)|set(actual)):
   if expected.get(key)!=actual.get(key):errors.append('Manifest mismatch '+key)
 print(json.dumps({'checked_files':len(files()),'ordered_modules':len(modules),'errors':errors},indent=2))
 return bool(errors)
if __name__=='__main__':sys.exit(main())

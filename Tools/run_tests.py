#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Run curated tests in separate Wolfram processes; never use production Work."""
import argparse, datetime, json, os, pathlib, re, subprocess, sys, tempfile, time
ROOT=pathlib.Path(__file__).resolve().parents[1]
SUITES={
 'core':['RunTests.wls','DiagnosticsCompatibility.wls','DiagnosticsNotebook.wls','PaperResult.wls','EvidenceValidation.wls'],
 'algebra':['WeightedAuditResume.wls','WeightedScalarIdentity.wls'],
}

def work_is_isolated(work):
 work=work.resolve();root=ROOT.resolve();production=(root/'Work').resolve()
 return all(work!=p and p not in work.parents and work not in p.parents for p in (root,production))

def main():
 p=argparse.ArgumentParser(description=__doc__)
 p.add_argument('--kernel',required=True,help='WolframKernel executable (no shell command)')
 p.add_argument('--suite',choices=[*SUITES,'all'],default='core')
 p.add_argument('--work',type=pathlib.Path,help='Isolated integration export; never daily Work')
 p.add_argument('--timeout',type=int,default=1800,help='Per-process wall seconds')
 args=p.parse_args()
 if args.timeout<=0:p.error('--timeout must be positive')
 suites=list(SUITES) if args.suite=='all' else [args.suite]
 work=args.work.resolve() if args.work else pathlib.Path(tempfile.mkdtemp(prefix='rgz-release-integration-'))
 if not work_is_isolated(work):p.error('Work must be isolated from the source tree and production Work, including aliases and ancestors')
 stamp=datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%S')
 out=ROOT/'Evidence/runs'/f'{stamp}-{os.getpid()}';out.mkdir(parents=True)
 env=dict(os.environ,RGZ_TEST_WORK=str(work),RGZ_TEST_CORES='0',RGZ_TEST_ENVIRONMENT_RECORD=str(out/'environment.json'))
 env.setdefault('KMP_USE_SHM','0')
 def sanitize(text):
  for value,alias in [(str(ROOT),'${PROJECT}'),(str(work),'${TEST_WORK}'),(tempfile.gettempdir(),'${TEMP}'),(str(pathlib.Path.home()),'${HOME}')]:text=text.replace(value,alias)
  return re.sub(r'/private/var/' + r'folders/[^\s,\]\}"<>]+','${TEMP}',text)
 report={'release':'1.0.0','suite':args.suite,'date_utc':stamp,'tests':[],'production_data_used':False,'full_amplitude_recomputed':False}
 tests=[]
 for suite in suites:
  tests.extend((suite,'Tests/'+f) for f in SUITES[suite])
 tests.append(('syntax','Tools/check_sources.wls'))
 tests.append(('notebooks','Tools/check_notebooks.wls'))
 for suite,rel in tests:
  start=time.monotonic();print(f'RUN {suite}: {rel}',flush=True)
  try:
   proc=subprocess.run([args.kernel,'-script',str(ROOT/rel)],cwd=ROOT,env=env,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True,timeout=args.timeout)
   code=proc.returncode;text=proc.stdout
  except subprocess.TimeoutExpired as e:
   code=124;text=e.stdout or ''
   if isinstance(text,bytes):text=text.decode(errors='replace')
   text+='\nRelease runner wall timeout: current process did not complete.\n'
  except OSError as e:code=127;text=str(e)
  elapsed=round(time.monotonic()-start,3)
  clean=sanitize(text);log=pathlib.Path(rel).stem+'.log';(out/log).write_text(clean)
  case={'suite':suite,'script':rel,'exit_code':code,'passed':code==0,'seconds':elapsed,'log':log}
  report['tests'].append(case);(out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
  print(f'{"PASS" if code==0 else "FAIL"} {rel} ({elapsed}s)',flush=True)
  if code!=0:
   print(clean[-5000:]);break
 report['passed']=len(report['tests'])==len(tests) and all(c['passed'] for c in report['tests'])
 (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
 print('REPORT '+str((out/'report.json').relative_to(ROOT)),flush=True)
 return 0 if report['passed'] else 1
if __name__=='__main__':sys.exit(main())

# SPDX-License-Identifier: GPL-3.0-or-later
"""Filesystem regression tests for release collection and test-work isolation."""
import contextlib
import io
import pathlib
import sys
import tempfile
import unittest
from unittest.mock import patch
sys.path.insert(0,str(pathlib.Path(__file__).resolve().parents[1]/'Tools'))
import check_release
import run_tests

class ReleaseToolsTests(unittest.TestCase):
 def test_directory_symlinks_are_not_silently_omitted(self):
  with tempfile.TemporaryDirectory() as tmp:
   root=pathlib.Path(tmp);(root/'Kernel').mkdir();(root/'outside').mkdir()
   link=root/'Kernel'/'alias';link.symlink_to(root/'outside',target_is_directory=True)
   with patch.object(check_release,'ROOT',root):
    self.assertIn(link,check_release.files())
 def test_work_paths_reject_source_ancestors_and_aliases(self):
  with tempfile.TemporaryDirectory() as tmp:
   parent=pathlib.Path(tmp);root=parent/'project';root.mkdir();outside=parent/'production';outside.mkdir()
   (root/'Work').symlink_to(outside,target_is_directory=True)
   alias=parent/'alias';alias.symlink_to(outside,target_is_directory=True)
   with patch.object(run_tests,'ROOT',root):
    for path in [root,root/'nested',parent,root/'Work',outside,alias,alias/'nested']:
     self.assertFalse(run_tests.work_is_isolated(path),path)
    self.assertTrue(run_tests.work_is_isolated(parent/'isolated'))
 def test_missing_distribution_files_are_reported(self):
  with tempfile.TemporaryDirectory() as tmp,patch.object(check_release,'ROOT',pathlib.Path(tmp)):
   with contextlib.redirect_stdout(io.StringIO()) as out:
    self.assertTrue(check_release.main())
   self.assertIn('Missing PacletInfo.wl',out.getvalue())
if __name__=='__main__':unittest.main()

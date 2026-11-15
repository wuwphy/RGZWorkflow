# Attribution

This dashboard and the traffic collector are derived from [GitHub Traffic Tracker](https://github.com/djdarcy/github-traffic-tracker), Copyright (C) 2025 Dustin Darcy, GPL-3.0-or-later. Upstream commit: `045bd1dba2e6a84659dfb8273aee4690cfae3ac7`.

The GPL text is available in the repository's COPYING file. RGZWorkflow adaptations configure the project, pin the GitHub action, initialize/reuse Gists on the first authorized run, read the configured public statistics Gist directly from its CDN, and fail explicitly when traffic permissions are missing. No physics source is changed.

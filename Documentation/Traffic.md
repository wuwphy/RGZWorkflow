# Repository traffic

[Public dashboard](https://wuwphy.github.io/RGZWorkflow/stats/). Deployment uses [github-traffic-tracker](https://github.com/djdarcy/github-traffic-tracker), pinned to upstream commit `045bd1dba2e6a84659dfb8273aee4690cfae3ac7`.

## One-time setup

1. Ensure GitHub Actions can run (resolve any account billing lock).
2. Create a dedicated fine-grained PAT for **only RGZWorkflow** with repository permissions **Administration: read**, **Actions: read**, **Contents: read**, and account permission **Gists: read/write**. Save it as the repository Actions secret `TRAFFIC_GIST_TOKEN`. Do not put it in source, chat, or a Gist. An expiring token must be renewed before it expires.
3. Enable GitHub Pages from branch `main`, folder `/docs`.
4. Run **Actions → Repository traffic → Run workflow** once. It creates a public statistics Gist and an unlisted archive Gist, then reuses them on subsequent runs. The workflow summary links to the data. Optional variables `TRAFFIC_GIST_ID` and `TRAFFIC_ARCHIVE_GIST_ID` can select existing storage.

The dashboard discovers its public data by the exact `[GTT] wuwphy/RGZWorkflow - badges` description; no browser token is needed. Do not rename or duplicate the tracker Gists. An unlisted archive is not access-controlled: its link is readable by anyone who knows it.

## Schedule and meaning

Collection runs daily at **03:17 UTC / 11:17 Asia/Shanghai**, or manually. GitHub schedules may be delayed. Gists contain traffic aggregates and badges, not source code or credentials. The dashboard publicly exposes collected repository statistics, including referrers and popular paths.

GitHub supplies only the most recent 14 days of clone/view data. Earlier traffic cannot be reconstructed by installing this tracker. History accumulates from the first successful collection. Summed daily unique counts are not deduplicated people across the lifetime of the project; CI-clone subtraction is the upstream heuristic. Release downloads count GitHub release assets and do not include Zenodo downloads. See [upstream limitations](https://github.com/djdarcy/github-traffic-tracker#current-status).

Until the first collection, the dashboard says it is awaiting data rather than claiming a successful zero-count collection. Missing token or traffic permissions stop the job. The tracker has no effect on Wolfram calculations or existing releases.

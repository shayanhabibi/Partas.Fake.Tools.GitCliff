# Fake.Tools.GitCliff

FAKE compatible [git-cliff](https://git-cliff.org/) runner, and configuration
parser/generator.

```ansi
dotnet add package Partas.Fake.Tools.GitCliff
```

```fsharp
open Fake.Tools

GitCliff.run ...
```

## Motivation

The motivation for this was to automate the release notes generation from commit messages, and to tie that in with Fake.Core.ReleaseNotes to generate the assembly version from the tag et al.

Essentially, my workflow combines:
- Automatically packs and pushes a nuget package with the tag github tag/release version.
- Release note generation and automatic push when publishing a version
- Extensive customisability of release note template.

You don't need git-cliff on your machine, you can run it entirely through github actions.

> [!NOTE]
> Unlike the generic defaults from git-cliff, this package generates a default configuration with FAKE friendly formatting so that the assembly versions can appropriately be parsed.

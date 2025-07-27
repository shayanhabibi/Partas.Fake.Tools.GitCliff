module Fake.Tools.GitCliff

open Fake.Core
open Fake.IO
open Tomlyn

/// <summary>
/// Internal helpers for Tomlyn dotnet library
/// </summary>
module FSharpToml =
    let tomlModelOptions = TomlModelOptions ()

    tomlModelOptions.ConvertToModel <-
        System.Func<obj, System.Type, obj> (fun tkn ty ->
            match tkn with
            | Null -> None
            | :? bool as t ->
                t
                |> Some
                |> box
            | :? string as t ->
                t
                |> Some
                |> box
            | o ->
                o
                |> Some
                |> box)

    tomlModelOptions.ConvertToToml <-
        System.Func<obj, obj> (fun input ->
            match input with
            | _ when
                input
                |> Option.ofObj
                |> Option.isNone
                ->
                null
            | :? Option<obj> as (Some value) -> value
            | :? Option<bool> as (Some value) -> value
            | :? Option<string> as (Some value) -> value
            | _ -> input)

    tomlModelOptions.IgnoreMissingProperties <- true

    let inline toToml value =
        Toml.FromModel (value, tomlModelOptions)

    let inline fromToml< ^T when ^T: (new: unit -> ^T) and ^T: not struct> (input: string) : ^T =
        Toml.ToModel< ^T> (input, options = tomlModelOptions)

/// <summary>
/// Contains operators to assist with the templating delimiters of git-cliff/Tera.
/// </summary>
/// <remarks>
/// There are helpers to bind the <c>{{ }}</c>, <c>{% %}</c> delimiters.
/// <code>
/// sprintf (!% "if commit") = "{% if commit %}"
/// sprintf (!-% "if commit") = "{%- if commit %}"
/// sprintf (!-%- "if commit") = "{%- if commit -%}"
/// sprintf (!%- "if commit") = "{% if commit -%}"
/// sprintf (!?! "commit.author") = "{{ commit.author }}"
/// </code>
/// Just because you can, doesn't mean you should. Unless you're building it entirely
/// programmatically, I don't see the reason to insist on using interpolated strings
/// with these operators.
/// </remarks>
module ConfigOperators =
    let (!%) value =
        $"{{%% {value} %%}}"

    let (!-%) value =
        $"{{%%- {value} %%}}"

    let (!%-) value =
        $"{{%% {value} -%%}}"

    let (!-%-) value =
        $"{{%%- {value} -%%}}"

    let (!?!) value =
        $"{{{{ {value} }}}}"

open ConfigOperators

/// <summary>
/// Contains types, methods and functions to generate, parse, and therefor manipulate
/// <c>git-cliff</c> configuration files.
/// </summary>
/// <seealso href="https://git-cliff.org/docs">
/// See the <c>git-cliff</c> docs.
/// </seealso>
module ConfigHelper =

    type CliffTemplates =
        | Cocogitto
        | Detailed
        | GitHubKeepAChangeLog
        | GitHub
        | KeepAChangeLog
        | Minimal
        | Scoped
        | ScopedSorted
        | Statistics
        | Unconventional

        static member Default = CliffTemplates.GitHubKeepAChangeLog

        override this.ToString() =
            match this with
            | Cocogitto -> "cocogitto"
            | Detailed -> "detailed"
            | GitHubKeepAChangeLog -> "github-keepachangelog"
            | GitHub -> "gitHub"
            | KeepAChangeLog -> "keepachangelog"
            | Minimal -> "minimal"
            | Scoped -> "scoped"
            | ScopedSorted -> "scopedsorted"
            | Statistics -> "statistics"
            | Unconventional -> "unconventional"

    [<CLIMutable; CompiledName "Footers">]
    type CommitFooter =
        { Token: string
          Separator: string
          Value: string
          Breaking: bool option }

        static member init =
            { Token = null
              Separator = null
              Value = null
              Breaking = None }

        static member Create(?token: string, ?separator: string, ?value: string, ?breaking: bool) =
            let defaultString = Option.defaultValue null

            { Token = defaultString token
              Separator = defaultString separator
              Value = defaultString value
              Breaking = breaking }

    [<CLIMutable; CompiledName "Links">]
    type CommitLink =
        { Text: string
          Href: string }

        static member init = { Text = null; Href = null }

        static member Create(?text: string, ?href: string) =
            let defaultString = Option.defaultValue null

            { Text = defaultString text
              Href = defaultString href }

    [<CLIMutable; CompiledName "Author">]
    type CommitAuthor =
        { Name: string
          Email: string
          Timestamp: int64 option }

        static member init =
            { Name = null
              Email = null
              Timestamp = None }

        static member Create(?name: string, ?email: string, ?timestamp: System.DateTime) =
            let defaultString = Option.defaultValue null

            { Name = defaultString name
              Email = defaultString email
              Timestamp =
                timestamp
                |> Option.map _.Ticks }

    [<CLIMutable; CompiledName "Committer">]
    type CommitCommitter =
        { Name: string
          Email: string
          Timestamp: int64 option }

        static member init =
            { Name = null
              Email = null
              Timestamp = None }

        static member Create(?name: string, ?email: string, ?timestamp: System.DateTime) =
            let defaultString = Option.defaultValue null

            { Name = defaultString name
              Email = defaultString email
              Timestamp =
                timestamp
                |> Option.map _.Ticks }

    [<CLIMutable; CompiledName "CommitParsers">]
    type CommitParser =
        { Id: string
          Group: string
          Scope: string
          Message: string
          Body: string
          Footers: CommitFooter[]
          BreakingDescription: string
          Breaking: bool option
          Conventional: bool option
          MergeCommit: bool option
          Links: CommitLink[]
          Author: CommitAuthor option
          Committer: CommitAuthor option
          RawMessage: string
          Skip: bool option }

        static member init =
            { Id = null
              Group = null
              Scope = null
              Message = null
              Body = null
              Footers = null
              BreakingDescription = null
              Breaking = None
              Conventional = None
              MergeCommit = None
              Links = null
              Author = None
              Committer = None
              RawMessage = null
              Skip = None }

        static member Create
            (
                ?id,
                ?group,
                ?scope,
                ?message,
                ?body,
                ?footers,
                ?breakingDescription,
                ?breaking,
                ?conventional,
                ?mergeCommit,
                ?links,
                ?author,
                ?committer,
                ?rawMessage,
                ?skip
            ) =
            { Id =
                id
                |> Option.defaultValue null
              Group =
                group
                |> Option.defaultValue null
              Scope =
                scope
                |> Option.defaultValue null
              Message =
                message
                |> Option.defaultValue null
              Body =
                body
                |> Option.defaultValue null
              Footers =
                footers
                |> Option.defaultValue null
              BreakingDescription =
                breakingDescription
                |> Option.defaultValue null
              Breaking = breaking
              Conventional = conventional
              MergeCommit = mergeCommit
              Links =
                links
                |> Option.defaultValue null
              Author = author
              Committer = committer
              RawMessage =
                rawMessage
                |> Option.defaultValue null
              Skip = skip }

    [<CLIMutable; CompiledName "CommitPreprocessors">]
    type CommitPreprocessor =
        { Pattern: string
          Replace: string option
          ReplaceCommand: string option }

        static member init =
            { Pattern = null
              Replace = None
              ReplaceCommand = None }

        static member Create(?pattern: string, ?replace: string, ?replaceCommand: string) =
            { Pattern =
                pattern
                |> Option.defaultValue null
              Replace = replace
              ReplaceCommand = replaceCommand }

    [<CLIMutable; CompiledName "Postprocessors">]
    type CommitPostprocessor =
        { Pattern: string
          Replace: string option
          ReplaceCommand: string option }

        static member init =
            { Pattern = null
              Replace = None
              ReplaceCommand = None }

        static member Create(?pattern: string, ?replace: string, ?replaceCommand: string) =
            { Pattern =
                pattern
                |> Option.defaultValue null
              Replace = replace
              ReplaceCommand = replaceCommand }

    [<CLIMutable; CompiledName "LinkParsers">]
    type LinkParser =
        { Pattern: string
          Text: string option
          Href: string option }

        static member init =
            { Pattern = null
              Text = None
              Href = None }

        static member Create(?pattern: string, ?text: string, ?href: string) =
            { Pattern =
                pattern
                |> Option.defaultValue null
              Text = text
              Href = href }

    [<CLIMutable; CompiledName("Bump")>]
    type BumpOptions =
        { FeaturesAlwaysBumpMinor: bool option
          BreakingAlwaysBumpMajor: bool option
          InitialTag: string
          CustomMajorIncrementRegex: string
          CustomMinorIncrementRegex: string
          BumpType: string }

        static member init =
            { FeaturesAlwaysBumpMinor = None
              BreakingAlwaysBumpMajor = None
              InitialTag = null
              CustomMajorIncrementRegex = null
              CustomMinorIncrementRegex = null
              BumpType = null }

        static member Create
            (
                ?featuresAlwaysBumpMinor: bool,
                ?breakingAlwaysBumpMajor: bool,
                ?initialTag: string,
                ?customMajorIncrementRegex: string,
                ?customMinorIncrementRegex: string,
                ?bumpType: string
            ) =
            { FeaturesAlwaysBumpMinor = featuresAlwaysBumpMinor
              BreakingAlwaysBumpMajor = breakingAlwaysBumpMajor
              InitialTag =
                initialTag
                |> Option.defaultValue null
              CustomMajorIncrementRegex =
                customMajorIncrementRegex
                |> Option.defaultValue null
              CustomMinorIncrementRegex =
                customMinorIncrementRegex
                |> Option.defaultValue null
              BumpType =
                bumpType
                |> Option.defaultValue null }

    [<CLIMutable; CompiledName("Changelog")>]
    type ChangeLogOptions =
        { Header: string
          Body: string
          Footer: string
          Trim: bool option
          RenderAlways: bool option
          Postprocessors: CommitPostprocessor[]
          Output: string }

        static member init =
            { Header = null
              Body = null
              Footer = null
              Trim = None
              RenderAlways = None
              Postprocessors = null
              Output = null }

        static member Create
            (
                ?header: string,
                ?body: string,
                ?footer: string,
                ?trim: bool,
                ?renderAlways: bool,
                ?postprocessors: CommitPostprocessor[],
                ?output: string
            ) =
            let defaultString = Option.defaultValue null

            { Header = defaultString header
              Body = defaultString body
              Footer = defaultString footer
              Trim = trim
              RenderAlways = renderAlways
              Postprocessors =
                postprocessors
                |> Option.defaultValue null
              Output = defaultString output }

    [<CLIMutable; CompiledName("Git")>]
    type GitOptions =
        { ConventionalCommits: bool option
          FilterUnconventional: bool option
          RequireConventional: bool option
          SplitCommits: bool option
          CommitPreprocessors: CommitPreprocessor[]
          CommitParsers: CommitParser[]
          ProtectBreakingCommits: bool option
          FilterCommits: bool option
          TagPattern: string
          SkipTags: string
          IgnoreTags: string
          CountTags: string
          TopoOrder: bool option
          TopoOrderCommits: bool option
          SortCommits: string
          LinkParsers: LinkParser[]
          LimitCommits: int option
          RecurseSubmodules: bool option }

        static member init =
            { ConventionalCommits = None
              FilterUnconventional = None
              RequireConventional = None
              SplitCommits = None
              CommitPreprocessors = null
              CommitParsers = null
              ProtectBreakingCommits = None
              FilterCommits = None
              TagPattern = null
              SkipTags = null
              IgnoreTags = null
              CountTags = null
              TopoOrder = None
              TopoOrderCommits = None
              SortCommits = null
              LinkParsers = null
              LimitCommits = None
              RecurseSubmodules = None }

        static member Create
            (
                ?conventionalCommits: bool,
                ?filterUnconventional: bool,
                ?requireConventional: bool,
                ?splitCommits: bool,
                ?commitPreprocessors: CommitPreprocessor[],
                ?commitParsers: CommitParser[],
                ?protectBreakingCommits: bool,
                ?filterCommits: bool,
                ?tagPattern: string,
                ?skipTags: string,
                ?ignoreTags: string,
                ?countTags: string,
                ?topoOrder: bool,
                ?topoOrderCommits: bool,
                ?sortCommits: string,
                ?linkParsers: LinkParser[],
                ?limitCommits: int,
                ?recurseSubmodules: bool
            ) =
            let defaultString = Option.defaultValue null

            { ConventionalCommits = conventionalCommits
              FilterUnconventional = filterUnconventional
              RequireConventional = requireConventional
              SplitCommits = splitCommits
              CommitPreprocessors =
                commitPreprocessors
                |> Option.defaultValue null
              CommitParsers =
                commitParsers
                |> Option.defaultValue null
              ProtectBreakingCommits = protectBreakingCommits
              FilterCommits = filterCommits
              TagPattern = defaultString tagPattern
              SkipTags = defaultString skipTags
              IgnoreTags = defaultString ignoreTags
              CountTags = defaultString countTags
              TopoOrder = topoOrder
              TopoOrderCommits = topoOrderCommits
              SortCommits = defaultString sortCommits
              LinkParsers =
                linkParsers
                |> Option.defaultValue null
              LimitCommits = limitCommits
              RecurseSubmodules = recurseSubmodules }

    [<CLIMutable; CompiledName("Remote")>]
    type RemoteOptions =
        { Owner: string
          Repo: string
          Token: string
          ApiUrl: string
          NativeTls: bool option }

        static member init =
            { Owner = null
              Repo = null
              Token = null
              ApiUrl = null
              NativeTls = None }

        static member Create(?owner: string, ?repo: string, ?token: string, ?apiUrl: string, ?nativeTls: bool) =
            let defaultString = Option.defaultValue null

            { Owner = defaultString owner
              Repo = defaultString repo
              Token = defaultString token
              ApiUrl = defaultString apiUrl
              NativeTls = nativeTls }

    [<CLIMutable>]
    type Config =
        { Bump: BumpOptions
          Git: GitOptions
          Changelog: ChangeLogOptions
          Remote: RemoteOptions }

        static member init =
            { Bump = BumpOptions.init
              Git = GitOptions.init
              Changelog = ChangeLogOptions.init
              Remote = RemoteOptions.init }

        static member Default =
            let changelogOpts =
                let header =
                    """
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

"""

                let body =
                    "
{%- macro remote_url() -%}
  https://github.com/{{ remote.github.owner }}/{{ remote.github.repo }}
{%- endmacro -%}

{% if version -%}
  ## [ {{ version | trim_start_matches(pat=\"v\") }} ] - {{ timestamp | date(format=\"%Y-%m-%d\") }}
{% else -%}
  <h2>

[Unreleased]

</h2>
{% endif -%}

{% for group, commits in commits | group_by(attribute=\"group\") %}
  <h3>{{ group | upper_first }}</h3>
  {%- for commit in commits %}
    - {{ commit.message | split(pat=\"\n\") | first | upper_first | trim }} \
      {% if commit.remote.username %} by @{{ commit.remote.username }}{%- endif -%}
      {% if commit.remote.pr_number %} in \
      [#{{ commit.remote.pr_number }}]({{ self::remote_url() }}/pull/{{ commit.remote.pr_number }}) \
      {%- endif -%}
  {% endfor %}
{% endfor %}

{%- if github.contributors | filter(attribute=\"is_first_time\", value=true) | length != 0 %}
  <h2>New Contributors</h2>
{%- endif -%}

{% for contributor in github.contributors | filter(attribute=\"is_first_time\", value=true) %}
  * @{{ contributor.username }} made their first contribution
    {%- if contributor.pr_number %} in \
      [#{{ contributor.pr_number }}]({{ self::remote_url() }}/pull/{{ contributor.pr_number }}) \
    {%- endif %}
{%- endfor %}
"

                let footer =
                    "
{%- macro remote_url() -%}
  https://github.com/{{ remote.github.owner }}/{{ remote.github.repo }}
{%- endmacro -%}

{% for release in releases -%}
    {% if release.version -%}
        {% if release.previous.version -%}
            [{{ release.version | trim_start_matches(pat=\"v\") }}]: \
                {{ self::remote_url() }}/compare/{{ release.previous.version }}..{{ release.version }}
        {% endif -%}
    {% else -%}
        [unreleased]: {{ self::remote_url() }}/compare/{{ release.previous.version }}..HEAD
    {% endif -%}
{% endfor %}
<!-- generated by git-cliff -->
<!-- using Partas Fake.Tools.GitCliff -->
"

                let trim = true
                ChangeLogOptions.Create (header, body, footer, trim)

            let gitOpts =
                let createParser group message =
                    CommitParser.Create (group = group, message = message)

                let parserAdded = createParser "Added"
                let parserRemoved = createParser "Removed"
                let parserFixed = createParser "Fixed"
                let parserChanged = createParser "Changed"

                { GitOptions.init with
                    ConventionalCommits = Some true
                    FilterUnconventional = Some false
                    CommitPreprocessors = [| CommitPreprocessor.Create (@"\((\w+\s)?#([0-9]+)\)", replace = "") |]
                    CommitParsers =
                        [| parserAdded "^[a|A]dd"
                           parserAdded "^[s|S]upport"
                           parserRemoved "^[r|R]emove"
                           parserAdded "^.*: add"
                           parserAdded "^.*: support"
                           parserRemoved "^.*: remove"
                           parserRemoved "^.*: delete"
                           parserFixed "^test"
                           parserFixed "^fix"
                           parserFixed "^.*: fix"
                           parserChanged "^.*" |]
                    FilterCommits = Some false
                    TopoOrder = Some false
                    SortCommits = "newest" }

            { Config.init with
                Git = gitOpts
                Changelog = changelogOpts }

        static member Create(?bump: BumpOptions, ?git: GitOptions, ?changeLog: ChangeLogOptions, ?remote: RemoteOptions) =
            { Bump =
                bump
                |> Option.defaultValue BumpOptions.init
              Git =
                git
                |> Option.defaultValue GitOptions.init
              Changelog =
                changeLog
                |> Option.defaultValue ChangeLogOptions.init
              Remote =
                remote
                |> Option.defaultValue RemoteOptions.init }

    let sprintfConfiguration (defaults: Config -> Config) =
        Config.Default
        |> defaults
        |> FSharpToml.toToml


    let writeConfiguration (defaults: Config -> Config) path =
        let config =
            Config.Default
            |> defaults
            |> FSharpToml.toToml

        File.create path
        File.writeString false path config

    /// <summary>
    /// Loads configuration from the file path/string
    /// </summary>
    /// <param name="path">If file does not exist, then tries to parse from string</param>
    let loadConfiguration path =
        if File.exists path then
            File.readAsString path
            |> FSharpToml.fromToml<Config>
        else
            path
            |> FSharpToml.fromToml<Config>

    let createConfiguration (execParams: ExecParams -> ExecParams) (template: CliffTemplates) =
        { ExecParams.Program = "git-cliff"
          WorkingDir = ""
          CommandLine = $"--init {template}"
          Args = [] }
        |> execParams
        |> Process.shellExec
        |> function
            | 0 -> ()
            | exitCode ->
                exitCode
                |> failwithf "Failed to create git-cliff config with exit code: %i"

type CliFlags =
    | Help
    | Version
    | Verbose
    | BumpedVersion
    | Latest
    | Current
    | Unreleased
    | TopoOrder
    | UseBranchTags
    | NoExec
    | Context
    | UseNativeTls

    override this.ToString() =
        match this with
        | Help -> "-h"
        | Version -> "-V"
        | Verbose -> "-v"
        | BumpedVersion -> "--bumped-version"
        | Latest -> "-l"
        | Current -> "--current"
        | Unreleased -> "-u"
        | TopoOrder -> "--topo-order"
        | UseBranchTags -> "--use-branch-tags"
        | NoExec -> "--no-exec"
        | Context -> "-x"
        | UseNativeTls -> "--use-native-tls"

type BumpStrategy =
    | Auto
    | Major
    | Minor
    | Patch

    override this.ToString() =
        match this with
        | Auto -> "auto"
        | Major -> "major"
        | Minor -> "minor"
        | Patch -> "patch"

type StripOptions =
    | Header
    | Footer
    | All

    override this.ToString() =
        match this with
        | Header -> "header"
        | Footer -> "footer"
        | All -> "all"

type SortStrategy =
    | Oldest
    | Newest

    override this.ToString() =
        match this with
        | Oldest -> "oldest"
        | Newest -> "newest"

type GitInfo =
    | GitHub of string
    | GitLab of string
    | BitBucket of string
    | Gitea of string

type CliParams =
    { Flags: CliFlags list
      Init: string
      Bump: BumpStrategy option
      Config: string
      ConfigUrl: string
      WorkDir: string
      Repository: string
      IncludePath: string
      ExcludePath: string
      TagPattern: string
      WithCommit: string
      WithTagMessage: string
      IgnoreTags: string
      CountTags: string
      SkipCommit: string
      Prepend: string
      Output: string
      Tag: string
      Body: ConfigHelper.CliffTemplates option
      FromContext: string
      Strip: StripOptions option
      Sort: SortStrategy option
      GitRepo: GitInfo option
      GitToken: GitInfo option
      Args: string }

    static member init =
        { Flags = []
          Init = null
          Bump = None
          Config = null
          ConfigUrl = null
          WorkDir = null
          Repository = null
          IncludePath = null
          ExcludePath = null
          TagPattern = null
          WithCommit = null
          WithTagMessage = null
          IgnoreTags = null
          CountTags = null
          SkipCommit = null
          Prepend = null
          Output = null
          Tag = null
          Body = None
          FromContext = null
          Strip = None
          Sort = None
          GitRepo = None
          GitToken = None
          CliParams.Args = null }

open System

let run (cliParams: CliParams -> CliParams) dir =
    let cliParams =
        CliParams.init
        |> cliParams

    let makeArg (value: string) (name: string) =
        if
            value
            |> ((<>) null)
        then
            let kebabFolder state character =
                if
                    character
                    |> Char.IsUpper
                then
                    $"-{character
                        |> Char.ToLower}"
                else
                    string character
                |> (+) state

            let state = ""

            $"-{name
                |> Seq.fold kebabFolder state} {value}"
        else
            ""

    let makeArgOrFlag name value =
        if value = null then "" else value
        |> makeArg name

    let inline enumStringOrNull value : string =
        value
        |> Option.map _.ToString()
        |> Option.defaultValue null

    let options =
        [ nameof cliParams.Init
          |> makeArgOrFlag cliParams.Init
          nameof cliParams.Bump
          |> makeArg (
              cliParams.Bump
              |> enumStringOrNull
          )
          nameof cliParams.Config
          |> makeArg cliParams.Config
          nameof cliParams.ConfigUrl
          |> makeArg cliParams.ConfigUrl
          "-workdir"
          |> makeArg cliParams.WorkDir
          nameof cliParams.Repository
          |> makeArg cliParams.Repository
          nameof cliParams.IncludePath
          |> makeArg cliParams.IncludePath
          nameof cliParams.ExcludePath
          |> makeArg cliParams.ExcludePath
          nameof cliParams.TagPattern
          |> makeArg cliParams.TagPattern
          nameof cliParams.WithCommit
          |> makeArg cliParams.WithCommit
          nameof cliParams.WithTagMessage
          |> makeArg cliParams.WithTagMessage
          nameof cliParams.IgnoreTags
          |> makeArg cliParams.IgnoreTags
          nameof cliParams.CountTags
          |> makeArg cliParams.CountTags
          nameof cliParams.SkipCommit
          |> makeArg cliParams.SkipCommit
          nameof cliParams.Prepend
          |> makeArg cliParams.Prepend
          nameof cliParams.Output
          |> makeArgOrFlag cliParams.Output
          nameof cliParams.Tag
          |> makeArg cliParams.Tag
          nameof cliParams.Body
          |> makeArg (
              cliParams.Body
              |> enumStringOrNull
          )
          nameof cliParams.FromContext
          |> makeArg cliParams.FromContext
          nameof cliParams.Strip
          |> makeArg (enumStringOrNull cliParams.Strip)
          nameof cliParams.Sort
          |> makeArg (enumStringOrNull cliParams.Sort)
          match cliParams.GitRepo, cliParams.GitToken with
          | Some (GitHub repo), Some (GitHub token) -> $"--github-repo {repo} --github-token {token}"
          | Some (Gitea repo), Some (Gitea token) -> $"--gitea-repo {repo} --gitea-token {token}"
          | Some (GitLab repo), Some (GitLab token) -> $"--gitlab-repo {repo} --gitlab-token {token}"
          | Some (BitBucket repo), Some (BitBucket token) -> $"--bitbucket-repo {repo} --bitbucket-token {token}"
          | None, Some (GitHub token) -> $"--github-token {token}"
          | None, Some (Gitea token) -> $"--gitea-token {token}"
          | None, Some (GitLab token) -> $"--gitlab-token {token}"
          | None, Some (BitBucket token) -> $"--bitbucket-token {token}"
          | Some (GitHub repo), None -> $"--github-repo {repo}"
          | Some (Gitea repo), None -> $"--gitea-repo {repo}"
          | Some (GitLab repo), None -> $"--gitlab-repo {repo}"
          | Some (BitBucket repo), None -> $"--bitbucket-repo {repo}"
          | None, None -> null
          | _ -> null ]
        |> List.filter String.isNotNullOrEmpty
        |> String.concat " "
        |> String.trim

    let flags =
        cliParams.Flags
        |> List.map _.ToString()
        |> String.concat " "
        |> String.trim

    { ExecParams.Program = "git-cliff"
      WorkingDir = dir
      CommandLine =
        [ flags; options ]
        |> String.concat " "
        |> String.trim
      Args = [] }
    |> Process.shellExec
    |> function
        | 0 -> ()
        | exitCode -> failwith $"git-cliff failed with exit code {exitCode}"

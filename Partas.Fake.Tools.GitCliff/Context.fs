module Fake.Tools.GitCliffContext

open System
open FSharp.Json


let private config = JsonConfig.create (jsonFieldNaming = Json.snakeCase)

type Author =
    { Name: string option
      Email: string option
      [<JsonField(Transform = typeof<Transforms.DateTimeEpoch>)>]
      Timestamp: DateTime option }

type Committer =
    { Name: string option
      Email: string option
      [<JsonField(Transform = typeof<Transforms.DateTimeEpoch>)>]
      Timestamp: DateTime option }

type CommitRange = { From: string; To: string }

type Previous = { Version: string option }

type Link =
    { Text: string option
      Href: string option }

type Footer =
    { Token: string option
      Separator: string option
      Value: string option
      Breaking: bool }

type Contributor =
    { Username: string option
      PrTitle: string option
      PrNumber: string option
      PrLabels: string list
      IsFirstTime: bool }

type Commit =
    { Id: string
      Message: string
      Group: string option
      Scope: string option
      Links: Link list
      Body: string option
      Footers: Footer list option
      BreakingDescription: string option
      Breaking: bool option
      Conventional: bool
      MergeCommit: bool
      Author: Author
      Committer: Committer
      RawMessage: string
      Remote: Contributor option
      Bitbucket: Contributor
      Gitea: Contributor
      Gitlab: Contributor
      Github: Contributor }


type GitRepository = { Contributors: Contributor list }

type Context =
    { Version: string option
      Message: string option
      Commits: Commit list
      CommitId: string option
      [<JsonField(Transform = typeof<Transforms.DateTimeEpoch>)>]
      Timestamp: DateTime option
      Repository: string option
      CommitRange: CommitRange option
      Previous: Context option
      SubmoduleCommits: Map<string, Commit>
      Extra: obj option
      Github: GitRepository
      Gitlab: GitRepository
      Gitea: GitRepository
      Bitbucket: GitRepository }

type JsonContent = Context list

module Json =
    let deserialize = Json.deserializeEx<JsonContent> config
    let serialize: JsonContent -> string = Json.serializeEx config

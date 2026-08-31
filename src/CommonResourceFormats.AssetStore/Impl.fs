namespace CommonResourceFormats.AssetStore

open System
open System.IO
open CommonResourceFormats.AssetStore.Core.Domain
open Freql.Sqlite
open CommonResourceFormats.AssetStore.Core

type AssetStoreContext(path: string) as this =
    let ctx =
        match File.Exists path with
        | true -> SqliteContext.Open(path)
        | false -> SqliteContext.Create(path)

    do Operations.Initialization.run ctx

    member _.AddScene() = Operations.Scenes.addNewScene ctx

    member _.LoadScene(sceneId: EntityId, version: Domain.Version) =
        Operations.Scenes.getScene ctx version sceneId

namespace CommonResourceFormats.AssetStore

open System
open System.IO
open CommonResourceFormats.AssetStore.Core.Domain
open Freql.Sqlite
open CommonResourceFormats.AssetStore.Core
open FsToolbox.GameDevelopment.Core

type AssetStoreContext(path: string) as this =
    let ctx =
        match File.Exists path with
        | true -> SqliteContext.Open(path)
        | false -> SqliteContext.Create(path)

    do Operations.Initialization.run ctx

    member _.AddScene(name: string) = Operations.Scenes.addNewScene ctx name

    member _.LoadScene(sceneId: EntityId, version: Domain.Version) =
        Operations.Scenes.getScene ctx version sceneId

    member _.AddSceneObject(sceneVersionId: EntityId, parentId: EntityId option, name: string, transform: Types.Transform) =
        Operations.Scenes.Objects.add ctx  sceneVersionId parentId name transform
        
        
    member _.GetSceneListings() =
        Operations.Scenes.getListings ctx
        
    member _.GetSceneVersion(sceneVersionId: EntityId) =
        Operations.Scenes.getVersion ctx sceneVersionId
        
    member _.UpdateSceneObjectTransform(sceneObjectId: EntityId, transform: Transform) =
        Operations.Scenes.Objects.updateTransform ctx sceneObjectId transform
        
        
    member _.AddNewAsset(asset: NewAsset) =
        Operations.Assets.addNew ctx asset
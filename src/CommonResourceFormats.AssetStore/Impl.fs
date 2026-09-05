namespace CommonResourceFormats.AssetStore

open System
open System.IO
open CommonResourceFormats.AssetStore.Core.Domain
open CommonResourceFormats.AssetStore.Store.Persistence
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

    member _.AddSceneObject
        (sceneVersionId: EntityId, parentId: EntityId option, name: string, transform: Types.Transform)
        =
        Operations.Scenes.Objects.add ctx sceneVersionId parentId name transform
        
     member _.AddSceneObjectComponent
        (sceneObjectId: EntityId, componentVersionId: EntityId, data: string option)
        =
        Operations.Scenes.Objects.addComponent ctx sceneObjectId componentVersionId data

    member _.GetSceneListings() = Operations.Scenes.getListings ctx

    member _.GetSceneVersion(sceneVersionId: EntityId) =
        Operations.Scenes.getVersion ctx sceneVersionId
        
     member _.GetSceneObjectComponents(sceneObjectId: EntityId) =
        Operations.Scenes.Objects.getComponents ctx sceneObjectId

    member _.UpdateSceneObjectTransform(sceneObjectId: EntityId, transform: Transform) =
        Operations.Scenes.Objects.updateTransform ctx sceneObjectId transform

    member _.AddNewAsset(asset: NewAsset) = Operations.Assets.addNew ctx asset

    member _.GetAssetListings() = Operations.Assets.getListings ctx

    member _.GetAssetVersion(assetVersionId: EntityId) =
        Operations.Assets.getVersion ctx assetVersionId
    
    member _.AddNewComponent(comp: NewComponent) = Operations.Components.addNew ctx comp
    
    member _.GetComponentListings() = Operations.Components.getListings ctx
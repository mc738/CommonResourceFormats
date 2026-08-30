namespace CommonResourceFormats.AssetStore.Operations

open System
open System.Collections.Generic
open CommonResourceFormats.AssetStore.Core.Domain
open Freql.Sqlite
open CommonResourceFormats.AssetStore.Core
open CommonResourceFormats.AssetStore.Store.Persistence

module Scenes =

    module Internal =
        let getSceneAndVersionRecords (ctx: SqliteContext) (version: Domain.Version) (sceneId: Guid) =
            Operations.selectSceneRecord ctx [ "WHERE id = @0" ] [ sceneId ]
            |> Option.bind (fun sr ->
                match version with
                | Version.Latest -> Operations.selectSceneVersionRecord ctx [ "WHERE scene_id = @0" ] [ sceneId ]
                | Version.Specific i ->
                    Operations.selectSceneVersionRecord
                        ctx
                        [ "WHERE scene_id = @0 AND version = @1" ]
                        [ sceneId; version ]
                |> Option.map (fun svr -> sr, svr))



    let getScene (ctx: SqliteContext) (version: Domain.Version) (sceneId: Guid) =
        Internal.getSceneAndVersionRecords ctx version sceneId
        |> Option.map (fun (sr, svr) ->
            let topLevelObjects = ResizeArray<Records.SceneObject>()
            let buckets = Dictionary<Guid, ResizeArray<Records.SceneObject>>()

            // Get and split the objects
            Operations.selectSceneObjectRecords ctx [ "WHERE scene_version_id = @0" ] [ svr.Id ]
            |> List.iter (fun sor ->
                match sor.Parent with
                | None -> topLevelObjects.Add(sor)
                | Some pId ->
                    let parentId = Guid.ParseExact(pId, "n")

                    if buckets.ContainsKey parentId |> not then
                        buckets.Add(parentId, ResizeArray<Records.SceneObject>([ sor ]))
                    else
                        buckets[parentId].Add(sor))

            let rec traverse (sor: Records.SceneObject) =
                let sId = Guid.ParseExact(sor.Id, "n")

                let children =
                    match buckets.ContainsKey sId with
                    | false -> ResizeArray<SceneObject>()
                    | true ->
                        let children = buckets[sId]

                        children |> Seq.map traverse |> ResizeArray

                ({ Id = sId
                   Name = failwith "todo"
                   Children = children
                   Components =
                     Operations.selectSceneObjectComponentRecords ctx [ "WHERE scene_object_id = @0" ] [ sor.Id ]
                     |> List.map (fun ocr ->
                         let cId = Guid.ParseExact(ocr.Id, "n")

                         ({ Id = cId
                            Name = failwith "todo"
                            Type = ocr.ComponentType
                            JsonData = ocr.ComponentData
                            Assets =
                              Operations.selectSceneObjectComponentAssetRecords
                                  ctx
                                  [ "WHERE scene_object_component_id = @0" ]
                                  [ ocr.Id ]
                              |> List.map (fun car ->
                                  let caId = Guid.ParseExact(car.Id, "n")

                                  let r =
                                      match
                                          Operations.selectAssetVersionRecord
                                              ctx
                                              [ "WHERE id = @0" ]
                                              [ car.AssetVersionId ]
                                      with
                                      | None -> Result.Error "Failed to load asset version"
                                      | Some avr ->
                                          match
                                              Operations.selectAssetRecord ctx [ "WHERE id = @0" ] [ avr.AssetId ]
                                          with
                                          | None -> Error "Failed to load asset"
                                          | Some ar ->
                                              ({ Id = caId
                                                 Metadata =
                                                   Operations.selectSceneObjectComponentAssetMetadataItemRecords
                                                       ctx
                                                       [ "WHERE = @0" ]
                                                       []
                                                   |> List.map (fun md -> md.ItemKey, md.ItemValue)
                                                   |> Map.ofList
                                                 Asset =
                                                   { Id = Guid.ParseExact(avr.Id, "n")
                                                     AssetId = Guid.ParseExact(ar.Id, "n")
                                                     AssetType = ar.AssetType
                                                     Version = avr.Version
                                                     IsPrototype = avr.IsPrototype
                                                     Metadata =
                                                       Operations.selectAssetMetadataItemRecords
                                                           ctx
                                                           [ "WHERE asset_id = @0" ]
                                                           [ ar.Id ]
                                                       |> List.map (fun md -> md.ItemKey, md.ItemValue)
                                                       |> Map.ofList
                                                     VersionMetadata =
                                                       Operations.selectAssetVersionMetadataItemRecords
                                                           ctx
                                                           [ "WHERE asset_version_id = @0" ]
                                                           [ avr.Id ]
                                                       |> List.map (fun md -> md.ItemKey, md.ItemValue)
                                                       |> Map.ofList
                                                     Resources =
                                                       Operations.selectAssetVersionResourceRecords
                                                           ctx
                                                           [ "WHERE asset_version_id = @0" ]
                                                           [ avr.Id ]
                                                       |> List.map (fun lnk ->
                                                           match
                                                               Operations.selectResourceRecord
                                                                   ctx
                                                                   [ "WHERE id = @0" ]
                                                                   [ lnk.ResourceId ]
                                                           with
                                                           | None -> failwith "todo"
                                                           | Some rr ->
                                                               ({ Id = Guid.ParseExact(rr.Id, "n")
                                                                  Name = rr.Name
                                                                  Description = rr.Description
                                                                  FileType = rr.FileType
                                                                  Hash = rr.Hash
                                                                  Metadata =
                                                                    Operations.selectResourceMetadataItemRecords
                                                                        ctx
                                                                        [ "WHERE resource_id = @0" ]
                                                                        [ rr.Id ]
                                                                    |> List.map (fun md -> md.ItemKey, md.ItemValue)
                                                                    |> Map.ofList }
                                                               : AssetResource))
                                                       |> ResizeArray

                                                   } }
                                              : SceneObjectComponentAsset)
                                              |> Result.Ok

                                  match r with
                                  | Ok oca -> oca
                                  | Error errorValue -> failwith "todo")
                              |> ResizeArray
                            Metadata =
                              Operations.selectSceneObjectComponentMetadataItemRecords
                                  ctx
                                  [ "WHERE scene_object_component_id = @0" ]
                                  [ ocr.Id ]
                              |> List.map (fun r -> r.ItemKey, r.ItemValue)
                              |> Map.ofList }
                         : SceneObjectComponent))
                     |> ResizeArray
                   Metadata =
                     Operations.selectSceneObjectMetadataItemRecords ctx [ "WHERE scene_object_id = @0" ] [ sor.Id ]
                     |> List.map (fun md -> md.ItemKey, md.ItemValue)
                     |> Map.ofList }
                : SceneObject)

            ({ Objects = topLevelObjects |> Seq.map traverse |> ResizeArray }: Scene))


    let addNewScene (ctx: SqliteContext) =
        let id = Guid.NewGuid()

        Operations.insertScene ctx ({ Id = id.ToString("n"); Name = "" }: Parameters.NewScene)

        Operations.insertSceneVersion
            ctx
            ({ Id = Guid.NewGuid().ToString("n")
               SceneId = id.ToString("n")
               Version = 1 }
            : Parameters.NewSceneVersion)

        id

    let addSceneObject (ctx: SqliteContext) (version: Domain.Version) (sceneId: Guid)=
        Internal.getSceneAndVersionRecords ctx version sceneId
        |> Option.map (fun (sr, svr) ->
            
            
            
            Operations.insertSceneObject ctx ({
                Id = ""
                SceneVersionId = failwith "todo"
                Parent = failwith "todo"
                TransformPositionX = failwith "todo"
                TransformPositionY = failwith "todo"
                TransformPositionZ = failwith "todo"
                TransformRotationX = failwith "todo"
                TransformRotationY = failwith "todo"
                TransformRotationZ = failwith "todo"
                TransformRotationW = failwith "todo"
                TransformScaleX = failwith "todo"
                TransformScaleY = failwith "todo"
                TransformScaleZ = failwith "todo"
            }: Parameters.NewSceneObject)
            )
        
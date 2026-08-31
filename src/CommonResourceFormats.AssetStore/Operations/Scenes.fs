namespace CommonResourceFormats.AssetStore.Operations

open System
open System.Collections.Generic
open CommonResourceFormats.AssetStore.Core.Domain
open Freql.Sqlite
open CommonResourceFormats.AssetStore.Core
open CommonResourceFormats.AssetStore.Store.Persistence
open FsToolbox.GameDevelopment.Core

module Scenes =

    module Internal =


        let entityTypeName = "scene"

        let getSceneAndVersionRecords (ctx: SqliteContext) (sceneId: EntityId) (version: Version) =
            Generic.getEntityAndVersion
                Operations.selectSceneRecord
                Operations.selectSceneVersionRecord
                entityTypeName
                ctx
                sceneId
                version


    [<RequireQualifiedAccess>]
    module Objects =


        type AddResult = Successful of EntityId

        and AddFailure = Generic of Generic.GenericFailure

        let add
            (ctx: SqliteContext)
            (version: Domain.Version)
            (sceneId: EntityId)
            (parent: EntityId option)
            (transform: Transform)
            =
            Internal.getSceneAndVersionRecords ctx sceneId version
            |> Result.bind (fun (sr, svr) ->
                let eId = EntityId.Create()

                Operations.insertSceneObject
                    ctx
                    ({ Id = eId.Serialize()
                       Name = "[obj]"
                       SceneVersionId = svr.Id
                       Parent = parent |> Option.map _.Serialize()
                       TransformPositionX = transform.Position.X
                       TransformPositionY = transform.Position.Y
                       TransformPositionZ = transform.Position.Z
                       TransformRotationX = transform.Rotation.X
                       TransformRotationY = transform.Rotation.Y
                       TransformRotationZ = transform.Rotation.Z
                       TransformRotationW = transform.Rotation.W
                       TransformScaleX = transform.Scale.X
                       TransformScaleY = transform.Scale.Y
                       TransformScaleZ = transform.Scale.Z }
                    : Parameters.NewSceneObject)

                Ok eId)

        let build (ctx: SqliteContext) (sor: Records.SceneObject) (children: ResizeArray<SceneObject>) =
            let eId = EntityId.Deserialize sor.Id

            ({ Id = eId
               Name = sor.Name
               Children = children
               Components =
                 Operations.selectSceneObjectComponentRecords ctx [ "WHERE scene_object_id = @0" ] [ sor.Id ]
                 |> List.map (fun ocr ->
                     let cId = EntityId.Deserialize ocr.Id

                     //match Components.getVersion ctx cId with
                     //|


                     match Components.getVersion ctx cId with
                     | Error errorValue -> failwith "todo"
                     | Ok resultValue ->
                         let comp =
                             match resultValue with
                             | Components.BuildComponentResults.Success comp -> comp
                             | Components.BuildComponentResults.Partial(comp, errors) ->
                                 // TODO report on errors.

                                 comp

                         ({ Id = cId
                            OverrideComponentData = ocr.OverrideComponentData
                            Component = comp
                            Metadata =
                              Operations.selectSceneObjectComponentMetadataItemRecords
                                  ctx
                                  [ "WHERE scene_object_component_id = @0" ]
                                  [ ocr.Id ]
                              |> List.map (fun r -> r.ItemKey, r.ItemValue)
                              |> Map.ofList }
                         : SceneObjectComponent))
                 |> ResizeArray
               Metadata = [] |> Map.ofList }
            : SceneObject)

    [<RequireQualifiedAccess>]
    type GetSceneFailure = Generic of Generic.GenericFailure

    let getScene (ctx: SqliteContext) (version: Domain.Version) (sceneId: EntityId) =
        Internal.getSceneAndVersionRecords ctx sceneId version
        //|> Result.mapError GetSceneFailure.Generic
        |> Result.map (fun (sr, svr) ->
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

                Objects.build ctx sor children

            ({ Objects = topLevelObjects |> Seq.map (traverse) |> ResizeArray }: Scene))

    [<RequireQualifiedAccess>]
    type AddSceneResult =
        | Success of SceneId: Guid * SceneVersionId: Guid
        | Failure of AddSceneFailure

    and AddSceneFailure = UnhandledException of exn

    let addNewScene (ctx: SqliteContext) (name: string) =
        let sId = EntityId.Create()
        let svId = EntityId.Create()

        Operations.insertScene ctx ({ Id = sId.Serialize(); Name = name }: Parameters.NewScene)

        Operations.insertSceneVersion
            ctx
            ({ Id = svId.Serialize()
               SceneId = sId.Serialize()
               Version = 1 }
            : Parameters.NewSceneVersion)

        id

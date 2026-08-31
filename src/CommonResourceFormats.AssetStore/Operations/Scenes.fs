namespace CommonResourceFormats.AssetStore.Operations

open System
open System.Collections.Generic
open System.Numerics
open CommonResourceFormats.AssetStore.Core.Domain
open CommonResourceFormats.AssetStore.Core.Exceptions
open Freql.Sqlite
open CommonResourceFormats.AssetStore.Core
open CommonResourceFormats.AssetStore.Store.Persistence
open FsToolbox.GameDevelopment.Core

module Scenes =


    [<RequireQualifiedAccess>]
    module Objects =


        type AddResult = Successful of EntityId

        and AddFailure = SceneVersionNotFound of EntityId

        let add
            (ctx: SqliteContext)
            (sceneVersionId: EntityId)
            (parent: EntityId option)
            (name: string)
            (transform: Transform)
            =
            match Operations.selectSceneVersionRecord ctx [ "WHERE id = @0" ] [ sceneVersionId.Serialize() ] with
            | None -> Error(AddFailure.SceneVersionNotFound sceneVersionId)
            | Some svr ->
                let eId = EntityId.Create()

                Operations.insertSceneObject
                    ctx
                    ({ Id = eId.Serialize()
                       Name = name
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

                Ok eId

        let buildTransform (sor: Records.SceneObject) =
            let mutable t = Transform.Default

            t.Position <- Vector3(sor.TransformPositionX, sor.TransformPositionY, sor.TransformPositionZ)

            t.Rotation <-
                Quaternion(
                    sor.TransformRotationX,
                    sor.TransformRotationY,
                    sor.TransformRotationZ,
                    sor.TransformRotationW
                )

            t.Scale <- Vector3(sor.TransformScaleX, sor.TransformScaleY, sor.TransformScaleZ)

            t

        let build (ctx: SqliteContext) (sor: Records.SceneObject) (children: ResizeArray<SceneObject>) =
            let eId = EntityId.Deserialize sor.Id

            ({ Id = eId
               Name = sor.Name
               Children = children
               Transform = buildTransform sor
               Components =
                 Operations.selectSceneObjectComponentRecords ctx [ "WHERE scene_object_id = @0" ] [ sor.Id ]
                 |> List.map (fun ocr ->
                     let cId = EntityId.Deserialize ocr.Id

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

        let updateTransform (ctx: SqliteContext) (sceneObjectId: EntityId) (transform: Transform) =
            ctx.ExecuteVerbatimNonQuery(
                """
                UPDATE scene_objects 
                SET
                    transform_position_x = @0,
                    transform_position_y = @1,
                    transform_position_z = @2,
                    transform_rotation_x = @3,
                    transform_rotation_y = @4,
                    transform_rotation_z = @5,
                    transform_rotation_w = @6,
                    transform_scale_x = @7,
                    transform_scale_y = @8,
                    transform_scale_z = @9
                WHERE 
                    id = @10
            """,
                [ transform.Position.X
                  transform.Position.Y
                  transform.Position.Z
                  transform.Rotation.X
                  transform.Rotation.Y
                  transform.Rotation.Z
                  transform.Rotation.W
                  transform.Scale.X
                  transform.Scale.Y
                  transform.Scale.Z ]
            )
            |> ignore


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


        let build (ctx: SqliteContext) (sr: Records.Scene) (svr: Records.SceneVersion) =
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

            ({ Id = EntityId.Deserialize sr.Id
               Name = sr.Name
               VersionId = EntityId.Deserialize svr.Id
               Version = svr.Version
               Objects = topLevelObjects |> Seq.map (traverse) |> ResizeArray }
            : Scene)



    [<RequireQualifiedAccess>]
    type GetSceneFailure = Generic of Generic.GenericFailure

    let getScene (ctx: SqliteContext) (version: Domain.Version) (sceneId: EntityId) =
        Internal.getSceneAndVersionRecords ctx sceneId version
        //|> Result.mapError GetSceneFailure.Generic
        |> Result.map (fun (sr, svr) -> Internal.build ctx sr svr)

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

        { Id = sId
          Name = name
          VersionId = svId
          Version = 1
          Objects = ResizeArray<SceneObject>() }

    let getListings (ctx: SqliteContext) =
        { Scenes =
            Operations.selectSceneRecords ctx [] []
            |> List.map (fun sr ->
                ({ Id = EntityId.Deserialize sr.Id
                   Name = sr.Name
                   Versions =
                     Operations.selectSceneVersionRecords ctx [ "WHERE scene_id = @0" ] [ sr.Id ]
                     |> List.map (fun svr ->
                         ({ Id = EntityId.Deserialize svr.Id
                            Version = svr.Version }
                         : SceneVersionListingItem)) }
                : SceneListingItem)) }

    [<RequireQualifiedAccess>]
    type GetVersionFailure = SceneVersionNotFound of EntityId

    let getVersion (ctx: SqliteContext) (sceneVersionId: EntityId) =
        match Operations.selectSceneVersionRecord ctx [ "WHERE id = @0" ] [ sceneVersionId.Serialize() ] with
        | None -> Error(GetVersionFailure.SceneVersionNotFound sceneVersionId)
        | Some svr ->
            let sr =
                Operations.selectSceneRecord ctx [ "WHERE id = @0" ] [ svr.SceneId ]
                |> Option.defaultWith (fun () -> raise (IllegalDatabaseState()))

            Internal.build ctx sr svr |> Ok

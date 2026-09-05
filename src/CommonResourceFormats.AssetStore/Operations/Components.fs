namespace CommonResourceFormats.AssetStore.Operations

open System
open System.Collections.Generic
open CommonResourceFormats.AssetStore.Core.Domain
open Freql.Sqlite
open CommonResourceFormats.AssetStore.Store.Persistence



[<RequireQualifiedAccess>]
module Components =

    [<ReferenceEquality>]
    type BuildComponentResults =
        | Success of Component
        | Partial of Component * BuildComponentError list

        member bcr.Component =
            match bcr with
            | Success comp
            | Partial(comp, _) -> comp

    and [<RequireQualifiedAccess>] BuildComponentError = AssetVersion of Assets.GetVersionFailure

    module private Internal =

        let entityTypeName = "component"

        let getComponentAndVersion (ctx: SqliteContext) (componentId: EntityId) (version: Version) =
            Generic.getEntityAndVersion
                Operations.selectComponentRecord
                Operations.selectComponentVersionRecord
                entityTypeName
                ctx
                componentId
                version

        let getMetadata (ctx: SqliteContext) (componentId: EntityId) =
            Generic.getMetadataForEntity ctx entityTypeName componentId

        let getVersionMetadata (ctx: SqliteContext) (componentVersionId: EntityId) =
            Generic.getMetadataForEntity ctx $"{entityTypeName}_version" componentVersionId

        let setMetadataValue (ctx: SqliteContext) (componentId: EntityId) (key: string) (value: string) =
            match
                Operations.selectComponentMetadataItemRecord
                    ctx
                    [ "WHERE component_id = @0" ]
                    [ componentId.Serialize() ]
            with
            | None ->
                ({ ComponentId = componentId.Serialize()
                   ItemKey = key
                   ItemValue = value }
                : Parameters.NewComponentMetadataItem)
                |> Operations.insertComponentMetadataItem ctx
            | Some value ->
                ctx.ExecuteVerbatimNonQueryAnon(
                    "UPDATE component_metadata SET item_value = @0 WHERE component_id = @1 AND item_key = @2",
                    [ value; componentId.Serialize(); key ]
                )
                |> ignore

        let setVersionMetadataValue (ctx: SqliteContext) (componentVersionId: EntityId) (key: string) (value: string) =
            match
                Operations.selectComponentVersionMetadataItemRecord
                    ctx
                    [ "WHERE component_version_id = @0" ]
                    [ componentVersionId.Serialize() ]
            with
            | None ->
                ({ ComponentId = componentVersionId.Serialize()
                   ItemKey = key
                   ItemValue = value }
                : Parameters.NewComponentMetadataItem)
                |> Operations.insertComponentMetadataItem ctx
            | Some value ->
                ctx.ExecuteVerbatimNonQueryAnon(
                    "UPDATE component_metadata SET item_value = @0 WHERE component_id = @1 AND item_key = @2",
                    [ value; componentVersionId.Serialize(); key ]
                )
                |> ignore

        let buildModel (ctx: SqliteContext) (er: Records.Component) (evr: Records.ComponentVersion) =
            let entityId = EntityId.Deserialize er.Id

            let errors = ResizeArray<BuildComponentError>()

            let comp =
                { Id = entityId
                  VersionId = EntityId.Deserialize evr.Id
                  Version = evr.Version
                  Name = er.Name
                  ComponentType = er.ComponentType
                  Metadata = getMetadata ctx entityId
                  VersionMetadata = getVersionMetadata ctx entityId
                  SerializedData = evr.ComponentData
                  Assets =
                    Operations.selectComponentAssetRecords ctx [ "WHERE component_version_id = @0" ] [ evr.Id ]
                    |> List.choose (fun car ->

                        let caId = EntityId.Deserialize car.Id

                        match Assets.getVersion ctx (EntityId.Deserialize car.AssetVersionId) with
                        | Ok asset ->
                            KeyValuePair(
                                caId,
                                ({ Id = caId
                                   Asset = asset
                                   Metadata = Generic.getMetadataForEntity ctx "component_asset" caId }
                                : ComponentAsset)
                            )
                            |> Some
                        | Error errorValue ->
                            errors.Add(BuildComponentError.AssetVersion errorValue)
                            None)
                    |> Dictionary<ComponentAssetId, ComponentAsset> }

            match errors.Count > 0 with
            | true -> BuildComponentResults.Partial(comp, errors |> Seq.toList)
            | false -> BuildComponentResults.Success comp

    // ********** Add **************

    let add (ctx: SqliteContext) (componentType: string) =
        let id = EntityId.Create()

        ({ Id = id.Serialize()
           Name = ""
           ComponentType = componentType }
        : Parameters.NewComponent)
        |> Operations.insertComponent ctx

    [<RequireQualifiedAccess>]
    type AddVersionFailure =
        | ComponentNotFound of EntityId
        | ComponentVersionAlreadyExists of EntityId * int

    let addVersion (ctx: SqliteContext) (componentId: EntityId) (version: Version) (serializedComponentData: string) =
        //match Operations.selectResourceRecord ctx [ "WHERE id = @0;" ] [ componentId ] with
        // |
        let id = EntityId.Create()

        match Operations.selectComponentRecord ctx [ "WHERE id = @0" ] [ componentId.Serialize() ] with
        | None -> Error(AddVersionFailure.ComponentNotFound componentId)
        | Some cr ->
            match version with
            | Version.Latest ->

                let (previous, versionNumber) =
                    match
                        Operations.selectComponentVersionRecord
                            ctx
                            [ "WHERE component_id = @0 ORDER BY version DESC LIMIT 1" ]
                            [ componentId.Serialize() ]
                    with
                    | None -> (None, 1) // This is the first.
                    | Some value -> (Some value, value.Version + 1)

                // TODO handle cloning previous version.

                ({ Id = id.Serialize()
                   ComponentId = cr.Id
                   Version = versionNumber
                   ComponentData = serializedComponentData }
                : Parameters.NewComponentVersion)
                |> Operations.insertComponentVersion ctx

                Ok id
            | Version.Specific i ->
                match
                    Operations.selectComponentVersionRecord
                        ctx
                        [ "WHERE component_id = @0 AND version = @1" ]
                        [ cr.Id; i ]
                with
                | Some _ -> failwith ""
                | None ->
                    ({ Id = id.Serialize()
                       ComponentId = cr.Id
                       Version = i
                       ComponentData = serializedComponentData }
                    : Parameters.NewComponentVersion)
                    |> Operations.insertComponentVersion ctx

                Ok id

    let addNew (ctx: SqliteContext) (comp: NewComponent) =
        let createdOn = DateTime.UtcNow

        ({ Id = comp.Id.Serialize()
           Name = comp.Name
           ComponentType = comp.ComponentType }
        : Parameters.NewComponent)
        |> Operations.insertComponent ctx


        ({ Id = comp.VersionId.Serialize()
           ComponentId = comp.Id.Serialize()
           Version = 1
           ComponentData = comp.SerializedData }
        : Parameters.NewComponentVersion)
        |> Operations.insertComponentVersion ctx

        // TODO add metadata

        ()

    // ********** Update **********
    [<RequireQualifiedAccess>]
    type UpdateVersionComponentDataFailure = ComponentVersionNotFound of EntityId

    let updateVersionComponentData
        (ctx: SqliteContext)
        (componentVersionId: EntityId)
        (serializedComponentData: string)
        =
        match Operations.selectComponentVersionRecord ctx [ "WHERE id = @0" ] [ componentVersionId.Serialize() ] with
        | None -> Error(UpdateVersionComponentDataFailure.ComponentVersionNotFound componentVersionId)
        | Some _ ->
            ctx.ExecuteVerbatimNonQueryAnon(
                "UPDATE component_versions SET component_data = @0 WHERE id = @1",
                [ serializedComponentData; componentVersionId.Serialize() ]
            )
            |> ignore

            Ok()


    type GetFailure =
        | ComponentNotFound of EntityId
        | ComponentVersionNotFound of EntityId * Version

    // ************ Metadata ***********

    let getMetadata (ctx: SqliteContext) (componentId: EntityId) =
        Generic.getMetadataForEntity ctx Internal.entityTypeName componentId

    let getVersionMetadata (ctx: SqliteContext) (componentVersionId: EntityId) =
        Generic.getMetadataForEntity ctx $"{Internal.entityTypeName}_version" componentVersionId

    let setMetadataValue (ctx: SqliteContext) (componentId: EntityId) (key: string) (value: string) =
        match
            Operations.selectComponentMetadataItemRecord ctx [ "WHERE component_id = @0" ] [ componentId.Serialize() ]
        with
        | None ->
            ({ ComponentId = componentId.Serialize()
               ItemKey = key
               ItemValue = value }
            : Parameters.NewComponentMetadataItem)
            |> Operations.insertComponentMetadataItem ctx
        | Some value ->
            ctx.ExecuteVerbatimNonQueryAnon(
                "UPDATE component_metadata SET item_value = @0 WHERE component_id = @1 AND item_key = @2",
                [ value; componentId.Serialize(); key ]
            )
            |> ignore

    let setVersionMetadataValue (ctx: SqliteContext) (componentVersionId: EntityId) (key: string) (value: string) =
        match
            Operations.selectComponentVersionMetadataItemRecord
                ctx
                [ "WHERE component_version_id = @0" ]
                [ componentVersionId.Serialize() ]
        with
        | None ->
            ({ ComponentId = componentVersionId.Serialize()
               ItemKey = key
               ItemValue = value }
            : Parameters.NewComponentMetadataItem)
            |> Operations.insertComponentMetadataItem ctx
        | Some value ->
            ctx.ExecuteVerbatimNonQueryAnon(
                "UPDATE component_metadata SET item_value = @0 WHERE component_id = @1 AND item_key = @2",
                [ value; componentVersionId.Serialize(); key ]
            )
            |> ignore

    [<RequireQualifiedAccess>]
    type GetVersionFailure = VersionNotFound of EntityId

    let getVersion (ctx: SqliteContext) (componentVersionId: EntityId) =
        match Operations.selectComponentVersionRecord ctx [ "WHERE id = @0" ] [ componentVersionId.Serialize() ] with
        | None -> Error(GetVersionFailure.VersionNotFound componentVersionId)
        | Some evr ->
            let er =
                Operations.selectComponentRecord ctx [ "WHERE id = @0" ] [ evr.ComponentId ]
                |> Option.defaultWith (fun () -> failwith "Component missing, this represents a illegal database state")

            Internal.buildModel ctx er evr |> Ok

    // ************ Get ************
    let get (ctx: SqliteContext) (componentId: EntityId) (version: Version) =
        match Internal.getComponentAndVersion ctx componentId version with
        | Error errorValue ->
            match errorValue with
            | Generic.EntityNotFound(_, id) -> Error <| GetFailure.ComponentNotFound id
            | Generic.EntityVersionNotFound(_, id, version) -> Error <| GetFailure.ComponentVersionNotFound(id, version)
        | Ok(er, evr) -> Internal.buildModel ctx er evr |> Ok


    let getListings (ctx: SqliteContext) =
        { Entities =
            Operations.selectComponentRecords ctx [] []
            |> List.map (fun sr ->
                ({ Id = EntityId.Deserialize sr.Id
                   Name = sr.Name
                   Versions =
                     Operations.selectComponentVersionRecords ctx [ "WHERE component_id = @0" ] [ sr.Id ]
                     |> List.map (fun svr ->
                         ({ Id = EntityId.Deserialize svr.Id
                            Version = svr.Version }
                         : EntityVersionListingItem)) }
                : EntitiesListingItem)) }

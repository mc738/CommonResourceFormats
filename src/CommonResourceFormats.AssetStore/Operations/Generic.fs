namespace CommonResourceFormats.AssetStore.Operations

open CommonResourceFormats.AssetStore.Core.Domain
open CommonResourceFormats.AssetStore.Store.Persistence
open Freql.Sqlite

[<RequireQualifiedAccess>]
module Generic =

    type GenericFailure =
        | EntityNotFound of EntityTypeName: string * Id: EntityId
        | EntityVersionNotFound of EntityTypeName: string * Id: EntityId * Version: Version

    let getEntityAndVersion<'TEntity, 'TEntityVersion>
        (getEntityFn: SqliteContext -> string list -> obj list -> 'TEntity option)
        (getEntityVersionFn: SqliteContext -> string list -> obj list -> 'TEntityVersion option)
        (entityTypeName: string)
        (ctx: SqliteContext)
        (entityId: EntityId)
        (version: Version)
        =
        match getEntityFn ctx [ "WHERE id = @0" ] [ entityId.Serialize() ] with
        | None -> Error(EntityNotFound(entityTypeName, entityId))
        | Some er ->
            match version with
            | Version.Specific i ->
                getEntityVersionFn
                    ctx
                    [ $"WHERE {entityTypeName}_id = @0 AND version = @1" ]
                    [ entityId.Serialize(); i ]
            | Version.Latest ->
                getEntityVersionFn
                    ctx
                    [ "WHERE {entityTypeName}_id = @0 ORDER BY version DESC LIMIT 1;" ]
                    [ entityId.Serialize() ]
            |> function
                | None -> Error(EntityVersionNotFound(entityTypeName, entityId, version))
                | Some evr -> Ok(er, evr)

    let getMetadataForEntity (ctx: SqliteContext) (entityTypeName: string) (entityId: EntityId) =
        ctx.Bespoke(
            $"SELECT item_key, item_value FROM {entityTypeName}_metadata WHERE {entityTypeName}_id = @0",
            [ entityId.Serialize() ],
            fun r ->
                [ while r.Read() do
                      r.GetString(0), r.GetString(1) ]
        )
        |> EntityMetadata.Create

    let upsertMetadataItem
        (ctx: SqliteContext)
        (entityTypeName: string)
        (entityId: EntityId)
        (key: EntityKey)
        (value: string)
        =
        ctx.Bespoke(
            $"SELECT item_key, item_value FROM {entityTypeName}_metadata WHERE {entityTypeName}_id = @0 AND item_key = @1",
            [ entityId.Serialize(); key.Serialize() ],
            fun r ->
                [ while r.Read() do
                      r.GetString(0), r.GetString(1) ]
        )
        |> List.tryHead
        |> function
            | Some v ->
                ctx.ExecuteVerbatimNonQueryAnon(
                    $"UPDATE {entityTypeName}_metadata SET item_value = @0 WHERE item_key = @1",
                    [ value; key.Serialize() ]
                )
            | None ->
                ctx.ExecuteVerbatimNonQueryAnon(
                    $"INSERT INTO {entityTypeName}_metadata VALUES (@0, @1, @2)",
                    [ entityId.Serialize(); key.Serialize(); value ]
                )

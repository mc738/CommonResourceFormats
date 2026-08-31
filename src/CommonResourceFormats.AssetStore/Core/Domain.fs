namespace CommonResourceFormats.AssetStore.Core.Domain

open System

module private Cfg =
    let serializationFormation = "n"

[<RequireQualifiedAccess>]
type EntityId =
    | Guid of Guid

    static member Deserialize(str: string) =
        Guid.ParseExact(str, Cfg.serializationFormation) |> EntityId.Guid

    static member Create() = EntityId.Guid <| Guid.NewGuid()

    member eid.Serialize() =
        match eid with
        | Guid guid -> guid.ToString(Cfg.serializationFormation)

[<RequireQualifiedAccess>]
type Version =
    | Latest
    | Specific of int


type Asset =
    { Id: EntityId
      VersionId: EntityId
      AssetType: string
      Version: int
      IsPrototype: bool
      Metadata: Map<string, string>
      VersionMetadata: Map<string, string>
      Resources: AssetResource ResizeArray }

and AssetResource =
    { Id: EntityId
      Name: string
      Description: string
      FileType: string
      Hash: string
      Metadata: Map<string, string> }

type ComponentAsset =
    { Id: EntityId
      Asset: Asset
      Metadata: Map<string, string> }

type Component =
    { Id: EntityId
      VersionId: EntityId
      Version: int
      ComponentType: string

      Metadata: Map<string, string>
      VersionMetadata: Map<string, string>

      SerializedData: string

      Assets: ComponentAsset ResizeArray }

type SceneObject =
    { Id: EntityId
      Name: string
      Children: SceneObject ResizeArray
      Components: SceneObjectComponent ResizeArray
      Metadata: Map<string, string> }

and SceneObjectComponent =
    {
        Id: EntityId
        /// <summary>
        /// The component data is stored as a json string.
        /// It is up to the consumer to deserialize it.
        /// This is not 100% ideal but is flexible.
        /// </summary>
        OverrideComponentData: string option
        Component: Component
        Metadata: Map<string, string>
    }

type Scene =
    { Id: EntityId
      VersionId: EntityId
      Version: int
      Name: string

      // TODO metadata

      Objects: SceneObject ResizeArray }


type SceneListings = { Scenes: SceneListingItem list }

and SceneListingItem =
    { Id: EntityId
      Name: string
      Versions: SceneVersionListingItem list }

and SceneVersionListingItem = { Id: EntityId; Version: int }

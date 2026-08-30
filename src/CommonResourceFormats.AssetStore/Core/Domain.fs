namespace CommonResourceFormats.AssetStore.Core.Domain

open System

[<RequireQualifiedAccess>]
type Version =
    | Latest
    | Specific of int

type SceneObject =
    { Id: Guid
      Name: string
      Children: SceneObject ResizeArray
      Components: SceneObjectComponent ResizeArray
      Metadata: Map<string, string> }

and SceneObjectComponent =
    {
        Id: Guid
        Name: string
        Type: string
        /// <summary>
        /// The component data is stored as a json string.
        /// It is up to the consumer to deserialize it.
        /// This is not 100% ideal but is flexible.
        /// </summary>
        JsonData: string
        Assets: SceneObjectComponentAsset ResizeArray
        Metadata: Map<string, string>
    }

and SceneObjectComponentAsset =
    { Id: Guid
      Metadata: Map<string, string>
      Asset: Asset }


and Asset =
    { Id: Guid
      AssetId: Guid
      AssetType: string
      Version: int
      IsPrototype: bool
      Metadata: Map<string, string>
      VersionMetadata: Map<string, string>
      Resources: AssetResource ResizeArray }


and AssetResource =
    { Id: Guid
      Name: string
      Description: string
      FileType: string
      Hash: string
      Metadata: Map<string, string> }

type Scene = { Objects: SceneObject ResizeArray }

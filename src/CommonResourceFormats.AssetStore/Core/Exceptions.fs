namespace CommonResourceFormats.AssetStore.Core

open System

module Exceptions =
    
    type IllegalDatabaseState() =
        inherit Exception("Entity missing, this represents a illegal database state")
        
        
        member _.Test() = ()


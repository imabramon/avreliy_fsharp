module Database

open Npgsql.FSharp

open Domain
open Utils
open Result

let CHATS_NAME = "chats"
let P_CHAT_ID = "chat_Id"
let P_SKIN_NAME = "skin_name"

let getConnectionString () =
    result {
        let! host = getEnvVariable "DB_HOST"
        let! name = getEnvVariable "DB_NAME"
        let! user = getEnvVariable "DB_USER"
        let! password = getEnvVariable "DB_PASSWORD"

        return $"Host={host};Database={name};Username={user};Password={password}"
    }

let connect str =
    (fun () -> str |> Sql.connect) |> toResult

let createDatabase connection =
    (fun _ ->
        connection
        |> Sql.query
            $"""
            CREATE TABLE IF NOT EXISTS {CHATS_NAME} (
                {P_CHAT_ID} BIGINT PRIMARY KEY,
                {P_SKIN_NAME} TEXT NULL
            )
            """
        |> Sql.executeNonQuery)
    |> toResult


let addOrUpdateChat connection chatId skinName =
    (fun _ ->
        connection
        |> Sql.query
            $"""
            INSERT INTO {CHATS_NAME} ({P_CHAT_ID}, {P_SKIN_NAME}) 
            VALUES (@chatId, @skinName)
            ON CONFLICT ({P_CHAT_ID}) 
            DO UPDATE SET {P_SKIN_NAME} = EXCLUDED.{P_SKIN_NAME}
            """
        |> Sql.parameters [ "@chatId", Sql.int64 chatId; "@skinName", Sql.stringOrNone skinName ]
        |> Sql.executeNonQuery
        |> ignore)
    |> toResult

let getChatInfo connection chatId =
    (fun _ ->
        connection
        |> Sql.query
            $"""
            SELECT {P_CHAT_ID}, {P_SKIN_NAME} 
            FROM {CHATS_NAME} 
            WHERE {P_CHAT_ID} = @chatId
            """
        |> Sql.parameters [ "@chatId", Sql.int64 chatId ]
        |> Sql.executeRow (fun read -> read.textOrNone P_SKIN_NAME))
    |> toResult

let initDb () =
    result {
        let! connString = getConnectionString ()
        let! connection = connect connString

        do! createDatabase connection |> ignoreResult

        return
            { add = addOrUpdateChat connection
              get = getChatInfo connection }
    }

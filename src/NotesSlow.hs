{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Environment (getArgs)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.FromField as S
import qualified Database.SQLite.Simple.ToField as S

import Graphics.QML

import Paths_hsqml_demo_notes

newtype Note = Note {noteId :: Int} deriving (Eq, Ord, Typeable)

createTable :: S.Connection -> IO ()
createTable conn =
    S.execute_ conn . S.Query . T.pack $
        "CREATE TABLE IF NOT EXISTS notes (" ++
            "id INTEGER PRIMARY KEY AUTOINCREMENT, " ++
            "x INTEGER, y INTEGER, reverse BOOLEAN, front TEXT, back TEXT)"

selectNotes :: S.Connection -> a -> (a -> Note -> IO a) -> IO a
selectNotes conn zero func =
    let query = S.Query $ T.pack "SELECT id FROM notes ORDER BY id DESC"
    in S.fold_ conn query zero (\acc (S.Only i) -> func acc $ Note i)

insertNote :: S.Connection -> Int -> Int -> Text -> IO ()
insertNote conn x y front =
    S.execute conn (S.Query $ T.pack
        "INSERT INTO notes (x,y,reverse,front,back) VALUES (?, ?, 0, ?, '')")
        (x, y, front)

deleteNote :: S.Connection -> Note -> IO ()
deleteNote conn =
    let query = S.Query $ T.pack "DELETE FROM notes WHERE id = ?"
    in S.execute conn query . S.Only . noteId

readNoteAttrib :: (S.FromField a) =>
    S.Connection -> String -> ObjRef Note -> IO a
readNoteAttrib conn attrib note = do
    let query = S.Query . T.pack $
            "SELECT " ++ attrib ++ " FROM notes WHERE id = ?"
    [S.Only value] <- S.query conn query (S.Only . noteId $ fromObjRef note)
    return value

writeNoteAttrib :: (S.ToField a) =>
    S.Connection -> String -> SignalKey (IO ()) -> ObjRef Note -> a -> IO ()
writeNoteAttrib conn attrib changeKey note value = do
    let query = S.Query . T.pack $
            "UPDATE notes SET " ++ attrib ++ " = ? WHERE id = ?"
    S.execute conn query (value, noteId $ fromObjRef note)
    fireSignal changeKey note

createContext :: S.Connection -> IO (ObjRef ())
createContext conn = do
    changeKey <- newSignalKey
    noteClass <- newClass [
        defPropertySigRW "x" changeKey
            (readNoteAttrib conn "x" :: ObjRef Note -> IO Int)
            (writeNoteAttrib conn "x" changeKey),
        defPropertySigRW "y" changeKey
            (readNoteAttrib conn "y" :: ObjRef Note -> IO Int)
            (writeNoteAttrib conn "y" changeKey),
        defPropertySigRW "reverse" changeKey
            (readNoteAttrib conn "reverse" :: ObjRef Note -> IO Bool)
            (writeNoteAttrib conn "reverse" changeKey),
        defPropertySigRW "front" changeKey
            (readNoteAttrib conn "front" :: ObjRef Note -> IO Text)
            (writeNoteAttrib conn "front" changeKey),
        defPropertySigRW "back" changeKey
            (readNoteAttrib conn "back" :: ObjRef Note -> IO Text)
            (writeNoteAttrib conn "back" changeKey)]
    notePool <- newFactoryPool (newObject noteClass)
    rootClass <- newClass [
        defPropertySigRO' "notes" changeKey (\_ ->
            selectNotes conn [] (\objs note -> do
                object <- getPoolObject notePool note
                return $ object:objs)),
        defMethod' "insertNote" (\this x y front -> do
            insertNote conn x y front
            fireSignal changeKey this),
        defMethod' "deleteNote" (\this note -> do
            deleteNote conn $ fromObjRef note
            fireSignal changeKey this)]
    newObject rootClass ()

main :: IO ()
main = S.withConnection "notes.db" $ \conn -> do
    createTable conn
    ctx <- createContext conn
    args <- getArgs
    path <- case args of
        ("local":p:_) -> return $ p ++ ".qml"
        (p:_)         -> getDataFileName $ p ++ ".qml"
        _             -> getDataFileName "notes.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument path,
        contextObject = Just $ anyObjRef ctx}

{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}
module Main where

import System.Environment (getArgs)

import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Control.Applicative
import Control.Concurrent

import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.FromField as S
import qualified Database.SQLite.Simple.ToField as S

import Graphics.QML

import Paths_hsqml_demo_notes

newtype Note = Note {noteId :: Int} deriving (Eq, Ord, Typeable)

data DBStatus
    = Current
    | UpdatePending
    | InsertPending
    | DeletePending
    deriving Eq

data NoteData = NoteData {
    noteX :: Int,
    noteY :: Int,
    noteReverse :: Bool,
    noteFront :: Text,
    noteBack :: Text,
    noteStatus :: DBStatus
}

data DBCommand = Load | Pending | Quit

type ModelVar = MVar (Map Note NoteData)
type CmdVar = MVar DBCommand

type Lens t p = forall f. (Functor f) => (p -> f p) -> t -> f t

view :: Lens t p -> t -> p
view ln = getConst . ln Const

set :: Lens t p -> p -> t -> t
set ln val = runIdentity . ln (const $ Identity val)

alter :: Lens t p -> (p -> p) -> t -> t
alter ln fn = runIdentity . ln (\val -> Identity $ fn val)

noteXLens :: Lens NoteData Int
noteXLens fn nd = fmap (\x' -> nd {noteX = x'}) . fn $ noteX nd

noteYLens :: Lens NoteData Int
noteYLens fn nd = fmap (\y' -> nd {noteY = y'}) . fn $ noteY nd

noteReverseLens :: Lens NoteData Bool
noteReverseLens fn nd =
    fmap (\rev' -> nd {noteReverse = rev'}) . fn $ noteReverse nd

noteFrontLens :: Lens NoteData Text
noteFrontLens fn nd =
    fmap (\front' -> nd {noteFront = front'}) . fn $ noteFront nd

noteBackLens :: Lens NoteData Text
noteBackLens fn nd =
    fmap (\back' -> nd {noteBack = back'}) . fn $ noteBack nd

noteStatusLens :: Lens NoteData DBStatus
noteStatusLens fn nd =
    fmap (\status' -> nd {noteStatus = status'}) . fn $ noteStatus nd

selectNotes :: ModelVar -> IO [Note]
selectNotes modelVar = withMVar modelVar (
    return . Map.keys . Map.filter ((/= DeletePending) . noteStatus))

insertNote :: ModelVar -> Int -> Int -> Text -> IO ()
insertNote modelVar x y front = modifyMVar_ modelVar (\notes ->
    let (Note lastId) = fromMaybe (Note 0) . fmap fst . fmap fst $
            Map.maxViewWithKey notes
        newNote       = Note (lastId+1)
    in return $ Map.insert newNote (
        NoteData x y False front T.empty InsertPending) notes)

deleteNote :: ModelVar -> Note -> IO ()
deleteNote modelVar note = modifyMVar_ modelVar (
    return . Map.adjust (set noteStatusLens DeletePending) note)

readNoteAttrib :: ModelVar -> Lens NoteData a -> ObjRef Note -> IO a
readNoteAttrib modelVar attrib note = withMVar modelVar (
    return . view attrib .
    fromMaybe (NoteData 0 0 False T.empty T.empty Current) .
    Map.lookup (fromObjRef note))

writeNoteAttrib ::
    ModelVar -> CmdVar -> Lens NoteData a -> SignalKey (IO ()) ->
    ObjRef Note -> a -> IO ()
writeNoteAttrib modelVar cmdVar attrib changeKey note value = do
    modifyMVar_ modelVar (return . Map.adjust (
        set attrib value . alter noteStatusLens updateState) (fromObjRef note))
    tryPutMVar cmdVar Pending
    fireSignal changeKey note
    where updateState InsertPending = InsertPending
          updateState DeletePending = DeletePending
          updateState _ = UpdatePending

createContext :: ModelVar -> CmdVar -> SignalKey (IO ()) -> IO (ObjRef ())
createContext modelVar cmdVar changeKey = do
    noteClass <- newClass [
        defPropertySigRW "x" changeKey
            (readNoteAttrib modelVar noteXLens)
            (writeNoteAttrib modelVar cmdVar noteXLens changeKey),
        defPropertySigRW "y" changeKey
            (readNoteAttrib modelVar noteYLens)
            (writeNoteAttrib modelVar cmdVar noteYLens changeKey),
        defPropertySigRW "reverse" changeKey
            (readNoteAttrib modelVar noteReverseLens)
            (writeNoteAttrib modelVar cmdVar noteReverseLens changeKey),
        defPropertySigRW "front" changeKey
            (readNoteAttrib modelVar noteFrontLens)
            (writeNoteAttrib modelVar cmdVar noteFrontLens changeKey),
        defPropertySigRW "back" changeKey
            (readNoteAttrib modelVar noteBackLens)
            (writeNoteAttrib modelVar cmdVar noteBackLens changeKey)]
    notePool <- newFactoryPool (newObject noteClass)
    rootClass <- newClass [
        defPropertySigRO' "notes" changeKey (\_ -> do
            notes <- selectNotes modelVar
            mapM (getPoolObject notePool) notes),
        defMethod' "insertNote" (\this x y front -> do
            insertNote modelVar x y front
            tryPutMVar cmdVar Pending
            fireSignal changeKey this),
        defMethod' "deleteNote" (\this note -> do
            deleteNote modelVar $ fromObjRef note
            tryPutMVar cmdVar Pending
            fireSignal changeKey this)]
    newObject rootClass ()

createTableDb :: S.Connection -> IO ()
createTableDb conn =
    S.execute_ conn . S.Query . T.pack $
        "CREATE TABLE IF NOT EXISTS notes (" ++
            "id INTEGER PRIMARY KEY AUTOINCREMENT, " ++
            "x INTEGER, y INTEGER, reverse BOOLEAN, front TEXT, back TEXT)"

selectNotesDb :: S.Connection -> a -> (a -> Note -> NoteData -> IO a) -> IO a
selectNotesDb conn zero func =
    let query = S.Query $ T.pack
            "SELECT id,x,y,reverse,front,back FROM notes ORDER BY id DESC"
    in S.fold_ conn query zero (\acc (i,x,y,rev,front,back) ->
        func acc (Note i) (NoteData x y rev front back Current))

insertNoteDb :: S.Connection -> Note -> NoteData -> IO ()
insertNoteDb conn note dat =
    S.execute conn (S.Query $ T.pack
        "INSERT INTO notes (id,x,y,reverse,front,back) VALUES (?,?,?,?,?,?)")
        (noteId note, noteX dat, noteY dat,
            noteReverse dat, noteFront dat, noteBack dat)

deleteNoteDb :: S.Connection -> Note -> IO ()
deleteNoteDb conn =
    let query = S.Query $ T.pack "DELETE FROM notes WHERE id = ?"
    in S.execute conn query . S.Only . noteId

updateNoteDb :: S.Connection -> Note -> NoteData -> IO ()
updateNoteDb conn note dat =
    S.execute conn (S.Query $ T.pack 
        "UPDATE notes SET x=?, y=?, reverse=?, front=?, back=? WHERE id=?")
        (noteX dat, noteY dat,
            noteReverse dat, noteFront dat, noteBack dat, noteId note)

dbThread ::
    S.Connection -> ModelVar -> CmdVar -> MVar () -> IO () -> IO ()
dbThread conn modelVar cmdVar finVar changeCb = do 
    cmd <- takeMVar cmdVar
    case cmd of
        Load -> do
            createTableDb conn
            selectNotesDb conn () $ \_ note dat -> do
                modifyMVar_ modelVar (return . Map.insert note dat)
                changeCb
            dbThread conn modelVar cmdVar finVar changeCb
        Pending -> do
            pending <- modifyMVar modelVar (\notes ->
                let pending = Map.toList $
                        Map.filter ((/= Current) . noteStatus) notes
                    notes' = Map.mapMaybe (\dat -> case noteStatus dat of
                        DeletePending -> Nothing
                        _             -> Just $
                            set noteStatusLens Current dat) notes
                in return (notes', pending))
            mapM_ (\(note,dat) -> case noteStatus dat of
                DeletePending -> deleteNoteDb conn note
                InsertPending -> insertNoteDb conn note dat
                UpdatePending -> updateNoteDb conn note dat) pending
            dbThread conn modelVar cmdVar finVar changeCb
        Quit -> putMVar finVar ()

main :: IO ()
main = do
    modelVar <- newMVar Map.empty
    cmdVar <- newMVar Load
    finVar <- newEmptyMVar
    changeKey <- newSignalKey
    ctx <- createContext modelVar cmdVar changeKey
    forkOS $ S.withConnection "notes.db" $ \conn ->
        dbThread conn modelVar cmdVar finVar (fireSignal changeKey ctx)
    args <- getArgs
    path <- case args of
        ("local":p:_) -> return $ p ++ ".qml"
        (p:_)         -> getDataFileName $ p ++ ".qml"
        _             -> getDataFileName "notes.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument path,
        contextObject = Just $ anyObjRef ctx}
    tryPutMVar cmdVar Quit
    takeMVar finVar

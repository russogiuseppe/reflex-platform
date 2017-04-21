getDeleteButton :: String -> IO (Maybe Element)
getDeleteButton iden = do
      document <- jsg "document"
      createElement <- js1 "createElement"
      td <- document ^. createElement "TD"
      bt <- document ^. createElement "BUTTON"
      let bt ^. getTextContent = "Delete"

      void $
        Ev.on bt E.click $ do
          deleteQuote iden
          tr <- document ^. getElementById iden
          tr ^. getParentNode ^. removeChild tr
      return()

      {-    callBack <- jsg "callbackToHaskell" <# fun (\f this -> do --come si gestiscono più callback in fila??
                document <- jsg "document"
                getElementById <- js1 "getElementById"
                tr <- document ^. getElementById
                tr ^. getParentNode ^. removeChild tr) -}
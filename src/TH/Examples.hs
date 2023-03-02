{-# LANGUAGE TemplateHaskell #-}

module TH.Examples where
import Language.Haskell.TH

-- Сгенерировать Tuple с n элементами
makeNTuple :: Int -> Q Exp
makeNTuple n =
    do varName <- newName "x"
       -- Фактически генерирует код подобный следующей лямбде следующей лямбде: x -> (x, x, x...)
       return $ LamE [VarP varName]
                     (TupE $ replicate n $ (VarE varName))

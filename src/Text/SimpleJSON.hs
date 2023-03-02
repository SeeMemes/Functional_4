{-# LANGUAGE FlexibleInstances #-}

module Text.SimpleJSON(
    JSValue (..),
    JSON (..),
    Result (..),
    encode,
    decode,
    JSString,
    toJSString,
    fromJSString,
    JSObject,
    toJSObject,
    fromJSObject,
    showJSValue,
    makeObj,
    toJSON, toJSONGeneric,
    fromJSON, fromJSONGeneric,
    decodeJSON, encodeJSON
) where

import Text.SimpleJSON.String
    ( StringRepresentable(..), parseJSValue, showJSValue )

import Text.SimpleJSON.Generic
import Text.SimpleJSON.Result
import Text.SimpleJSON.Types
import Text.ParserCombinators.Parsec ( parse )

-- Декодирование JSON файла в конструкторы может вызывать ошибку при неправильном синтаксе, поэтому
-- Right - функция считывания
-- Left - Вывод ошибки
decode :: (StringRepresentable st, JSON a) => st -> Result a
decode s = case parse parseJSValue "" (toString s) of
    Right a -> readJSON a
    Left err -> Error $ show err

-- Вывод JSON файла
encode :: (JSON a) => a -> String
encode = flip showJSValue [] . showJSON

-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : provisional
Portability : portable
-}

{-# LANGUAGE RecordWildCards #-}

module Language.Bond.Syntax.Swagger
    where

import Data.Maybe
import Data.HashMap.Strict(HashMap, fromList)
import Language.Bond.Syntax.Types

data SwaggerBond =
    SwaggerBond
        { info :: HashMap String String
        , definitions :: HashMap String SwagDeclaration
        , paths :: HashMap String (HashMap String SwagPost)
        }
    deriving (Eq, Show)

data SwagDeclaration =
    SwagStruct
        { dtype :: String
        , drequired :: [String]
        , dproperties :: HashMap String SwagProperty
        }
    |
    SwagEnum
        { dtype :: String
        }
    deriving (Eq, Show)

data SwagProperty =
    SwagProperty
        { ptype :: String
        }
    deriving (Eq, Show)

data SwagResponse =
    SwagResponse
        { description :: String
        , schema :: HashMap String String
        }
    deriving (Eq, Show)

data SwagParameter =
    SwagParameter
        { pname :: String
        , pin :: String
        , prequired :: Bool
        , pschema :: HashMap String String
        }
    deriving (Eq, Show)

data SwagPost =
    SwagPost
        { operationId :: String
        , responses :: HashMap String SwagResponse
        , parameters :: [SwagParameter]
        }
    deriving (Eq, Show)

translateToSwagger :: String -> Bond -> SwaggerBond
translateToSwagger name Bond{..} = SwaggerBond { definitions = fromList $ catMaybes $ map convertStruct bondDeclarations
                         , paths = fromList $ concat $ map convertService bondDeclarations
                         , info = fromList [("title", name ++ ".bond"), ("version", "version not set")]
                         }

convertStruct :: Declaration -> Maybe (String, SwagDeclaration)
convertStruct Struct{..} = Just (declName, SwagStruct { dtype = "object"
                                                   , drequired = getRequiredFields structFields
                                                   , dproperties = fromList $ map convertFields (structFields)
                                                   })
convertStruct _ = Nothing

convertService :: Declaration -> [(String, HashMap String SwagPost)]
convertService Service{..} = catMaybes $ map convertMethod serviceMethods
convertService _ = []

convertMethod :: Method -> Maybe (String, HashMap String SwagPost)
convertMethod Function{..} = Just ("/v1/" ++ methodName, fromList [("post", SwagPost {
                                                 operationId = methodName
                                               , responses = fromList [("200", typeToResponse methodResult)]
                                               , parameters = [typeToParam methodInput]
                                               })])
convertMethod Event{..} = Just ("/v1/" ++ methodName, fromList [("post", SwagPost {
                                                 operationId = methodName
                                               , responses = fromList [("200", typeToResponse Nothing)]
                                               , parameters = [typeToParam methodInput]
                                               })])

typeToParam :: Maybe Type -> SwagParameter
typeToParam (Just (BT_UserDefined Struct{..} _)) = SwagParameter {
                                                      pname = "body"
                                                    , pin = "body"
                                                    , prequired = True
                                                    , pschema = fromList [("$ref", "#/definitions/" ++ declName)]
                                                    }
typeToParam (Just (BT_UserDefined Alias{..} _)) = typeToParam $ Just aliasType
typeToParam Nothing = SwagParameter { pschema = fromList [("$ref", "void")]
                                   , pname = ""
                                   , pin = ""
                                   , prequired = False
                                   }
typeToParam _ = SwagParameter { pschema = fromList []
                             , pname = "invalid"
                             , pin = "invalid"
                             , prequired = False
                             }

typeToResponse :: Maybe Type -> SwagResponse
typeToResponse (Just (BT_UserDefined Struct{..} _)) = SwagResponse { schema = fromList [("$ref", "#/definitions/" ++ declName)]
                                                             , description = ""
                                                             }
typeToResponse Nothing = SwagResponse { schema = fromList [("$ref", "void")]
                                , description = "void"
                                }
typeToResponse _ = SwagResponse { schema = fromList []
                          , description = "invalid"
                          }

convertFields :: Field -> (String, SwagProperty)
convertFields Field{..} = (fieldName, SwagProperty { ptype = convertType fieldType })

convertType :: Type -> String
convertType BT_Int8 = "number"
convertType BT_Int16 = "number"
convertType BT_Int32 = "number"
convertType BT_Int64 = "number"
convertType BT_UInt8 = "number"
convertType BT_UInt16 = "number"
convertType BT_UInt32 = "number"
convertType BT_UInt64 = "number"
convertType BT_Float = "number"
convertType BT_Double = "number"
convertType BT_Bool = "bool"
convertType BT_String = "string"
convertType BT_WString = "string"
convertType BT_MetaName = "string"
convertType BT_Blob = "array"
convertType (BT_Maybe _) = "array"
convertType (BT_List _) = "array"
convertType (BT_Vector _) = "array"
convertType (BT_Nullable _) = "array"
convertType (BT_Set _) = "array"
convertType (BT_Map _ _) = "object"
convertType (BT_UserDefined Struct{..} _) = "#/definitions" ++ declName
convertType (BT_UserDefined Alias{..} _) = convertType aliasType
convertType _ = "invalid"

getRequiredFields :: [Field] -> [String]
getRequiredFields fields = map fieldName (filter requiredFilter fields)

requiredFilter :: Field -> Bool
requiredFilter Field{..} = fieldModifier == Required


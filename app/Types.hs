{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

--extensions: TypeOperators allows us to use operators like (:>) as a type constructor.
    --this works well with DataKinds extension, which allows us ot use stirng constants (type-level literals) in a type declaration.
module Types where
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

--extensions: TypeOperators allows us to use operators like (:>) as a type constructor.
    --this works well with DataKinds extension, which allows us ot use stirng constants (type-level literals) in a type declaration.
module Types where

import Data.Text
import Servant.API

--will be the route to get the percentages from factors. ie: (field_goal_percentage,70%) means whoever had higher fierld goal percentage won 70% of the games.
type PercentageAPI = "percentage" :> Get '[JSON] [Percentages]

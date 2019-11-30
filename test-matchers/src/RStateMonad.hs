{-
Copyright 2018-2019 Google LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
{- |
Module:       RSMonad
Description:  Minimal implementation of the (lazy) State + Reader monad to avoid
              a direct dependency on transformers.
Copyright:    2018-2019 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

-}

module RStateMonad
  ( RState
  , runRState
  , get
  , put
  , modify
  , ask
  , asks
  ) where

newtype RState r s a = RState { runRState :: r -> s -> (a, s) }

instance Functor (RState r s) where
  fmap f (RState run) = RState $ \r s -> let (a, s') = run r s in (f a, s')

instance Applicative (RState r s) where
  pure x = RState $ \_ s -> (x, s)
  (RState runF) <*> (RState runX) = RState $ \r s ->
                                               let (f, s') = runF r s
                                                   (x, s'') = runX r s'
                                               in (f x, s'')

instance Monad (RState r s) where
  return = pure
  (RState run) >>= f = RState $ \r s -> let (x, s') = run r s
                                        in runRState (f x) r s'

get :: RState r s s
get = RState $ \_ s -> (s, s)

modify :: (s -> s) -> RState r s s
modify f = RState $ \_ s -> (s, f s)

put :: s -> RState r s ()
put s = RState $ \_ _ -> ((), s)

ask :: RState r s r
ask = asks id

asks :: (r -> r') -> RState r s r'
asks view = RState $ \r s -> (view r, s)

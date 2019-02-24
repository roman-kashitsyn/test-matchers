import Gauge.Main

import Control.DeepSeq (NFData(..))
import Control.Applicative ((<|>))
import qualified Data.Text as T

import Machines (oneOfStr, str, runMachineText, Range(..), State(..), step)

instance NFData Range where
  rnf (Range x y) = ()

instance (NFData a) => NFData (State a) where
  rnf (Done x) = rnf x
  rnf (Cont c) = rnf (step c)
  rnf Failed = ()

-- List of C++ keywords
kw :: [String]
kw = [ "alignas"
     , "alignof"
     , "and"
     , "and_eq"
     , "asm"
     , "atomic_cancel"
     , "atomic_commit"
     , "atomic_noexcept"
     , "auto"
     , "bitand"
     , "bitor"
     , "bool"
     , "break"
     , "case"
     , "catch"
     , "char"
     , "char8_t"
     , "char16_t"
     , "char32_t"
     , "class"
     , "compl"
     , "concept"
     , "const"
     , "consteval"
     , "constexpr"
     , "const_cast"
     , "continue"
     , "co_await"
     , "co_return"
     , "co_yield"
     , "decltype"
     , "default"
     , "delete"
     , "do"
     , "double"
     , "dynamic_cast"
     , "else"
     , "enum"
     , "explicit"
     , "export"
     , "extern"
     , "false"
     , "float"
     , "for"
     , "friend"
     , "goto"
     , "if"
     , "import"
     , "inline"
     , "int"
     , "long"
     , "module"
     , "mutable"
     , "namespace"
     , "new"
     , "noexcept"
     , "not"
     , "not_eq"
     , "nullptr"
     , "operator"
     , "or"
     , "or_eq"
     , "private"
     , "protected"
     , "public"
     , "reflexpr"
     , "register"
     , "reinterpret_cast"
     , "requires"
     , "return"
     , "short"
     , "signed"
     , "sizeof"
     , "static"
     , "static_assert"
     , "static_cast"
     , "struct"
     , "switch"
     , "synchronized"
     , "template"
     , "this"
     , "thread_local"
     , "throw"
     , "true"
     , "try"
     , "typedef"
     , "typeid"
     , "typename"
     , "union"
     , "unsigned"
     , "using"
     , "virtual"
     , "void"
     , "volatile"
     , "wchar_t"
     , "while"
     , "xor"
     , "xor_eq"
    ]

kwAltMachine = foldr ((<|>) . str) mempty kw
kwTrieMachine = oneOfStr kw

runOnKw m = map (runMachineText m . T.pack) kw

main = defaultMain [ bgroup "KeywordMatch"
                     [ bench "Alternative" $ nf runOnKw kwAltMachine
                     , bench "Trie" $ nf runOnKw kwTrieMachine
                     ]
                   ]

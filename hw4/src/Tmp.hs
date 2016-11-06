import Monads

data State s a = State { runState :: s -> (s, a)}

(State f) >>= g = State $ \s ->
  let (nS, v) = f s in
    case g v of
      State l -> l nS

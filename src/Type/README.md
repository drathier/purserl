Test driver:
1. `make ghci`
2. 
```
λ Type.Solve.run CTrue
λ Type.Solve.run (CEqual A.zero E.List UnitN (E.NoExpectation EmptyRecordN))
Stopped in <exception thrown>, <unknown>
_exception :: e = _
[<unknown>] λ seq _exception ()
()
[<unknown>] λ _exception 
([],(UFPoint<1> Descriptor {_content = Structure Unit1, _rank = 1, _mark = Mark 2, _copy = Nothing}),(UFPoint<1> Descriptor {_content = Structure EmptyRecord1, _rank = 1, _mark = Mark 2, _copy = Nothing}),Descriptor {_content = Error, _rank = 0, _mark = Mark 2, _copy = Nothing})
CallStack (from HasCallStack):
  error, called at /Users/drathier/drathier/purserl/purescript/src/Type/Unify.hs:37:9 in main:Type.Unify
[<unknown>] λ 
```

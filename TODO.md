Fix shift/reduce conflict

---

Sum types:
```
type result:[$a, $b] = enum(Ok $a, Error $b);

func result_bind:[$a, $b](v result:[$a, $b], f func (v $a) result:[$a, $b]) result:[$a, $b] {
  let a = v.Ok {
    return f(a);
  }
  return v;
}
```

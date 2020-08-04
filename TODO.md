`nil` literal:
```
val a *i8 = *i8(nil); // would require a new nil_type?
val b mut*i8 = mut*i8(nil);
// ideally, remove the cast altogether
// any ptr can be assigned/compared to nil_type
```

---

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

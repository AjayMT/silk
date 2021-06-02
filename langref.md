---
toc: true
include-before: |
    <h1 style="color:#f2983f;font-size:40pt;">〈Silk〉</h1>
    <p style="padding:10px;border-top:5px solid #f2983f;background-color:#eee;font-size:13pt;">
    [Overview](./index.html)
    <span style="padding-left:20px"></span>
    [Source Code](https://github.com/AjayMT/silk)
    <span style="padding-left:20px"></span>
    [Bug Tracker](https://github.com/AjayMT/silk/issues)
    <span style="padding-left:20px"></span>
    [Language Reference](./langref.html)
    </p>
    <br />
    <details><summary>Table of Contents</summary>
---

</details>

## Primitive Types
| Primitive Type | Equivalent C Type | Description |
| -------------- | ----------------- | ----------- |
| `i8`           | `int8_t`          | 8-bit signed integer |
| `i16`          | `int16_t`         | 16-bit signed integer |
| `i32`          | `int32_t`         | 32-bit signed integer |
| `i64`          | `int64_t`         | 64-bit signed integer |
| `u8`           | `uint8_t`         | 8-bit unsigned integer |
| `u16`          | `uint16_t`        | 16-bit unsigned integer |
| `u32`          | `uint32_t`        | 32-bit unsigned integer |
| `u64`          | `uint64_t`        | 64-bit unsigned integer |
| `f32`          | `float`           | 32-bit floating point value |
| `f64`          | `double`          | 64-bit floating point value |
| `bool`         | `bool`            | Boolean value |
| `void`         | `void`            | Void type |

## Derived Types
Given arbitrary types `a`, `b`, `c` and non-negative integer `n`:

| Derived Type | Equivalent C Type | Description |
| ------------ | ----------------- | ----------- |
| `*a`         | `const a*`        | Immutable pointer to `a` |
| `mut*a`      | `a*`              | Mutable pointer to `a` |
| `func (a, b) c` | `const c (*foo)(a, b)` | Immutable pointer to a function |
| `[n]a`       | `a[n]`            | Array of `a` of size `n` |
| `struct(a, b)` | N/A             | Unlabeled struct |
| `struct(foo a, bar b)` | `struct { a foo; b bar; }` | Labeled struct |
| `packed(a, b)` | N/A | Packed unlabeled struct |
| `packed(foo a, bar b)` | `struct { a foo; b bar; } __attribute__((packed))` | Packed labeled struct |

## Values
`i32` literals expressed in decimal, hexadecimal, octal and binary:
```
12, 0xff, 0o127, 0b10101
```

`i8`, `i16`, `i64` literals:
```
100c, 128h, 55l
```

`u8`, `u16`, `u32`, `u64` literals:
```
123uc, 85uh, 10u, 15ul
```

`i8` literals expressed as characters:
```
'a', 'b', '\n', '\\'
```

`bool` literals:
```
true, false
```

`*i8` ("string") literals:
```
"hello\n", "asdf", nil
```

The special `nil` literal is the same value as `0` but is of type `*i8`. It is useful for setting immutable pointers to zero since they cannot be cast to or from integers or mutable pointers.

```
var a = nil; // a is of type *i8
var b = *i32(nil); // b is of type *i32
var c = *my_type(nil); // c is of type *my_type
```

### Compound Values
Arrays are constructed with curly braces:
```
val i32array = { 12, 13, 14 };
i32array[0]; i32array[1]; i32array[i32array[1]]; // elements accessed by index
```

Zero-initialized arrays can be constructed with the following syntax:
```
val zero_array = [i32; 3]; // equivalent to { 0, 0, 0 }
```

Packed and non-packed unlabeled structs:
```
val a = (1, 'a', 3ul); // struct with members of type i32, i8 and u64
val b = (: nil, 0xff :) // packed struct with members of type *i8 and i32
a.0; a.1; a.2; // members are accessed by index
```

Packed and non-packed labeled structs:
```
val a = struct(foo i32, bar i8)(12, 'a'); // struct with members foo and bar
val b = packed(asdf u64, qwer i32)(3ul, 12); // packed struct with members asdf and qwer
a.foo; a.bar; // members are accessed by name

type my_type = struct(one i32, two i32);
val c = my_type(1, 2);
c.one; c.two;
```

## Declarations
Variables and constants are declared with `var` and `val` respectively.
The mutability of variables and constants applies to their members/elements as well.
```
var a = 1;
a = 2; // mutable
val b = 0; // immutable

var c = (1, 2, 3);
c.0 = 12; // mutable
val d = (3, 4, 5); // immutable

var b = [2; i32];
b[0] = 1; // mutable
val c = { 0xae, 0xbeef, 0xc0ffee }; // immutable
```

Types can be specified explicity:
```
var a i32 = 0;
val b struct(i8, struct(i32, i32)) = ('a', (0, 1));
```

Function declarations:
```
func f(a i32, b i32) void; // forward declaration

func main(argc i32, argv **i8) i32 {
  f();
  return 0;
}

func f(a i32, b i32) void {}
```

Functions are public by default but can be made private with the `private` keyword:
```
private func secret() void {}
```

Functions declared with `extern` are assumed to be defined in a different object file:
```
extern func printf(s *i8) void;

func main(argc i32, argv **i8) i32 {
  printf("hello\n");
  return 0;
}
```

## Pointers
Pointers can be mutable (read + write) or immutable (read-only).

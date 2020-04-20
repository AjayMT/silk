
# Silk

Silk is a small system programming language that I wrote for fun and clout.
It is designed to be as flexible and simple as C, but more pleasant to write.
To that end, Silk provides

- nicer syntax
- a sophisticated type system (including parametric types!)
- a sense of pride and accomplishment

Silk does not aim to be as safe or fast as C++, Rust, etc. The LLVM backend (you didn't
think I wrote an end-to-end compiler, did you?) does all of the optimization.

As is true of many of my projects, Silk is currently **alpha**-stage software.
Don't expect it to work well (or at all).

<https://github.com/AjayMT/silk>

## Syntax

Silk is heavily inspired by [Go](https://golang.org/) and other modern programming
languages.

```
// this is a comment

extern func printf(s *i8) void;

func main(argc i32, argv **i8) i32 {
  printf("hello, world\n");

  if argc > 1 {
    val arg = @(argv + 1);
    printf(arg);
  }

  return 0;
}
```

Notably,

- types come after names
- `@` is the pointer dereference operator
- `val` and `var` are used to declare values and variables (whose types are inferred
  unless specified explicitly)
- `func`, `return`, `if`, `for` and `while` are familiar keywords
- `extern` declares a symbol defined in a different object file.

Silk also features parametric polymorphism:

```
func add:<$t> (a $t, b $t) $t {
  return a + b;
}

func main() i32 {
  val a = add:<i32>(1, 2);
  val b = add:<i8>('a', 'b');
  return a + i32(b);
}
```

[Here](./syntax.html) is a comprehensive specification of Silk's syntax.

## Semantics and features

Silk is an **imperative** and **statically typed** language.

Though safety is not a primary design goal, Silk's features make writing
unsafe code more difficult than it would otherwise be.

### Mutability

The `val` and `var` keywords declare immutable values and mutable variables
respectively. The immutability of values also applies to aggregate types (i.e
structures and arrays).

```
type my_type = struct(a i32, b i32, c i32);

func main() void {
  val foo = my_type(1, 2, 3);
  foo.a = 0; // produces a compiler error

  // bar is a zero-initialized array of 16 my_types
  val bar = [my_type; 16];
  bar[1].c = 2; // this also produces a compiler error
}
```

Silk has distinct mutable and immutable pointer types. Taking the address of a
value produces an immutable pointer, whereas taking the address of a variable
produces a mutable pointer.

```
type my_type = struct(a i32, b i32, c i32);

func main() void {
  val foo = my_type(1, 2, 3);
  var foo_addr *my_type = &foo;
  // foo_addr is an immutable pointer.
  // it is a var and can therefore be re-assigned
  // but cannot be write-dereferenced
  @foo_addr.a = 0; // produces a compiler error

  var bar = my_type(4, 5, 6);
  val bar_addr mut*my_type = &bar;
  // bar_addr is a mutable pointer, which cannot be re-assigned
  // but can be write-dereferenced
  @bar_addr.b = 7;
}
```

Mutable and immutable pointers can never be cast to/from each other.
A mutable pointer can be *promoted* to an immutable pointer when passed as an argument
to a function -- the function cannot mutate the object unless it also has a mutable
pointer or direct access to the object.

```
// p is an immutable *i32
func f(p *i32) void {
  @p = 12; // this produces a compiler error
}

func main() void {
  var a = 12;
  val a_addr = &a;
  // a_addr is a mut*i32 that is promoted to an immutable *i32
  f(a_addr);
  // but it is still a mutable pointer in this function
  @a_addr = 13;
}
```

Additionally, only mutable pointers can be cast to/from integers.
Silk's mutability rules guarantee that:

- mutable pointers never point to immutable objects
- immutable pointers can only point to mutable objects when the objects
  can be mutated in the same function
  - (or when the objects are mutated elsewhere concurrently, but that's a
    big can of worms)

### Aggregate types

Silk's aggregate types behave similarly to their C counterparts, with a few key
differences.

**Arrays** do not decay to pointers as in C. They generally behave as a single
entity of data, much like structures.

```
func g(arr *i32) void {}

func f(arr [3]i32) void {}

func main() void {
  val a = { 1, 2, 3 };
  g(a); // this produces a compiler error
  f(a); // this does not
  g(&a[0]); // this does not

  val b = [i32; 3];
  // b is a zero-initialized array of 3 i32s
  f(b);
}
```

Silk does not support variable-length arrays. The size of every array must be known
at compile time and is part of its type.

**Structures** are either *labeled* (every member has a name) or *unlabeled*.
Unlabeled structures can be constructed with *struct literals*:
```
func main() void {
  var a = (1, 2, 3);
  // (1, 2, 3) is a struct literal
  // a is of type struct(i32, i32, i32)

  var b = a.0; // its members are accessed by 'index'
  var c = a.1;
  var d = a.2;
}
```

Labeled structs can be defined as newtypes or constructed ad-hoc:
```
type my_type = struct(num i32, chr i8, flag bool);

func main() void {
  var beans = my_type(1, 'B', true);
  var b = beans.num;
  // beans has members 'num', 'chr' and 'flag'

  var cake = struct(asdf i32, qwer u32)(12, 12u);
  var x = cake.qwer;
  // cake has members 'asdf' and 'qwer'
}
```

Structures are aligned by default, but can be *packed*.
```
type packed_type = packed(a i32, b i32, c i32);

func main() void {
  val a = (: 1, 2, 3 :);
  // a is a packed unlabeled struct

  val b = packed_type(4, 5, 6);
  // b is a packed labeled struct
}
```

### User-defined types and parametric polymorphism

Types are defined with the `type` keyword:

```
type int = i32;

func main() int {
  val a int = 12;
  val b i32 = a;
  return b;
}
```

Notice that the `int` type defined in the previous example is not a newtype; it is
an alias for `i32`.

Unlike other types, structs are unique. To define a newtype, simply use a
one-member struct:
```
type my_i32 = struct(i32);
type other_i32 = struct(i32);

func main() my_i32 {
  val a = 12;
  val b = my_i32(a);
  val c = other_i32(b.0);
  // a, b, c are of distinct types
}
```

Forward declaring a type allows it to be defined recursively.
A singly linked list type could be defined as follows:

```
type list;
type list = struct(value i32, next mut*list);

func make_list_node(value i32) {
  return list(value, mut*list (0));
}

func main() i32 {
  var head = make_list_node(1);
  var tail = make_list_node(2);
  head.next = &tail;

  return @(head.next).value;
}
```

Silk's types and functions can also accept type parameters:

```
// binary tree with key and value
type bintree:<$key_type, $value_type>;
type bintree:<$kt, $vt> = struct(
  key $kt, value $vt,
  left mut*bintree:<$kt, $vt>,
  right mut*bintree:<$kt, $vt>
);

func make_bintree_node:<$kt, $vt>(key $kt, value $vt) bintree:<$kt, $vt> {
  val null = mut*bintree:<$kt, $vt> (0);
  return bintree:<$kt, $vt>(key, value, null, null);
}

func main() i32 {
  var root = make_bintree_node:<*i8, i32>("hello", 1);
  var child = make_bintree_node:<*i8, i32>("world", 2);
  root.right = &child;
  return @(root.right).value;
}
```

(This syntax is exceptionally ugly, I'm working on it.)

### Other miscellaneous things

Silk will never implicitly cast or coerce types. This is both a design choice and a
product of my own laziness.

```
func main() i32 {
  // a is of type u64 ('u': unsigned, 'l': long i.e 64-bit)
  val a = 125ul;

  // this produces a compiler error since
  // 12 is an i32
  val b = a + 12;

  // these work as expected
  val c = a + 12ul;
  val d = a + u64(12);
}
```

If every member in a struct literal is an lvalue, the struct literal can be an
lvalue. This enables some cool 'group' assignment stuff.

```
func main() i32 {
  var a = 1;
  var b = 2;
  var c = 3;
  val c_ptr = &c;

  // all of the following statements assign
  // a, b, c to 4, 5, 6 respectively
  (a, b, c) = (4, 5, 6);
  (a, (b, c)) = (4, (5, 6));
  (a, b, @c_ptr) = (4, 5, 6);

  // this will swap a and b
  (a, b) = (b, a);

  return a;
}
```

## Build

In order to build Silk, you will need:

- [ocaml](https://ocaml.org/) toolchain
- [dune](https://dune.build/) build tool
- [menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator

Then simply acquire the [source code](https://github.com/AjayMT/silk)
and `./build.sh`.

This will produce a `silk.exe` executable in the `_build/default/src/` directory.

## Usage

In order to use Silk, you will need the [LLVM](http://llvm.org/) toolchain.

Since I have not written a CLI yet, the `silk.exe` executable reads Silk from
stdin and writes LLVM to stdout. LLVM can be compiled to native object files
with the `llc` tool.

For example:

```
$ cat hello.silk
extern func printf(s *i8) void;

func main() i32 {
  printf("hello, world\n");
  return 0;
}
$ silk < hello.silk > hello.llvm
$ llc -filetype=obj hello.llvm -o hello.o
$ ld -o hello hello.o -lc
$ ./hello
hello, world
```

The LLVM output can be compiled directly without writing it to a file:

```
$ silk < hello.silk | llc -filetype=obj -o hello.o -
```

Silk does not include a preprocessor or package/module system of any kind. Your
favorite C compiler can probably produce preprocessor output:

```
$ cc -E -x c hello.silk > hello.silk.out
$ # Compile hello.silk.out as described above
```

## TODOs / Roadmap

- Un-spaghetti the code and produce better error messages
  - Possibly by rewriting the whole thing
- Better type parameter syntax
- Cool optimization stuff
- Make a real CLI
- More docs!

## License

Silk is distributed under the terms of the MIT License.

---

Silk is one of my [many projects](http://ajaymt.github.io/). If you find this
(or any of my work) interesting, please [contact me](mailto:ajaymt2@illinois.edu)!
I am [available for hire](https://www.linkedin.com/in/ajay-tatachar-980556193/).

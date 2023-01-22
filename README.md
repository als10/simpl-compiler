# SIMPL to APRIMPL compiler

This project contains a Racket program that compiles a LISP-like language called SIMPL into a toy assembly-like language called A-PRIMPL. There is also an assembler that converts A-PRIMPL into PRIMPL. PRIMPL instructions can be run using the PRIMPL interpreter.

SIMPL is a simple imperative, language that allows for function and variable definitions, function calling, arithmetic and logical operations, conditional evaluation via `iif` statements, and repitition via `while` loops. There are some examples of SIMPL programs in the `examples` directory.

The grammar for SIMPL is as follow:
```
program  = function ...
 	 	 	 	 
function = (fun (id id ...) (vars [(id int) ...] stmt ...))
 	 	 	 	 
aexp.    = (id aexp ...)
         | (+ aexp aexp)
         | (* aexp aexp)
         | (- aexp aexp)
         | (div aexp aexp)
         | (mod aexp aexp)
         | number
         | id
 	 	 	 	 
bexp     = (= aexp aexp)
         | (> aexp aexp)
         | (< aexp aexp)
         | (>= aexp aexp)
         | (<= aexp aexp)
         | (not bexp)
         | (and bexp ...)
         | (or bexp ...)
         | true
         | false

stmt     = (return aexp)
         | (print aexp)
         | (print string)
         | (set id aexp)
         | (seq stmt ...)
         | (iif bexp stmt stmt)
         | (skip)
         | (while bexp stmt ...)

```

The grammar for A-PRIMPL is as follows:
```
program  = (stmt | value) ...
 	 	 	 	 
  stmt   = (halt)
         | (lit psymbol-or-value)
         | (const psymbol psymbol-or-value)
         | (data psymbol psymbol-or-value ...)
         | (data psymbol (nat psymbol-or-value))
         | (label psymbol)
         | (add dest opd opd)
         | (sub dest opd opd)
         | (mul dest opd opd)
         | (div dest opd opd)
         | (mod dest opd opd)
         | (gt dest opd opd)
         | (ge dest opd opd)
         | (lt dest opd opd)
         | (le dest opd opd)
         | (equal dest opd opd)
         | (not-equal dest opd opd)
         | (land dest opd opd)
         | (lor dest opd opd)
         | (lnot dest opd)
         | (jump opd)
         | (branch opd opd)
         | (move dest opd)
         | (print-val opd)
         | (print-string string)
 	 	 	 	 
  opd    = imm
         | ind
         | (imm ind)
 	 	 	 	 
  dest   = ind
         | (imm ind)
 	 	 	 	 
  imm    = integer
         | boolean
 	 	 	 	 
  ind.   = (nat)
```

## Setup

First, ensure that you have [Racket](https://download.racket-lang.org/racket-v8.1.html) installed (v8.1 was used to write these programs).

Next, provide the execute permission to the `compile` and `run` scripts by running the following:
```
chmod u+x run compile
```

## Usage

Make sure you have followed the instructions in the setup section first.

To compile a SIMPL program, run
```
./compile path/to/file.simpl
```

This will generate a `.aprimp` file in the same directory with the same file name.

To run a `APRIMPL` file, run
```
./run path/to/file.aprimp
```

And that's it!

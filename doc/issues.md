# Misc issues

### Export format for Lambdas 

The *Type Checking in Lean 4* book, in the export format grammar for `#EL` says: 
``` 
  | eidx "#EL"  Info nidx eidx
```
However, the `export_format.md` file says: 
``` 
<eidx'> #EL <info> <nidx> <eidx_1> <eidx_2>
```

### Export format for Defs

This spec for definitions: 
```
Def ::= "#DEF" (name : nidx) (type : eidx) (value : eidx) (hint : Hint) (uparams : uidx*)
```
makes it seem like hints are mandatory, however it should(?) be:
```
Def ::= "#DEF" (name : nidx) (type : eidx) (value : eidx) (hint : Hint+) (uparams : uidx*)
```
In other words, I don't see explicit hints in every def, so perhaps this is optional.

### Strings in name

Can strings in hirarchical names (`#NS`) be arbitrary unicode? 

### Wrong specification for `EZ`:

The spec says EZ exprs are exported as follows: 

``` 
  | eidx "#EZ"  Info nidx eidx eidx eidx
```

However, it seems like it should just be
``` 
  | eidx "#EZ"  nidx eidx eidx eidx
```

### Missing spec for Opaque definitions

The format does not have a line for opaque definitions. It should be identical 
to the defs but without any `hint`s. 

``` 
Opaq ::= "#OPAQ" (name : nidx) (type : eidx) (value : eidx) (uparams : uidx*)
```

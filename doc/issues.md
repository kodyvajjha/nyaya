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
In other words, I don't see explicit hints in every def. 
TESTS
-----

 1.t tests _check_attribs(), tests validity of LHS of {class,key,attrib} => ...
     lines
 2.t more tests of _check_attribs(), tests validity of column numbers.
 3.t make Moose objects with no sub-objects, as array and hash
3a.t As 3.t but with re-defined 'class' and 'key' keywords. Tested reset of
     keywords
 4.t make a 2-level Moose object with an Arrayref sub-obj
4a.t make a 2-level Moose object with an Hashref sub-obj
4b.t make a 2-level Moose object with an object sub-obj
4c.t make a 2-level Moose object with all three object sub-objs: ArrayRef,
     Hashref, Obj ref
 5.t make a 3-level Moose object with all sub-objs as ArrayRefs
 6.t tests validity of params to ATM: data not AoA, empty desc
 7.t more tests of error checking in ATM, incl redefining 'class' & 'key' at
     and desc attribs given as refs (the latter tests probably already having
     been done in 1.t
 8.t Test mismatches in sub-objects between the declaration of the object in
     Moose, vs. that in "desc
8a.t test warn_multiple_rows()
8b.t test warn_nonunique_keys()

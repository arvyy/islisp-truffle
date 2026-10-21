(setf (documentation 'class '<truffle-object>)
    '((kind . class)
      (superclasses . (<object>))
      (slots . ())
      (desc . "Class for interop truffle objects with fields.")))
      
(setf (documentation 'class '<truffle-vector>)
    '((kind . class)
      (superclasses . (<basic-vector>))
      (slots . ())
      (desc . "Class for interop truffle object which are vector-like.")))
      
(setf (documentation 'class '<truffle-native-library>)
    '((kind . class)
      (superclasses . (<object>))
      (slots . ())
      (desc . "Class for interop truffle objects representing a library loaded through FFI facilities.")))

(setf (documentation 'function 'truffle-object-fields)
    '((kind . function)
      (syntax . "(truffle-object-fields truffle-obj)")
      (return-type . "<vector>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns a vector of strings, listing fields exposed by given truffle obj.
An error shall be signaled if  _truffle-obj_ is not an instance of *<truffle-object>* (error-id. _domain-error_).
Interop errors are signalled as an instance of *<truffle-interop-error>*.")))
      
(setf (documentation 'function 'truffle-object-field)
    '((kind . function)
      (syntax . "(truffle-object-field truffle-obj field-name)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns a value of a field named by _field-name_ in given _truffle-obj_.
An error shall be signaled if  _truffle-obj_ is not an instance of *<truffle-object>*, or if _field-name_ is not a string (error-id. _domain-error_).
Interop errors are signalled as an instance of *<truffle-interop-error>*.")))
      
(setf (documentation 'function 'set-truffle-object-field)
    '((kind . function)
      (syntax . "(set-truffle-object-field value truffle-obj field-name)")
      (return-type . "<object>")
      (min-args . 3)
      (max-args . 3)
      (desc . "Sets value of _field-name_ field in _truffle-obj_ to _value_. Returns _value_.
An error shall be signaled if  _truffle-obj_ is not an instance of *<truffle-object>*, or if _field-name_ is not a string (error-id. _domain-error_).
Interop errors are signalled as an instance of *<truffle-interop-error>*.")))
      
(setf (documentation 'function 'load-native-library)
    '((kind . function)
      (syntax . "(load-native-library name)")
      (return-type . "<truffle-native-library>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Loads a native library with given _name_.
An error shall be signaled if  _name_ is not a string (error-id. _domain-error_).
Interop errors are signalled as an instance of *<truffle-interop-error>*.")))
      
(setf (documentation 'function 'native-library-symbol)
    '((kind . function)
      (syntax . "(native-library-symbol native-library symbol signature")
      (return-type . "<object>")
      (min-args . 3)
      (max-args . 3)
      (desc . "Retrieves binding _symbol_ in given _native_library_. _signature_ specifies the shape of the binding such that it can be transposed into truffle runtime. See truffle's NFI documentation for details.
An error shall be signaled if  _native-library_ is not an instance of *<truffle-native-library>* or if either _signature_ or _symbol_ is not a string (error-id. _domain-error_).
Interop errors are signalled as an instance of *<truffle-interop-error>*.")))

(setf (documentation 'function 'truffle-interop-error-message)
    '((kind . function)
      (syntax . "(truffle-interop-error-message error")
      (return-type . "<string>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Retrieves message attached to _error_ raised during truffle interop operations.
An error shall be signaled if  _error_ is not an instance of *<truffle-interop-error>*.")))

(setf (documentation 'setf 'truffle-object-field)
    '((kind . setf)
      (syntax . "(setf (truffle-object-field truffle-obj field-name) value)")
      (return-type . "<object>")
      (desc . "Sets value of _field-name_ field in _truffle-obj_ to _value_. Returns _value_.
An error shall be signaled if  _truffle-obj_ is not an instance of *<truffle-object>*, or if _field-name_ is not a string (error-id. _domain-error_).
Interop errors are signalled as an instance of *<truffle-interop-error>*.")))

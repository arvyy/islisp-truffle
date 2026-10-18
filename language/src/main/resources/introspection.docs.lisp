(setf (documentation 'class '<module>)
    '((kind . class)
      (superclasses . (<object>))
      (slots . ())
      (desc . "Class representing an ISLISP module.")))

(setf (documentation 'function 'lookup-module)
    '((kind . function)
      (syntax . "(lookup-module path)")
      (return-type . "<module>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns reference to a module defined at _path_ as an instance of *<module>*, or *nil* if it doesn't exist. Uses same rules as provide form.
An error shall be signaled if  _path_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'lookup-module-documentations)
    '((kind . function)
      (syntax . "(list-module-documentations module)")
      (return-type . "<vector>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns all documentation entries registered in _module_. Each entry is itself a vector length of 3, containing namespace, name, and documentation alist. See *documentation* method for details on its structure.
An error shall be signaled if  _module_ is not an instance of *<module>* (error-id. _domain-error_).")))

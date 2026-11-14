(library (lang properties)
  (export
    make-annotation-property
    annotation-property?
    annotation-property-set!
    annotation-property-ref)
  (import (chezscheme))

  (define-record-type annotation-property
    (nongenerative)
    (sealed #t)
    (fields name table)
    (protocol
      (lambda (new)
        (lambda (name)
          (unless (symbol? name)
            (assertion-violationf 'make-annotation-property "~s is not a symbol" name))
          (new name (make-ephemeron-eq-hashtable))))))

  ;; Reader handlers in different threads may set properties at once, and
  ;; two threads must not change one hashtable.
  (define table-mutex (make-mutex))

  (define (check-arguments who property annotation)
    (unless (annotation-property? property)
      (assertion-violationf who "~s is not an annotation property" property))
    (unless (annotation? annotation)
      (assertion-violationf who "~s is not an annotation" annotation)))

  (define (annotation-property-set! property annotation value)
    (check-arguments 'annotation-property-set! property annotation)
    (with-mutex table-mutex
      (eq-hashtable-set! (annotation-property-table property) annotation value)))

  (define (annotation-property-ref property annotation default)
    (check-arguments 'annotation-property-ref property annotation)
    (with-mutex table-mutex
      (eq-hashtable-ref (annotation-property-table property) annotation default))))

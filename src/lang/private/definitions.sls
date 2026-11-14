(library (lang private definitions)
  (export
    definer
    definition-form?
    form-defined-names
    unbound-identifier?)
  (import (chezscheme) (lang private probe))

  (define definer)

  (define (unbound-identifier? id)
    (unless (identifier? id)
      (assertion-violationf 'unbound-identifier? "~s is not an identifier" id))
    (free-identifier=? id (datum->syntax chez-lang-probe:context (syntax->datum id))))

  (define (invalid-definition form)
    (syntax-violation #f "invalid syntax" form))

  ;; Chez's define-record-type makes its default names in the context of the
  ;; record name; an export must name the same identifiers.
  (define (construct-name template . parts)
    (datum->syntax template
      (string->symbol
        (apply string-append
          (map (lambda (part) (if (string? part) part (symbol->string (syntax->datum part))))
               parts)))))

  (define (define-names form lookup)
    (syntax-case form ()
      [(_ name) (identifier? #'name) (list #'name)]
      [(_ name expr) (identifier? #'name) (list #'name)]
      [(_ (name . formals) body1 body2 ...) (identifier? #'name) (list #'name)]
      [_ (invalid-definition form)]))

  (define (define-syntax-names form lookup)
    (syntax-case form ()
      [(_ name expr) (identifier? #'name) (list #'name)]
      [(_ (name id) body1 body2 ...) (and (identifier? #'name) (identifier? #'id)) (list #'name)]
      [_ (invalid-definition form)]))

  (define (alias-names form lookup)
    (syntax-case form ()
      [(_ new old) (and (identifier? #'new) (identifier? #'old)) (list #'new)]
      [_ (invalid-definition form)]))

  (define (define-values-names form lookup)
    (syntax-case form ()
      [(_ formals expr)
       (let loop ([formals #'formals])
         (syntax-case formals ()
           [() '()]
           [id (identifier? #'id) (list #'id)]
           [(id . rest) (identifier? #'id) (cons #'id (loop #'rest))]
           [_ (invalid-definition form)]))]
      [_ (invalid-definition form)]))

  (define (record-names form name constructor predicate clauses)
    (define (field-names spec)
      (syntax-case spec (immutable mutable)
        [(immutable field accessor)
         (and (identifier? #'field) (identifier? #'accessor))
         (list #'accessor)]
        [(mutable field accessor mutator)
         (and (identifier? #'field) (identifier? #'accessor) (identifier? #'mutator))
         (list #'accessor #'mutator)]
        [(immutable field)
         (identifier? #'field)
         (list (construct-name name name "-" #'field))]
        [(mutable field)
         (identifier? #'field)
         (list (construct-name name name "-" #'field)
               (construct-name name name "-" #'field "-set!"))]
        [field
         (identifier? #'field)
         (list (construct-name name name "-" #'field))]
        [_ (invalid-definition form)]))
    (define (clause-names clause)
      (syntax-case clause (fields)
        [(fields spec ...) (apply append (map field-names #'(spec ...)))]
        [_ '()]))
    (cons* name constructor predicate (apply append (map clause-names clauses))))

  (define (define-record-type-names form lookup)
    (syntax-case form ()
      [(_ name clause ...)
       (identifier? #'name)
       (record-names form #'name
         (construct-name #'name "make-" #'name)
         (construct-name #'name #'name "?")
         #'(clause ...))]
      [(_ (name constructor predicate) clause ...)
       (and (identifier? #'name) (identifier? #'constructor) (identifier? #'predicate))
       (record-names form #'name #'constructor #'predicate #'(clause ...))]
      [_ (invalid-definition form)]))

  (define (define-enumeration-names form lookup)
    (syntax-case form ()
      [(_ type (symbol ...) constructor)
       (and (identifier? #'type) (for-all identifier? #'(symbol ...)) (identifier? #'constructor))
       (list #'type #'constructor)]
      [_ (invalid-definition form)]))

  (define (define-condition-type-names form lookup)
    (syntax-case form ()
      [(_ type supertype constructor predicate (field accessor) ...)
       (for-all identifier? #'(type supertype constructor predicate field ... accessor ...))
       #'(type constructor predicate accessor ...)]
      [_ (invalid-definition form)]))

  (define (define-ftype-names form lookup)
    (syntax-case form ()
      [(_ name ftype . options) (identifier? #'name) (list #'name)]
      [(_ (name ftype . options) ...) (for-all identifier? #'(name ...)) #'(name ...)]
      [_ (invalid-definition form)]))

  ;; Chez's define-structure makes each accessor and mutator in the context
  ;; of its field, not of the structure name.
  (define (define-structure-names form lookup)
    (define (structure-names name fields)
      (cons* (construct-name name "make-" name)
             (construct-name name name "?")
             (apply append
               (map (lambda (field)
                      (list (construct-name field name "-" field)
                            (construct-name field "set-" name "-" field "!")))
                    fields))))
    (syntax-case form ()
      [(_ (name field ...))
       (for-all identifier? #'(name field ...))
       (structure-names #'name #'(field ...))]
      [(_ (name field1 ...) ((field2 init) ...))
       (for-all identifier? #'(name field1 ... field2 ...))
       (structure-names #'name #'(field1 ... field2 ...))]
      [_ (invalid-definition form)]))

  (define (module-names form lookup)
    (define (entry-name entry)
      (syntax-case entry ()
        [id (identifier? #'id) #'id]
        [(id indirect ...) (identifier? #'id) #'id]
        [_ (invalid-definition form)]))
    (syntax-case form ()
      [(_ name (entry ...) body ...) (identifier? #'name) (list #'name)]
      [(_ (entry ...) body ...) (map entry-name #'(entry ...))]
      [_ (invalid-definition form)]))

  (define (meta-names form lookup)
    (define (names-of definition)
      (if (definition-form? definition lookup)
          (form-defined-names definition lookup)
          (invalid-definition form)))
    (syntax-case form ()
      [(_ b definition ...)
       (and (identifier? #'b) (free-identifier=? #'b #'begin))
       (apply append (map names-of #'(definition ...)))]
      [(_ . definition) (names-of #'definition)]))

  (define (no-names form lookup) '())

  (define definition-keywords
    (list
      (cons #'define define-names)
      (cons #'define-syntax define-syntax-names)
      (cons #'alias alias-names)
      (cons #'define-values define-values-names)
      (cons #'define-record-type define-record-type-names)
      (cons #'define-enumeration define-enumeration-names)
      (cons #'define-condition-type define-condition-type-names)
      (cons #'define-ftype define-ftype-names)
      (cons #'define-structure define-structure-names)
      (cons #'trace-define define-names)
      (cons #'trace-define-syntax define-syntax-names)
      (cons #'module module-names)
      (cons #'meta meta-names)
      (cons #'define-property no-names)
      (cons #'import no-names)
      (cons #'import-only no-names)
      (cons #'indirect-export no-names)
      (cons #'implicit-exports no-names)
      (cons #'export no-names)
      (cons #'include no-names)))

  ;; A property lookup raises on an unbound identifier.
  (define (form-head form)
    (syntax-case form ()
      [(head . rest) (and (identifier? #'head) (not (unbound-identifier? #'head))) #'head]
      [_ #f]))

  (define (keyword-names head)
    (cond
      [(assp (lambda (keyword) (free-identifier=? head keyword)) definition-keywords) => cdr]
      [else #f]))

  (define (check-lookup who lookup)
    (unless (procedure? lookup)
      (assertion-violationf who "~s is not a property lookup procedure" lookup)))

  (define (definition-form? form lookup)
    (check-lookup 'definition-form? lookup)
    (let ([head (form-head form)])
      (and head
           (or (lookup head #'definer) (keyword-names head))
           #t)))

  (define (form-defined-names form lookup)
    (define who 'form-defined-names)
    (check-lookup who lookup)
    (let ([head (form-head form)])
      (cond
        [(and head (lookup head #'definer))
         => (lambda (names-of)
              (unless (procedure? names-of)
                (assertion-violationf who "the definer property of ~s is ~s, not a procedure"
                  (syntax->datum head) names-of))
              (let ([names (names-of form)])
                (unless (and (list? names) (for-all identifier? names))
                  (assertion-violationf who "the definer of ~s returned ~s, not a list of identifiers"
                    (syntax->datum head) names))
                names))]
        [(and head (keyword-names head)) => (lambda (names-of) (names-of form lookup))]
        [else (syntax-violation who "not a definition" form)]))))

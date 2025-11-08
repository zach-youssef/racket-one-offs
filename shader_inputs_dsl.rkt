#lang racket

(struct field (type name) #; transparent)
(define-syntax-rule (_field type name)
  (field (syntax->datum #'type) (syntax->datum #'name)))

(struct buffer (name fields) #;transparent)
(struct layout (type buffers) #;transparent)

(define-syntax field-parse-list
  (syntax-rules()
    [(field-parse-list) '()]
    [(field-parse-list (type name)) (list (_field type name))]
    [(field-parse-list (type name) more ...) (cons (_field type name)
                                                   (field-parse-list more ...))]))

(define-syntax buffer-parse
  (syntax-rules ()
    [(buffer-parse (name fields ...)) (buffer (syntax->datum #'name) (field-parse-list fields ...))]))

(define-syntax buffer-parse-list
  (syntax-rules ()
    [(buffer-parse-list) '()]
    [(buffer-parse-list buffer) (list (buffer-parse buffer))]
    [(buffer-parse-list buffer more ...) (cons (buffer-parse buffer)
                                               (buffer-parse-list more ...))]))

(define-syntax data-layout-parse
  (syntax-rules ()
    [(data-layout-parse (binding-type definitions ...)) (layout (syntax->datum #'binding-type) (buffer-parse-list definitions ...))]))

(define-syntax data-layout-parse-list
  (syntax-rules ()
    [(data-layout-parse-list) '()]
    [(data-layout-parse-list layout-syntax) (list (data-layout-parse layout-syntax))]
    [(data-layout-parse-list layout-syntax more ...) (cons (data-layout-parse layout-syntax)
                                                           (data-layout-parse-list more ...))]))

(define-syntax shader-data
  (syntax-rules ()
    [(shader-data header-path shader-path data-layouts ...)
     (let ([layouts (data-layout-parse-list data-layouts ...)])
       (cpp-string layouts))]))

(define (cpp-string layouts)
  (define (cpp-field field)
    ; TODO: map types to glm equivalent
    (string-append "\t" (symbol->string (field-type field)) " " (symbol->string (field-name field)) ";"))
  (define (cpp-ubo ubo)
    ; TODO: Add alginment macros
    (string-append "struct " (string-titlecase (symbol->string (buffer-name ubo))) "{\n"
                   (string-join (map cpp-field (buffer-fields ubo)) "\n")
                   "};\n"))
  (define (cpp-vert vert) "") ; TODO: Implement
  (define (cpp-layout l)
    (match l
      [(layout 'uniform-buffers ubos) (string-join (map cpp-ubo ubos) "\n")]
      [(layout 'vertex-streams vert-streams) (string-join (map cpp-vert vert-streams) "\n")]
      [(layout unknown _) (error "Unknown layout type" unknown)]))
  (string-append "// This code was generated and should not be modified by hand.\n#pragma once\n"
                 ; TODO: Add glm imports
                 (string-join (map cpp-layout layouts) "\n")))

(shader-data "path/to/c++/out" "path/to/glsl/out"
             (uniform-buffers (ubo0 [mat4 mvp]
                                    [float foo])
                              (ubo1 [vec3 bar]
                                    [vec4 baz]))
             (vertex-streams (vertex [vec3 position]
                                     [vec3 normal]
                                     [vec3 color])))

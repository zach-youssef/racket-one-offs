#lang racket

(struct field (type name) #;transparent)
(struct buffer (name fields) #;transparent)
(struct layout (type buffers) #;transparent)

(define-syntax field-parse
  (syntax-rules ()
    [(field-parse (type name)) (field (syntax->datum #'type) (syntax->datum #'name))]))

(define-syntax buffer-parse
  (syntax-rules ()
    [(buffer-parse (name fields ...)) (buffer (syntax->datum #'name) (field-parse-list fields ...))]))

(define-syntax data-layout-parse
  (syntax-rules ()
    [(data-layout-parse (binding-type definitions ...)) (layout (syntax->datum #'binding-type) (buffer-parse-list definitions ...))]))

(define-syntax define-list-parser
  (syntax-rules ()
    [(define-list-parser id element-parser)
     (define-syntax id
       (syntax-rules ()
         [(id) '()]
         [(id element) (list (element-parser element))]
         [(id element more (... ...)) (cons (element-parser element)
                                            (id more (... ...)))]))]))

(define-list-parser data-layout-parse-list data-layout-parse)
(define-list-parser buffer-parse-list buffer-parse)
(define-list-parser field-parse-list field-parse)

(struct ifield (index type name) #;transparent)
(struct ibuffer (index name fields) #;transparent)

(define (label-layout-indices layouts)
  (for/list ([l layouts])
    (layout (layout-type l) (for/list ([b (layout-buffers l)]
                                       [i (in-naturals)])
                              (ibuffer i (buffer-name b) (for/list ([f (buffer-fields b)]
                                                                    [j (in-naturals)])
                                                           (ifield j (field-type f) (field-name f))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax shader-data
  (syntax-rules ()
    [(shader-data header-path shader-path data-layouts ...)
     (let ([layouts (label-layout-indices (data-layout-parse-list data-layouts ...))]
           [cpp-out (open-output-file header-path)]
           [glsl-out (open-output-file shader-path)])
       (begin
         (display (glsl-string layouts) glsl-out)
         (close-output-port glsl-out)
         (display (cpp-string layouts) cpp-out)
         (close-output-port cpp-out)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-layout header-string uniform-fn vertex-fn)
  (λ [layouts] (string-append header-string
                              (string-join (map (λ [l] (match l
                                                         [(layout 'uniform-buffers ubos) (string-join (map uniform-fn ubos) "\n")]
                                                         [(layout 'vertex-streams vert-streams) (string-join (map vertex-fn vert-streams) "\n")]
                                                         [(layout unknown _) (error "Unknown layout type" unknown)]))
                                                layouts) "\n")
                              "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (buffer-struct-name buffer)
  (string-titlecase (symbol->string (ibuffer-name buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cpp-string (let* ([glm-prefix (λ [type] (if (ormap (λ [t] (symbol=? t type)) '(vec2 vec3 vec4 mat3 mat4)) "glm::" ""))]
                          [cpp-field (λ [field] (string-append "\t" (glm-prefix (ifield-type field)) (symbol->string (ifield-type field)) " " (symbol->string (ifield-name field)) ";"))]
                          [cpp-struct (λ [buffer extras](string-append "struct " (buffer-struct-name buffer) " {\n"
                                                                       (string-join (map cpp-field (ibuffer-fields buffer)) "\n")
                                                                       extras
                                                                       "\n};\n"))]
                          [map-type-to-format (λ [type](match type
                                                         ['float "VK_FORMAT_R32_SFLOAT"]
                                                         ['uint "VK_FORMAT_R32_UINT"]
                                                         ['int "VK_FORMAT_R32_SINT"]
                                                         ['vec2 "VK_FORMAT_R32G32_SFLOAT"]
                                                         ['vec3 "VK_FORMAT_R32G32B32_SFLOAT"]
                                                         ['vec4 "VK_FORMAT_R32G32B32A32_SFLOAT"]
                                                         [_ (error "Unsupported vertex stream type" type)]))])
                     (convert-layout (string-append "// This code was generated and should not be modified by hand.\n"
                                                    "#pragma once\n\n"
                                                    "#include <vulkan/vulkan.h>\n\n"
                                                    "#define GLM_FORCE_DEFAULT_ALIGNED_GENTYPES\n"
                                                    "#include <glm/glm.hpp>\n\n")
                                     (λ [ubo] (cpp-struct ubo ""))
                                     (λ [vert] (cpp-struct vert
                                                           (let ([binding (number->string (ibuffer-index vert))]
                                                                 [struct-name (buffer-struct-name vert)])
                                                             (string-append
                                                              "\n\n\tstatic VkVertexInputBindingDescription getBindingDescription() {\n"
                                                              "\t\tVkVertexInputBindingDescription bindingDescription{};\n"
                                                              "\t\tbindingDescription.binding = " binding ";\n"
                                                              "\t\tbindingDescription.stride = sizeof(" struct-name ");\n"
                                                              "\t\t// Move to next data entry after each vertex (alternative is after each instance)\n"
                                                              "\t\tbindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;\n"
                                                              "\t\treturn bindingDescription;\n"
                                                              "\t}\n\n"
                                                              (let ([count (number->string (length (ibuffer-fields vert)))])
                                                                (string-append
                                                                 "\tstatic std::array<VkVertexInputAttributeDescription, " count "> getAttributeDescriptions() {\n"
                                                                 "\t\tstd::array<VkVertexInputAttributeDescription, " count "> attributeDescriptions{};\n"))
                                                              (string-join (for/list ([f (ibuffer-fields vert)])
                                                                             (let ([location (number->string (ifield-index f))])
                                                                               (string-append "\t\tattributeDescriptions["location"].binding = " binding ";\n"
                                                                                              "\t\tattributeDescriptions["location"].location = " location ";\n"
                                                                                              "\t\tattributeDescriptions["location"].format = " (map-type-to-format (ifield-type f)) ";\n"
                                                                                              "\t\tattributeDescriptions["location"].offset = offsetof("struct-name", "(symbol->string (ifield-name f))");\n")))
                                                                           "\n\n")
                                                              "\t\treturn attributeDescriptions;\n"
                                                              "\t}")))))))
                                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define glsl-string (convert-layout
                     (string-append "// This code was generated and should not be modified by hand.\n"
                                    "#version 450\n\n")
                     (λ [ubo] (string-append "layout(binding = "(number->string (ibuffer-index ubo))") uniform "(buffer-struct-name ubo)" {\n"
                                             (string-join (for/list ([f (ibuffer-fields ubo)])
                                                            (string-append "\t" (symbol->string (ifield-type f)) " " (symbol->string (ifield-name f)) ";"))
                                                          "\n")
                                             "\n} " (symbol->string (ibuffer-name ubo)) ";\n"))
                     (λ [vert] (string-join (for/list ([f (ibuffer-fields vert)])
                                              (string-append "layout(location = " (number->string (ifield-index f)) ") in " (symbol->string (ifield-type f)) " in" (string-titlecase (symbol->string (ifield-name f))) ";"))
                                            "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(shader-data "/tmp/ShaderLayout.h" "/tmp/ShaderLayout.glsl"
             (uniform-buffers (ubo0 [mat4 mvp]
                                    [float foo])
                              (ubo1 [vec3 bar]
                                    [vec4 baz]))
             (vertex-streams (vertex [vec3 position]
                                     [vec3 normal]
                                     [vec3 color])))

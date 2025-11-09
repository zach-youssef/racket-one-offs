#lang racket

(require "shader_inputs_dsl.rkt")

(shader-data "/tmp/ShaderLayout.h" "/tmp/ShaderLayout.glsl"
             (uniform-buffers (ubo0 [mat4 mvp]
                                    [float foo])
                              (ubo1 [vec3 bar]
                                    [vec4 baz]))
             (vertex-streams (vertex [vec3 position]
                                     [vec3 normal]
                                     [vec3 color])))
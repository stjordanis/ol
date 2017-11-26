#!/usr/bin/ol
(import (lib opengl))

(define Context (gl:Create "7. OpenGL 2.0"))

(import (OpenGL version-2-0))

(define width 1280)
(define height 920)

(gl:run

   Context

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

(let ((po (glCreateProgram))
      (vs (glCreateShader GL_VERTEX_SHADER))
      (fs (glCreateShader GL_FRAGMENT_SHADER)))
   (if (= po 0)
      (runtime-error "Can't create shader program." '()))

   ; пример, как можно передать в функцию массив указателей на строки:
   ; vertex shader:
   (glShaderSource vs 2 (list (c-string "#version 120 // OpenGL 2.1\n")
                              (c-string "
      void main() {
         gl_Position = gl_Vertex;
      }")) #f)
   (glCompileShader vs)
   (let ((isCompiled (vm:new-raw-object type-vector-raw '(0))))
      (glGetShaderiv vs GL_COMPILE_STATUS isCompiled)

      (if (= (ref isCompiled 0) 0)
         (let*((maxLength "??")
               (_ (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog vs))))
   (glAttachShader po vs)

   ; fragment shader:
   (if (eq? (length *vm-args*) 2)
      (glShaderSource fs 1 (list (c-string (runes->string (file->list (cadr *vm-args*))))) #f)

      (glShaderSource fs 2 (list (c-string "#version 120 // OpenGL 2.1")
                                 (c-string "
         // http://glslsandbox.com/e#19102.0
         uniform float time;

         #define iterations 14
         #define formuparam 0.530

      #define volsteps 18
      #define stepsize 0.2

      #define zoom   0.800
      #define tile   0.850
      #define speed  0.0001

      #define brightness 0.0015
      #define darkmatter 0.400
      #define distfading 0.760
      #define saturation 0.800

      void main(void) {
         vec2 viewport = vec2(1280, 720);

         //get coords and direction
         vec2 uv=gl_FragCoord.xy / viewport.xy - .5;
         uv.y*=viewport.y/viewport.x;
         vec3 dir=vec3(uv*zoom,1.);

         float a2=speed+.5;
         float a1=0.0;
         mat2 rot1=mat2(cos(a1),sin(a1),-sin(a1),cos(a1));
         mat2 rot2=rot1;//mat2(cos(a2),sin(a2),-sin(a2),cos(a2));
         dir.xz*=rot1;
         dir.xy*=rot2;

         vec3 from=vec3(-0.05, 0.05, 0);
         //from.x-=time; <- movement

         from.z = time / 20000.0;

         from.x-=0.2;//mouse.x;
         from.y-=0.7;//mouse.y;

         from.xz*=rot1;
         from.xy*=rot2;

         //volumetric rendering
         float s=.4,fade=.2;
         vec3 v=vec3(0.4);
         for (int r=0; r<volsteps; r++) {
         vec3 p=from+s*dir*.5;
         p = abs(vec3(tile)-mod(p,vec3(tile*2.))); // tiling fold
         float pa,a=pa=0.;
         for (int i=0; i<iterations; i++) {
            p=abs(p)/dot(p,p)-formuparam; // the magic formula
            a+=abs(length(p)-pa); // absolute sum of average change
            pa=length(p);
         }
         float dm=max(0.,darkmatter-a*a*.001); //dark matter
         a*=a*a*2.; // add contrast
         if (r>3) fade*=1.-dm; // dark matter, don't render near
         //v+=vec3(dm,dm*.5,0.);
         v+=fade;
         v+=vec3(s,s*s,s*s*s*s)*a*brightness*fade; // coloring based on distance
         fade*=distfading; // distance fading
         s+=stepsize;
         }
         v=mix(vec3(length(v)),v,saturation); //color adjust
         gl_FragColor = vec4(v*.01,1.);

      }")) #f))
   (glCompileShader fs)
   (let ((isCompiled (vm:new-raw-object type-vector-raw '(0))))
      (glGetShaderiv fs GL_COMPILE_STATUS isCompiled)

      (if (= (ref isCompiled 0) 0)
         (let*((maxLength "??")
               (_ (glGetShaderiv fs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog fs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog fs))))

   (glAttachShader po fs)

   (glLinkProgram po)
   (glDetachShader po fs)
   (glDetachShader po vs)


   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)

   (let* ((ss ms (clock)))
   (list po ss ms))))

; draw
(lambda (po ss ms)
   (let ((time (glGetUniformLocation po (c-string "time")))
         (resolution (glGetUniformLocation po (c-string "resolution"))))

   (glClear GL_COLOR_BUFFER_BIT)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)

   (glUseProgram po)
   (let* ((s2 m2 (clock)))
      (glUniform1f time (+ (/ (- m2 ms) 1000) (- s2 ss)))) ; раз в час будем сбрасывать период
   (if (> resolution 0)
      (glUniform2f resolution width height))

   (glBegin GL_TRIANGLE_STRIP)
      (glVertex2f -1 -1)
      (glVertex2f +1 -1)
      (glVertex2f -1 +1)
      (glVertex2f +1 +1)
   (glEnd))

(list po ss ms)))